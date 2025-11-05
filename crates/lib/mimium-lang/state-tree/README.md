# state-tree

状態ツリーユーティリティライブラリ。異なる2つのコンパイルされたソースコード間でDSP（デジタルシグナルプロセッシング）の内部状態を保持・管理するための機能を提供します。

## 概要

mimiumでは、ホットリロード機能を実現するために、コンパイルしたコードの状態（遅延線バッファ、メモリ値、フィードバック値など）を、新しくコンパイルされたコードに引き継ぐ必要があります。このクレートは、古い状態ツリーから新しい状態ツリーへ効率的にデータを移行するための仕組みを提供します。

## 主要な機能

### 1. 状態ツリーの表現 (`tree` module)

**StateTree** - DSPの内部状態を階層的に表現するデータ構造

- **Delay**: 遅延線を表現（読み込みインデックス、書き込みインデックス、バッファデータ）
- **Mem**: メモリノード（値のリスト）
- **Feed**: フィードバック値（制御信号など）
- **FnCall**: 複数の子ノードを持つ関数呼び出しノード

**StateTreeSkeleton** - 状態ツリーの「構造」を型安全に表現

- 実際の値ではなく、各ノードのサイズ情報のみを保持
- 新しいコード構造がどうなるかの定義に使用
- ジェネリック型 `T: SizedType` で各ノードのサイズ計算方法を指定

### 2. 差分検出 (`tree_diff` module)

**take_diff** - 古いツリーと新しいツリーの差分を検出

差分検出アルゴリズム:
- **Longest Common Subsequence (LCS)** アルゴリズムを使用してノード間の対応関係を検出
- ノードが「同じ種類」かつ「同じサイズ」であることを確認して対応付け
- 挿入・削除・共有ノードを識別

検出結果は `CopyFromPatch` のリストとして返される

### 3. パッチ適用 (`patch` module)

**CopyFromPatch** - 古いツリーから新しいツリーへのデータコピー指示

- `old_path`: 古いツリー内のノード位置を示すパス（インデックス配列）
- `new_path`: 新しいツリー内のノード位置を示すパス（インデックス配列）

**apply_patches** - パッチを適用して新しいツリーに古いデータを転送

- 古いツリーから新しいツリーへ対応するノードのデータをコピー
- Delay の readidx/writeidx、Mem/Feed のデータを適切にコピー

### 4. 状態ストレージ更新 (`lib.rs`)

**update_state_storage** - 統合的な更新関数

```rust
pub fn update_state_storage<T: SizedType + PartialEq>(
    old: &[u64],                              // 古い状態の フラット化されたバイナリ表現
    old_state_skeleton: StateTreeSkeleton<T>, // 古い構造
    new_state_skeleton: StateTreeSkeleton<T>, // 新しい構造
) -> Result<Option<Vec<u64>>, Box<dyn std::error::Error>>
```

処理フロー:
1. 古いバイナリデータを古い構造でデシリアライズ
2. 新しい構造で空のツリーを作成
3. 差分を検出（`take_diff`）
4. パッチを適用（`apply_patches`）
5. 新しいツリーをバイナリにシリアライズして返す

構造に変更がない場合は `None` を返す（最適化）

## 使用例

### 基本的な使い方

```rust
use state_tree::{update_state_storage, tree::*};

// 古い状態をバイナリで保持
let old_state_bytes: Vec<u64> = vec![/* ... */];

// 古いコンパイル結果から得られた構造
let old_skeleton = StateTreeSkeleton::FnCall(vec![
    Box::new(StateTreeSkeleton::Delay { len: 2 }),
    Box::new(StateTreeSkeleton::Mem(SizeInfo { size: 100 })),
]);

// 新しくコンパイルされたコードの構造
let new_skeleton = StateTreeSkeleton::FnCall(vec![
    Box::new(StateTreeSkeleton::Mem(SizeInfo { size: 100 })),  // 順序が変更
    Box::new(StateTreeSkeleton::Delay { len: 2 }),
    Box::new(StateTreeSkeleton::Feed(SizeInfo { size: 1 })),   // 新規追加
]);

// 状態を新しい構造に適応させる
match update_state_storage(&old_state_bytes, &old_skeleton, &new_skeleton) {
    Ok(Some(new_state_bytes)) => {
        // 新しい構造に適応した状態を使用
    }
    Ok(None) => {
        // 構造が変わらない場合
    }
    Err(e) => {
        // エラー処理
    }
}
```

## 実装のポイント

### シリアライズ/デシリアライズ

- `serialize_tree_untagged`: StateTreeを `Vec<u64>` に平坦化
  - タグ情報を含まない（skeleton で構造情報を別管理）
  - 効率的なメモリ使用

- `deserialize_tree_untagged`: skeleton を使って `Vec<u64>` をStateTreeに復元

### ノード対応付けの戦略

LCS アルゴリズムを用いることで:
- ノードの並び替えに対応
- ノードの挿入・削除を自動検出
- 対応するノード間のデータ転送を最小化

### パス指定

各ノードへのアクセスはパス（インデックスの配列）で指定:
```
old_path: [2, 1, 0]  // [2]番目の子 → その[1]番目の子 → その[0]番目の子
```

## テスト

### ユニットテスト (`tests/diff.rs`)

- 単純な差分検出
- ノードの挿入・削除
- LCS アルゴリズムの正確性

### 統合テスト (`tests/end2end.rs`)

- ノードの並び替え
- 複雑な構造変更
- シリアライズ/デシリアライズと差分適用の統合動作

## 設計上の考慮

### パフォーマンス

- LCS アルゴリズムは $O(mn)$ の時間計算量（m, n はノード数）
- 実際のDSP構造は通常ノード数が少ないため実用的

### 型安全性

- `SizedType` トレイトで各ノードのサイズ計算を汎用化
- skeleton を型で表現することで、実行時の型不一致エラーを減らす

### エラーハンドリング

- デシリアライズ失敗時は `Result` で返す
- patch 適用時は無効なパスが指定されるとパニック（debug_assert での検証）

## 関連するmodule

- **crates/lib/mimium-lang**: メインのlang クレート。言語処理の一部で state-tree を使用
- **plugins/mimium-scheduler**: スケジューリング機能で状態管理が必要

## ライセンス

ワークスペースの共通ライセンスに従う
