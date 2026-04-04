# mimium

main: [![Test(main)](https://github.com/tomoyanonymous/mimium-rs/actions/workflows/ci.yaml/badge.svg?branch=main)](https://github.com/tomoyanonymous/mimium-rs/actions/workflows/ci.yaml) dev: [![Test(dev)](https://github.com/tomoyanonymous/mimium-rs/actions/workflows/ci.yaml/badge.svg?branch=dev)](https://github.com/tomoyanonymous/mimium-rs/actions/workflows/ci.yaml)

mimium:音と音楽のためのインフラストラクチャとして設計されたプログラミング言語

<p align="center" style="display:flex; justify-content:center;">
<img src = "mimium_logo_slant.svg" width="300" alt="mimium のアイコン。" />
</p>

https://mimium.org

---

mimium（*MInimal-Musical-medIUM*）は、音と音楽のためのプログラミング言語です。

mimiumは、ミュージシャンやプログラマ向けの単なるツールではなく、音楽をソースコードという形で流通させるための基盤となることを目指して作られています。

その文法は、*[Faust](https://faust.grame.fr)*、*[Kronos](https://kronoslang.io/)*、*[Extempore](https://extemporelang.github.io/)* といった現代的な音響向けプログラミング言語から着想を得ています。

例えば、以下の簡単なコードは440Hzのサイン波を生成します。

```rust
// minimal.mmm
let twopi = 3.141595*2.0
fn dsp(){
  sin(now * 440.0 * twopi / samplerate)
}
```

## Unique Features

### Functional Approach

関数内では特別なキーワード`self`を使用でき、これはその関数の直前の戻り値を表します。

これにより、信号処理のフィードバック接続を関数のパイプとして簡潔に記述できます。

```rust
fn lpf(input,fb){    
     (1.0-fb)*input + fb*self
}
```

また、言語設計は多段階計算を伴う値呼びラムダ計算に基づいており、高階関数によってオシレータの複製のような複雑なシグナルグラフを表現できます。

```rust
//10 Sequentially connected oscillators
#stage(macro)
fn cascade(n,gen){
    if (n>0.0){
        let multiplier = 1.0-(1.0/(n*3)) |> lift_f
        `|rate| rate + ($gen)(rate/3)* 0.5 * rate* $multiplier  
                |> cascade!(n - 1.0 ,gen) 
    }else{
        `|rate| ($gen)(rate)
    }
}
#stage(main)
let osc = ...
fn dsp(){
    let out =  50 |> cascade!(10,`osc) 
    (out,out)
}
```

mimium は静的型付け言語ですが、多くの型注釈は型推論システムによって省略できます。mimium の理論的背景に興味がある場合は、[mimiumに関する論文](https://matsuuratomoya.com/en/research/lambdammm-ifc-2024/)をご覧ください。

### Live Coding

mimiumはFaustのように非常に低レベルなデジタル信号処理アルゴリズムを記述できます。さらに、実行中のファイルを上書き保存するだけで音声の不連続を起こさずにソースコードを更新でき、ディレイやリバーブのテールも保持できるため、**フルスクラッチDSPライブコーディング**が可能です。

### Extensibility

mimiumのVM設計はLuaに着想を得ており、プラグインシステムを通じてRustアプリケーションへ容易に組み込めます。Rust側で定義した外部関数（クロージャ）をmimiumから簡単に呼び出せます。

## Installation

mimiumを始める最も簡単な方法は [Visual Studio Code 拡張機能](https://github.com/mimium-org/mimium-language)を使うことです。拡張機能をインストールすると、CLI ツール `mimium-cli` とLanguage Server（シンタックスハイライトおよびエラー表示が可能）が自動でダウンロードされます。

コマンドパレットから `.mmm` ファイルを実行できます。

また、GitHub Releases からシェルスクリプト経由で最新版の CLI ツール [mimium-cli](https://github.com/tomoyanonymous/mimium-rs/releases)をダウンロードすることもできます。

## Development

[Development](./Development) セクションを参照してください。

## Contributing

バグ修正、リファクタリング、ドキュメント整備、ユースケースの共有など、あらゆる種類の貢献を歓迎します。

（ただし、mimiumはまだまだ開発初期段階で、取り組むべきことが多くあるため、Pull Requestを伴わない新機能の提案や要望は受け付けられない場合があります。）

参加する前に[Code of Conduct](./CODE_OF_CONDUCT) をご確認ください。

## [License](LICENSE)

©️ mimium development community.

ソースコードは[Mozilla Public License 2.0 (MPL2.0)](LICENSE) のもとでライセンスされています。

## Original Author

Tomoya Matsuura/松浦知也 <https://matsuuratomoya.com/>

## Acknowledgements

本プロジェクトは、以下の助成金および奨学支援を受けています。

- IPA（独立行政法人情報処理推進機構）による 2019年度未踏IT人材発掘・育成事業（[The MITOU Program](https://www.ipa.go.jp/jinzai/mitou/portal_index.html)）
- かけはし芸術文化振興財団（2022）
- 日本学術振興会 科研費 23K12059「音楽と工学の相互批評的実践としての「音楽土木工学」の研究」（2023-2025）

### Contributers

この一覧には、v1時点での開発、ドキュメント、資金面でのスポンサー（GitHub Sponsors 経由）への貢献者などが含まれます。

#### Source Code Contributions

- [Hiroaki Yutani](https://github.com/yutannihilation)
- [karnpapon](https://github.com/karnpapon)
- [Shinichi Tanaka(t-sin)](https://github.com/t-sin)
- [kyo](https://github.com/syougikakugenn)
- [Inqb8tr-jp](https://github.com/Inqb8tr-jp)
- [zakuro9715](https://github.com/zakuro9715)
- [kuroboshi](https://github.com/kuroboshi)

#### Other forms of Contributions

- [Baku Hashimoto](https://baku89.com)
- [Yuichi Yogo](https://github.com/yuichkun)
- [Ayumu Nagamatsu](http://ayumu-nagamatsu.com/)
- [zigen](https://horol.org/)


## Known Bugs

["bug" タグ付きの GitHub Issues](https://github.com/tomoyanonymous/mimium-rs/issues?q=is%3Aissue+is%3Aopen+label%3Abug) を参照してください。

## [ロードマップ](./Roadmap.md)
