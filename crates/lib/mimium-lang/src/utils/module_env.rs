use std::thread::Scope;

enum LookupRes<'a, V: Clone> {
    Local(&'a V),
    UpValue(&'a V),
    Global(&'a V),
}

// Language Serverなど、利用する側のインターフェース
trait EnvInterface<'a> {
    type Symbol: Eq;
    type Cursor: Copy;
    type Value: Clone;
    type Span: Clone;
    fn lookup_qualified(
        &'a self,
        name: &[Self::Symbol],
        cursor: Self::Cursor,
    ) -> Option<LookupRes<'a, Self::Value>>;
    fn get_completion_candidates(&self, position: Self::Span) -> Vec<String>;
}

slotmap::new_key_type! {
    pub struct ModuleEnvKey;
    pub struct ScopeEnvKey;
}
enum Visibility {
    Private,
    Public,
}
enum ModuleEntry<V: Clone + PartialEq> {
    ConstValue(Visibility, String, V),
    Function(Visibility, String, V, ScopeEnv<V>),
    Module(Visibility, String, ModuleEnvKey),
}

struct ModuleEnvInner<V: Clone + PartialEq> {
    parent: Option<ModuleEnvKey>,
    entries: Vec<ModuleEntry<V>>,
}

struct ModuleEnv<V: Clone + PartialEq> {
    modules: slotmap::SlotMap<ModuleEnvKey, ModuleEnvInner<V>>,
    scopes: slotmap::SlotMap<ScopeEnvKey, ScopeEnv<V>>,
    root: ModuleEnvKey,
}

struct ScopeEnv<V: Clone + PartialEq> {
    parent: Option<ScopeEnvKey>,
    binds: Vec<(String, V)>,
    children: Vec<ScopeEnvKey>,
}
