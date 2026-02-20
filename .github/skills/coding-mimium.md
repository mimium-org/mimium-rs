---
name: coding-mimium
description: Basic Language Spec of mimium language. Use when write code in mimium language.
---

mimium has a similar syntax to Rust language but has slightly different specs.

## Type Annotation

- Function type uses arrow like `(a)->b` . Parentheses are necessary even for single argument.
- Code type for type-safe macro can be used with backquote `` `a ``.
- **Any single-small-cap characters in type annotation** is treated as type parameter. We do not use angle brackets like `<T>`.
- Type alias declaration can be used like `type alias Id = (a)->a`.
- Recursive type is supported without box types with `type rec` declaration like `type rec List = Nil | Cons(float,List float)`

## Expressions

- Recursive function definition can be used with `letrec`. `fn hoge(a){...}` is equivalent to `letrec hoge = |a| {...}`.
- Record updating syntax can be used with `{var <- hoge = 10, ... }`. This is equivalent to `{ let tmp = var; tmp.hoge =10; tmp }`
- Parameter pack can be used for any functions. Function with multiple argument is always equivalent to the single-argument function with corresponding record type. Also unpacking for tuple is available if each type of elements are equal to the records.
- Pipe operator can be used. `a |> b` is equivalent to `b(a)`.
- Underscore expression is used for partial application. `a(_,b)` is equivalent to `|x| a(x,b) `.

## Multi stage computation

mimium has special macro system called multi-stage computation. The user can use bracket `` `expr `` and splice `$expr` for embedding/extracting code between stage like Lisp's quote system.

- Bracket value for type a has code type `` `a ``.
- User can specify the stage of computation by declaraing `#stage(macro)` or `#stage(main)`.
- The variable can only be referred from same stage. If you want to use the variable defined in the macro stage in main stage, you have to use `lift` primitive function.
- Macro invocation notation `hoge!(a)` is equivalent to `$(hoge(a))`