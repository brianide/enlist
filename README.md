# enlist
A small, moderately-golfed but generally-legible, Clojure/MAL/soron.lisp-inspired Lisp intepreter for use in HackMUD. 

The code herein doesn't represent especially good Javascript conventions (to put it lightly), but I've tried to make it as easy to follow as possible without succumbing to the char count limit.

![Example](https://github.com/brianide/enlist/raw/master/example.png)

## Features
* Decent library of functions and macros mostly matching the functionality of their Clojure counterparts
* Curly-brace shorthand for hash-map definitions
* RegExp literals
* Symbols, hash-maps, lists, and sets applicable as functions
* Hygienic macros with automatic symbol generation via `#` suffix
* Multi-arity function definitions via `defa`
* `loop`/`recur` iteration model
* Array destructuring within `let`/`fn`/`loop` forms, with nesting and variadics

## Caveats and Weirdness
* Square-brackets are synonymous with parentheses, as in Chicken Scheme.
* Curly-braces are strictly a shorthand for `(hash-map ...)`; they are not hash-map literals.
* Variadic args receive an empty list, rather than nil, when nothing is supplied for them.
* No implicit `loop`/`recur` frames.
* Lists are just JS arrays, hash-maps are just JS objects, symbols are just JS strings, and functions are just JS functions. There are limitations that come with this -- TCO is nearly non-existent, vectors (and literal syntax for them) don't exist, maps only support string keys, etc. -- but it drastically simplifies interop.
* Mutating functions are dangerous and should rarely be used directly. They're particularly insidious when used in conjunction with self-evaluating forms present in the AST, as it becomes very easy to inadvertently alter those forms within the AST itself.

## Thanks
* I never would have undertaken this project if @ethankaminski hadn't done it first, and several ideas are pilfered wholesale from soron.lisp.
* The [Make A Lisp](https://github.com/kanaka/mal) project was an indespensable resource on this project, and I can't recommend it enough, especially if you're completely clueless about Lisp, as I was when I started.
