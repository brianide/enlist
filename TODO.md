# TODO

## General
Relaxing the direct-interop constraint would have a lot of interesting ramifcations. On one hand, hash-map literals could be implemented on top of JS Maps, which would also allow for non-string keys. Vectors and lazy sequences could also be implemented to match the suite of native JS array functions that support enlist's map/filter/reduce/etc., which would enable more idiomatic Clojure constructs.

The tradeoff would be that any object "leaving" enlist back to the land of JS would need to be explicitly marshalled back to the proper types. I can't think of a way of doing this that doesn't annoy the shit out of me.

```clojure
(->> ($scr 'upgrades {'full true})
     (filter even?)
     (hash-map 'i)
     $marshal
     ($scr 'cull))
```

Barf.

## Language
* Add anonymous function shorthand
* Defer as-function application to core if possible

## Core
* Add `sort-by`
* Add macro for dispatching based on argument constructor
