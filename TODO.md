# TODO

## Language
* Add function shorthand; eg. `(%fn (get %1 'name))`
* Add `try`/`catch` to supplement `throw`
* Fix `prn` for RegExp
* Figure out a way to do safe database access, so that a filesystem can be implemented in a separate library and not the interpreter; maybe run core libraries in their own subcontext

## Core
* Modify `sort` to accept a comparator
* Add `sort-by`
* Add `partition`
* Add `juxt`
* Potentially split up `_core` a bit, and make the loading of non-essential functions optional (but otherwise automatic); performance difference may not be worth added char weight