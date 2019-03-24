# TODO

## Language
* Add function shorthand; eg. `(%fn (get %1 'name))`
* Add `try`/`catch` to supplement `throw`
* Fix `prn` for RegExp
* Figure out a way to do safe database access, so that a filesystem can be implemented in a separate library and not the interpreter; maybe run core libraries in their own subcontext with access to `$db`
* Consider bringing back groups (special arrays which evaluate to standard arrays containing their evaluated members)
* Do something about multiple arities
* Catch exceptions at top level and return as hackmud success/failure message
* Set literals
* Destructuring

## Core
* Modify `sort` to accept a comparator
* Scrutinize usages of `concat`
* Add `sort-by`
* Add `partition`
* Add `condp`
* Potentially split up `_core` a bit, and make the loading of non-essential functions optional (but otherwise automatic); performance difference may not be worth added char weight

## Uploader
* Add preprocessing phase to remove semicolon comments
* Automatically encode backticks to hex so that ELN files can contain them
