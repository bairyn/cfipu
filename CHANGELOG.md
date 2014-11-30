PLACEHOLDER
-----

1.1.0.2
-----
* Fixed build errors.
  * Cfipu.hs exports main.
  * Add newly missing import of Control.Exception in Cfipu.hs.
  * Replace mtl's pure state monad constructor with `state`, previously
    `State`.
* Add missing main-is field, fixing build errors.
* Correct git repository URL for head.

1.1.0.1
-----
* Add homepage and bug-reports to cabal file, linking to the github repository
  and its issue tracker, respectively.
* Added `CHANGELOG.md`.
