grep-tree
=========
STATUS: Experimental

Implemented features:
 * NONE

Install
-------
You need cabal-dev.
 1. $ git clone https://github.com/mathfur/grep-tree.git
 2. $ cd grep-tree
 3. $ cabal-dev install --only-dependencies
 4. $ cabal-dev configure
 5. $ cabal-dev build
 6. $ cabal-dev install
 7. Add dist/build/grep-tree/grep-tree to PATH

Usage
-----
 1. $ grep-tree --wordopt=(SEARCH WORD) --depthopt=(SEARCH DEPTH) --outputopt=input.json --wdiropt=(TARGET DIR)
 2. copy input.json to grep-tree directory.
 3. Open index.html by browser.

Memo
----
```
class => ':'
module => '_'
class method => '.'
instance method => '#'
block => '~'
if => '\'
other => '@'
end => '/'
```

License
-------
Copyright &copy; 2012 mathfur
Distributed under the [MIT License][mit].
[MIT]: http://www.opensource.org/licenses/mit-license.php
