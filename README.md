grep-tree
=========
This tool grep a word recursively at rails project, and output graphical tree.

STATUS: Experimental

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
 1. $ cd (TARGET DIR)
 1. $ rake routes > rake_routes
 2. $ grep-tree --depth=3 --output=/foo/bar/input.json (SEARCHWORD)
 3. copy input.json to public directory under grep-tree project.
 4. Open public/index.html by browser.

Memo
----
```
class => ':'
module => '_'
class method => '.'
instance method => '#'
block => 'B'
if => '|'
current line => '@'
end => '>'
```

License
-------
Copyright &copy; 2012 mathfur
Distributed under the [MIT License][mit].
[MIT]: http://www.opensource.org/licenses/mit-license.php
