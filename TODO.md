- pour devel into a functions: create and infect
- provide a commented default function to create a functional package (a
  throw()) and default test cases for Runit and testthat using wishkers
  These are hardcoded in : 
      - R/throw.R
      - tests/runit.R
      - tests/runit/throw.R
      - tests/testthat/test-throw.R
  add a function provide\_throw() that provides these files using whisker
  templates from the above files and that adds projroot/log/ to .Rbuildignore.
- Add RUnit to suggests in DESCRIPTION
- Use is\_null\_or\_[true|false]() to set overriding in use\_devtools() and
  providing other files (set\_package\_info)
- function to provide a .travis.yml from ... here?
- use MakefileR to create a Makefile
