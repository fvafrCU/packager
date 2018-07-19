- get rid of file.path() with relative paths
  in provide\_cran\_comments:
        check <- parse_check_results(file.path(path, check_log))
  and maybe elsewhere
- in inect(): add a TODO.md with content "- improve test coverage" and
  Rbuildignore it.
- add vignette into main make chain, here or in fakemake
- use fakemake::make in infect with a makelist that's an extension of fakemake's
  "package" list
- run on windows with git unavailable: test for git, win_builder
- document the options (force)
- enhance coverage
- devel the release() function
- add release() conditionally to makelist.
- fix todo in git\_add\_commit() (git2r::status)
- try to re-implement use digest::sha1() for unit testing.
