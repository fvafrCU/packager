- use fakemake::make in infect with a makelist that's an extension of fakemake's
  "package" list
- add make.R as template, add function to provide it.
- document the options (force)
- enhance coverage
- devel the release() function
- add release() conditionally to makelist.
- fix todo in git\_add\_commit() (git2r::status)
- try to re-implement use digest::sha1() for unit testing.
