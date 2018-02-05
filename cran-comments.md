Dear CRAN Team,
this is a resubmission of package 'packager'. I have added the following changes:

* Setting inital package version to '0.1.0'.
* Using the minor R version in DESCRIPTION (not the patched one).
* Fixed setting a package's title if no description is given.
* Set the argument author\_at\_r for function set\_package\_info to default to 
  option packager/whoami.
* Add a failsafe version of git2r::commit called git\_commit().
* Added exception handling if reading the git config throws an error by
  conditionally setting a local git config.
* Added option `verbose` to create().
* Sanitized the return value of git\_sync\_status().
* Linted the codes heavily.
* Added an inclusion pattern to check\_codetags() and set reasonable defaults
  for patterns.

Please upload to CRAN.
Best, Andreas Dominik

# Package packager 0.8.0
## Test  environments 
- R Under development (unstable) (2018-01-12 r74112)
  Platform: x86_64-pc-linux-gnu (64-bit)
  Running under: Devuan GNU/Linux 1 (jessie)
- R version 3.4.2 (2017-01-27)
  Platform: x86_64-pc-linux-gnu (64-bit)
  Running under: Ubuntu 14.04.5 LTS
- win-builder (devel)

## R CMD check results
0 errors | 1 warning  | 1 note 
