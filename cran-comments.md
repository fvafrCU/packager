Dear CRAN Team,
this is a resubmission of package 'packager'. I have added the following changes:

Wrapped the travis-cli interface into tryCatch to be able to use my Makefile as
template on systems where travis-cli will fail.

Please upload to CRAN.
Best, Andreas Dominik

# Package packager 0.6.0
## Test  environments 
- R Under development (unstable) (2017-08-15 r73096)
  Platform: x86_64-pc-linux-gnu (64-bit)
  Running under: Debian GNU/Linux 9 (stretch)
- R version 3.4.2 (2017-01-27)
  Platform: x86_64-pc-linux-gnu (64-bit)
  Running under: Ubuntu 14.04.5 LTS
- win-builder (devel)

## R CMD check results
0 errors | 0 warnings | 2 notes
