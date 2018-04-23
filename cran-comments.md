Dear CRAN Team,
this is a resubmission of package 'packager'. I have added the following changes:

* Added update\_deps() for updating package dependencies like internals from
  package remotes does.
* provide\_cran\_comments() now optionally reads a travis log from file.
* Fixed git commit in release().

Please upload to CRAN.
Best, Andreas Dominik

# Package packager 0.10.0
## Test  environments 
- R Under development (unstable) (2018-01-30 r74185)
  Platform: x86_64-pc-linux-gnu (64-bit)
  Running under: Debian GNU/Linux 9 (stretch)
- log/travis_log.txt
- win-builder (devel)

## R CMD check results
0 errors | 1 warning  | 1 note 
