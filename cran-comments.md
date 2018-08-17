Dear CRAN Team,
this is a resubmission of package 'packager'. I have added the following changes:

* provide\_cran\_comments() now reads info from logs on gitlab.com, given that
  .gitlab-ci.yml from this package (via packager:::use_gitlab_ci()) is used.

Please upload to CRAN.
Best, Andreas Dominik

# Package packager 0.16.0
## Test  environments 
- R Under development (unstable) (2018-07-01 r74950)
    Platform: x86_64-pc-linux-gnu (64-bit)
    Running under: Devuan GNU/Linux ascii
    0 errors | 1 warning  | 1 note 
- gitlab.com
  R version 3.5.1 (2018-07-02)
  Platform: x86_64-pc-linux-gnu (64-bit)
  Running under: Debian GNU/Linux 9 (stretch)
  Status: 1 NOTE
- win-builder (devel)
