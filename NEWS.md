
# packager 0.9.0
* Added function print\_lints() to print lints sorted by patterns
  matching source file names.


# packager 0.8.0

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

# packager 0.7.0

* Added function release() which skips the usual interactive questions done
  by devtools::release().
* Added function provide\_make\_list() which is an exentsion to 
  fakemake::provide\_make\_list().
* Enhanced docs for provide\_cran\_comments().

# packager 0.6.0

Wrapped the travis-cli interface into tryCatch to be able to use my Makefile as
template on systems where travis-cli will fail.

# packager 0.5.0

- Fixed testing.
- Added internal function strip\_off\_attributes(), mainly to get rid of object 
names.
- Fixed querying the package's maintainer's name.
- Updated package's info.


# packager 0.4.1

Fixed adding github url to DESCRIPTION.

# packager 0.4.0

- Hotfixed git\_tag()
- Added internal function git\_add\_commit() to mimic `git commit -am"MESSAGE"`.

# packager 0.3.1

provide\_cran\_comments(name = ) now defaults to NA, using the DESCRIPTION's
maintainer's given name.

# packager 0.3.0

Resetting RUnit tests.

# packager 0.2.2

Vignette defaults are now set from details and description passed to 
set\_package\_info.

# packager 0.2.1

Added bugfix for get\_news().

# packager 0.2.0

Added function to add github url to DESCRIPTION.

# packager 0.1.0

* Added a `NEWS.md` file to track changes to the package.

