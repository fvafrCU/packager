# Force posix:
.POSIX:

R = R-devel
Rscript = Rscript-devel

PKGNAME = $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS = $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  = $(shell pwd)
R_FILES = $(shell find R/ -type f -name "*.[rR]" -print)
MAN_FILES = $(shell find man/ -type f -print)
RUNIT_FILES = $(shell find tests/ -type f  -print | grep  'runit')
TESTTHAT_FILES = $(shell find tests/ -type f  -print | grep  'testthat')
VIGNETTES_FILES = $(shell find vignettes/ -type f -print)
INST_FILES = $(shell find inst/ -type f -print)
DEPS = "callr", "rprojroot", "covr", "knitr", "devtools", "rmarkdown", "RUnit", "checkmate", "roxygen2", "lintr", "hunspell", "roxygen2", "cleanr"
TEMP_FILE = $(shell tempfile)
LOG_DIR = log
R = R-devel
Rscript = Rscript-devel

.PHONY: all
all: $(LOG_DIR)/install.Rout devel

# devel stuff
.PHONY: devel
devel: vignettes
.PHONY: vignettes
vignettes:
	$(Rscript) --vanilla -e 'devtools::build_vignettes(); lapply(tools::pkgVignettes(dir = ".")[["docs"]], function(x) knitr::purl(x, output = file.path(".", "inst", "doc", sub("\\.Rmd$$", ".R", basename(x))), documentation = 0))'

# install
.PHONY: install
install: $(LOG_DIR)/install.Rout
$(LOG_DIR)/install.Rout: $(LOG_DIR)/cran_comments.Rout
	$(R) --vanilla CMD INSTALL  $(PKGNAME)_$(PKGVERS).tar.gz > $(LOG_DIR)/install.Rout 2>&1 

cran-comments.md: #$(LOG_DIR)/check.Rout
	$(Rscript) --vanilla -e 'packager::provide_cran_comments(check_log = "log/check.Rout")' > $(LOG_DIR)/cran_comments.Rout 2>&1 

.PHONY: check
check: $(LOG_DIR)/check.Rout
$(LOG_DIR)/check.Rout: $(PKGNAME)_$(PKGVERS).tar.gz 
	export _R_CHECK_FORCE_SUGGESTS_=TRUE && \
		$(R) --vanilla CMD check --as-cran --run-donttest $(PKGNAME)_$(PKGVERS).tar.gz; \
		cp $(PKGNAME).Rcheck/00check.log $(LOG_DIR)/check.Rout

.PHONY: build
build: $(PKGNAME)_$(PKGVERS).tar.gz 
$(PKGNAME)_$(PKGVERS).tar.gz: NEWS.md README.md DESCRIPTION LICENSE \
	$(LOG_DIR)/roxygen2.Rout $(R_FILES) $(MAN_FILES) $(TESTTHAT_FILES) \
	$(RUNIT_FILES) $(VIGNETTES_FILES) $(INST_FILES) $(LOG_DIR)/spell.Rout \
	$(LOG_DIR)/check_codetags.Rout $(LOG_DIR)/news.Rout $(LOG_DIR)/runit.Rout \
	$(LOG_DIR)/testthat.Rout $(LOG_DIR)/covr.Rout $(LOG_DIR)/cleanr.Rout \
	$(LOG_DIR)/lintr.Rout
	$(R) --vanilla CMD build $(PKGSRC)

README.md: README.Rmd R/$(PKGNAME)-package.R
	$(Rscript) --vanilla -e 'knitr::knit("README.Rmd")'

$(LOG_DIR)/roxygen2.Rout: $(LOG_DIR) $(R_FILES)
	$(R) --vanilla -e 'roxygen2::roxygenize(".")' > $(LOG_DIR)/roxygen2.Rout 2>&1 

$(LOG_DIR): 
	$(Rscript) --vanilla -e 'devtools:::use_directory("log", ignore = TRUE)' # FIXME: use packager::

# utils
.PHONY: clean
clean:
	rm -rf $(PKGNAME).Rcheck

.PHONY: remove
remove:
	 $(R) --vanilla CMD REMOVE  $(PKGNAME)

.PHONY: viz
viz: $(LOG_DIR)/make.png 
$(LOG_DIR)/make.png: $(LOG_DIR) Makefile $(R_FILES) $(MAN_FILES) \
	$(TESTTHAT_FILES) $(RUNIT_FILES) $(VIGNETTES_FILES) $(INST_FILES)
	make -Bnd all devel | make2graph | dot -Tpng -o $(LOG_DIR)/make.png

# checks
.PHONY: cleanr
cleanr: $(LOG_DIR)/cleanr.Rout 
$(LOG_DIR)/cleanr.Rout: $(R_FILES)
	$(Rscript) --vanilla -e 'tryCatch(cleanr::check_directory("R/", check_return = FALSE), cleanr = function(e) print(e))' > $(LOG_DIR)/cleanr.Rout 2>&1 

.PHONY: lintr
lintr: $(LOG_DIR)/lintr.Rout 
$(LOG_DIR)/lintr.Rout: $(R_FILES) $(VIGNETTES_FILES)
	$(Rscript) --vanilla -e 'lintr::lint_package(path = ".")' > $(LOG_DIR)/lintr.Rout 2>&1 

.PHONY: coverage
coverage: $(LOG_DIR)/covr.Rout 
$(LOG_DIR)/covr.Rout: $(R_FILES) $(TESTTHAT_FILES) $(RUNIT_FILES)
	$(Rscript) --vanilla -e 'co <- covr::package_coverage(path = ".", function_exclusions = "\\.onLoad"); covr::zero_coverage(co); print(co)' > $(LOG_DIR)/covr.Rout 2>&1 

.PHONY: testthat
testthat: $(LOG_DIR)/testthat.Rout 
$(LOG_DIR)/testthat.Rout: $(LOG_DIR) $(R_FILES) $(TESTTHAT_FILES)
	$(Rscript) --vanilla -e 'devtools::test()' >  $(LOG_DIR)/testthat.Rout 2>&1

.PHONY: runit
runit: $(LOG_DIR)/runit.Rout
$(LOG_DIR)/runit.Rout: $(LOG_DIR) $(R_FILES) $(RUNIT_FILES)
	$(Rscript) --vanilla tests/runit.R > $(LOG_DIR)/runit.Rout 2>&1 
	
.PHONY: news
news: $(LOG_DIR)/news.Rout
$(LOG_DIR)/news.Rout: $(LOG_DIR) DESCRIPTION NEWS.md
	$(Rscript) --vanilla -e 'packager::check_news()' > $(LOG_DIR)/news.Rout 2>&1 

.PHONY: codetags
codetags: $(LOG_DIR)/check_codetags.Rout 
$(LOG_DIR)/check_codetags.Rout:
	$(Rscript) --vanilla -e 'packager::check_codetags()' > $(LOG_DIR)/check_codetags.Rout 2>&1 

.PHONY: spell
spell: $(LOG_DIR)/spell.Rout
$(LOG_DIR)/spell.Rout: $(LOG_DIR) DESCRIPTION $(LOG_DIR)/roxygen2.Rout $(MAN_FILES)
	$(Rscript) --vanilla -e 'spell <- devtools::spell_check(); if (length(spell) > 0) {print(spell); warning("spell check failed")} ' > $(LOG_DIR)/spell.Rout 2>&1 

##   #% devtools
##   # a loose collection of helpful stuff while developing
##   
##   .PHONY: tag
##   tag: $(LOG_DIR)/git_tag.Rout 
##   .PHONY: $(LOG_DIR)/git_tag.Rout 
##   $(LOG_DIR)/git_tag.Rout: 
##   	$(R) --vanilla -e 'source(file.path("utils", "git_tag.R")); git_tag()'
##   
##   .PHONY: build_win
##   build_win:
##   	echo "Run \n \t$(Rscript) --vanilla -e 'devtools::build_win()'"
##   
##   .PHONY: release
##   release: build_win
##   	echo "Run \n \t$(R) interacitvely and do 'devtools::release(check = FALSE)'"
##   
##   .PHONY: use_dev_version
##   use_dev_version: $(LOG_DIR)/use_dev_version.Rout
##   .PHONY: $(LOG_DIR)/use_dev_version.Rout
##   $(LOG_DIR)/use_dev_version.Rout:
##   	$(Rscript) --vanilla -e 'devtools::use_dev_version()' > $(LOG_DIR)/use_dev_version.Rout 2>&1 
##   
##   .PHONY: dependencies_forced
##   dependencies_forced: $(LOG_DIR)/dependencies_forced.Rout
##   $(LOG_DIR)/dependencies_forced.Rout:
##   	$(Rscript) --vanilla -e 'deps <-c($(DEPS)); for (dep in deps) install.packages(dep, repos = "https://cran.uni-muenster.de/")' > $(LOG_DIR)/dependencies_forced.Rout 2>&1 
##   
