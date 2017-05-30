VERSION = $(shell grep ^Version DESCRIPTION | sed s/Version:\ //)

doc:
	R --slave -e 'devtools::document()'
	git add --all man/*.Rd

test:
	R CMD INSTALL --install-tests .
	R --slave -e 'library(httptest); setwd(file.path(.libPaths()[1], "crunch", "tests")); options(crunch.check.updates=FALSE); system.time(test_check("crunch", filter="${file}", reporter=ifelse(nchar("${r}"), "${r}", "summary")))'

deps:
	R --slave -e 'install.packages(c("devtools", "Rcpp", "testthat", "jsonlite", "curl", "httpcache", "codetools", "httptest"), repo="http://cran.at.r-project.org", lib=ifelse(nchar(Sys.getenv("R_LIB")), Sys.getenv("R_LIB"), .libPaths()[1]))'

install-ci: deps
	R CMD INSTALL --install-tests -l $(R_LIB) .
	R -e '.libPaths(Sys.getenv("R_LIB")); devtools::install_github("nealrichardson/testthat", ref="tap-file"); devtools::install_github("nealrichardson/httptest", ref="1.3.0")'

test-ci:
	R --slave -e '.libPaths(Sys.getenv("R_LIB")); library(httptest); cwd <- getwd(); setwd(file.path(.libPaths()[1], "crunch", "tests")); options(crunch.check.updates=FALSE); test_check("crunch", reporter=MultiReporter$$new(list(SummaryReporter$$new(), TapReporter$$new(file.path(cwd, "rcrunch.tap")))))'

clean:
	R --slave -e 'options(crunch.api=getOption("test.api"), crunch.email=getOption("test.user"), crunch.pw=getOption("test.pw")); library(crunch); login(); lapply(urls(datasets()), crDELETE)'

build: doc
	R CMD build .

check: build
	-unset INTEGRATION && export _R_CHECK_CRAN_INCOMING_REMOTE_=FALSE && R CMD CHECK --as-cran crunch_$(VERSION).tar.gz
    # cd crunch.Rcheck/crunch/doc/ && ls | grep .html | xargs -n 1 egrep "<pre><code>.. NULL" >> ../../../vignette-errors.log
	rm -rf crunch.Rcheck/
    # cat vignette-errors.log
    # rm vignette-errors.log

vdata:
	cd vignette-data && R -f make-vignette-rdata.R

man: doc
	R CMD Rd2pdf man/ --force

md:
	R CMD INSTALL --install-tests .
	mkdir -p inst/doc
	R -e 'setwd("vignettes"); lapply(dir(pattern="Rmd"), knitr::knit, envir=globalenv())'
	mv vignettes/*.md inst/doc/
	-cd inst/doc && ls | grep .md | xargs -n 1 sed -i '' 's/.html)/.md)/g'
	-cd inst/doc && ls | grep .md | xargs -n 1 egrep "^.. Error"

build-vignettes: md
	R -e 'setwd("inst/doc"); lapply(dir(pattern="md"), function(x) markdown::markdownToHTML(x, output=sub("\\\\.md", ".html", x)))'
	cd inst/doc && ls | grep .html | xargs -n 1 sed -i '' 's/.md)/.html)/g'
	# That sed isn't working, fwiw
	open inst/doc/getting-started.html

covr:
	R --slave -e 'Sys.setenv(R_TEST_USER=getOption("test.user"), R_TEST_PW=getOption("test.pw"), R_TEST_API=getOption("test.api")); library(covr); cv <- package_coverage(); df <- covr:::to_shiny_data(cv)[["file_stats"]]; cat("Line coverage:", round(100*sum(df[["Covered"]])/sum(df[["Relevant"]]), 1), "percent\\n"); shine(cv, browse=TRUE)'
