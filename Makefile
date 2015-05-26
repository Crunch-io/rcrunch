VERSION = $(shell grep ^Version DESCRIPTION | sed s/Version:\ //)

doc:
	R --slave -e 'library(roxygen2); roxygenise()'
	git add --all man/*.Rd

test:
	R CMD INSTALL --install-tests .
	R --slave -e 'library(testthat); setwd(file.path(.libPaths()[1], "crunch", "tests")); system.time(test_check("crunch", filter="${file}", reporter=ifelse(nchar("${r}"), "${r}", "summary")))'

deps:
	R --slave -e 'install.packages(c("jsonlite", "httr", "codetools", "testthat", "devtools"), repo="http://cran.at.r-project.org", lib=ifelse(nchar(Sys.getenv("R_LIB"), dependencies=TRUE), Sys.getenv("R_LIB"), .libPaths()[1]))'

install-ci: deps
	R CMD INSTALL --install-tests -l $(R_LIB) .
	R -e 'devtools::install_github("nealrichardson/testthat")'

test-ci:
	R --slave -e '.libPaths(Sys.getenv("R_LIB")); options(test.user=Sys.getenv("R_TEST_USER"), test.pw=Sys.getenv("R_TEST_PW"), test.api=Sys.getenv("R_TEST_API")); library(testthat); sink(file="rcrunch.tap"); setwd(file.path(.libPaths()[1], "crunch", "tests")); test_check("crunch", reporter="tap"); sink()'

clean:
	R --slave -e 'options(crunch.api=getOption("test.api"), crunch.email=getOption("test.user"), crunch.pw=getOption("test.pw")); library(crunch); login(); crunch:::.delete_all_my_datasets()'

build: doc
	R CMD build .

check: build
	unset INTEGRATION && R CMD CHECK --as-cran crunch_$(VERSION).tar.gz

vdata:
	cd vignette-data && find *.R | xargs -n 1 R -f

man: doc
	R CMD Rd2pdf man/ --force

md:
	R CMD INSTALL --install-tests .
	mkdir -p inst/doc
	R -e 'setwd("vignettes"); lapply(dir(pattern="Rmd"), knitr::knit)'
	mv vignettes/*.md inst/doc/
	cd inst/doc && ls | grep .md | xargs -n 1 sed -i '' 's/.html)/.md)/g'

build-vignettes: md
	R -e 'setwd("inst/doc"); lapply(dir(pattern="md"), function(x) markdown::markdownToHTML(x, output=sub("\\\\.md", ".html", x)))'
	cd inst/doc && ls | grep .html | xargs -n 1 sed -i '' 's/.md)/.html)/g'
	open inst/doc/getting-started.html