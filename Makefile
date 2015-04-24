VERSION = $(shell grep ^Version pkg/DESCRIPTION | sed s/Version:\ //)

doc:
	R --slave -e 'library(roxygen2); roxygenise("pkg")'
	git add --all pkg/man/*.Rd

test:
	R CMD INSTALL --install-tests pkg
	R --slave -e 'library(testthat); setwd(file.path(.libPaths()[1], "crunch", "tests")); system.time(test_check("crunch", filter="${file}", reporter=ifelse(nchar("${r}"), "${r}", "summary")))'

install-ci:
	R --slave -e 'install.packages(c("jsonlite", "httr", "codetools", "testthat", "devtools"), repo="http://cran.at.r-project.org", lib=Sys.getenv("R_LIB"))'
	R CMD INSTALL --install-tests -l $(R_LIB) pkg

test-ci:
	R --slave -e '.libPaths(Sys.getenv("R_LIB")); options(test.user=Sys.getenv("R_TEST_USER"), test.pw=Sys.getenv("R_TEST_PW"), test.api=Sys.getenv("R_TEST_API")); library(testthat); sink(file="rcrunch.tap"); setwd(file.path(.libPaths()[1], "crunch", "tests")); test_check("crunch", reporter="tap"); sink()'

clean:
	R --slave -e 'options(crunch.api=getOption("test.api"), crunch.email=getOption("test.user"), crunch.pw=getOption("test.pw")); library(crunch); login(); crunch:::.delete_all_my_datasets()'

build: doc
	R CMD build pkg

check: build
	unset INTEGRATION && R CMD CHECK --as-cran crunch_$(VERSION).tar.gz

vdata:
	cd vignette-data && find *.R | xargs -n 1 R -f

man: doc
	R CMD Rd2pdf pkg/man/ --force

build-vignettes:
	mkdir -p pkg/doc
	R -e 'setwd("pkg/vignettes"); lapply(lapply(dir(pattern="Rmd"), knitr::knit), function(x) markdown::markdownToHTML(x, output=sub("\\\\.md", ".html", x)))'
	mv pkg/vignettes/*.md pkg/doc/
	mv pkg/vignettes/*.html pkg/doc/
	cd pkg/doc && ls | grep .md | xargs -n 1 sed -i '' 's/.html)/.md)/g'
	open pkg/doc/getting-started.html