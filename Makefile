doc:
	R --slave -e 'library(devtools); document("pkg")'
	git add pkg/man/*.Rd

test:
	R CMD INSTALL pkg
	R --slave -e 'library(testthat); system.time(test_package("rcrunch", filter="${file}"))'

test-ci:
	R --slave -e 'install.packages(c("httr", "RJSONIO", "codetools", "testthat"), repo="http://cran.at.r-project.org")'
	R CMD INSTALL pkg
	R --slave -e 'library(testthat); sink(file="rcrunch.tap"); test_package("rcrunch", reporter="tap"); sink()'

clean:
	R --slave -e 'options(crunch.api=getOption("test.api"), crunch.email=getOption("test.user"), crunch.pw=getOption("test.pw")); library(rcrunch); login(); rcrunch:::.delete_all_my_datasets()'
