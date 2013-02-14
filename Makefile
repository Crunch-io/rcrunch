doc:
	R --slave -e 'library(devtools); document("pkg")'
	git add pkg/man/*.Rd

test:
	R CMD INSTALL pkg
	R --slave -e 'library(testthat); test_package("rcrunch")'