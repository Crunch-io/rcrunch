context("UTF-8 Encoding")

if (run.integration.tests) {
    ## Move the actual tests to a different file so that non-Unicode
    ## environments won't fail to parse (even if not running these tests)
    source("utftesting.R", local=TRUE)
}