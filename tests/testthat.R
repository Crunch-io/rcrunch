library(httptest)
if (nchar(Sys.getenv("JENKINS_HOME"))) {
    options(
        crunch.check.updates=FALSE,
        testthat.tap.output_file=file.path(Sys.getenv("WORKSPACE"), "rcrunch.tap"),
        testthat.junit.output_file=file.path(Sys.getenv("WORKSPACE"), "rcrunch.xml")
    )
    test_check("crunch",
        reporter=MultiReporter$new(list(
            SummaryReporter$new(),
            TapReporter$new(),
            JunitReporter$new()
        )))
} else {
    test_check("crunch")
}
