library(httptest)
if (nchar(Sys.getenv("JENKINS_HOME"))) {
    options(crunch.check.updates=FALSE, download.file.method="curl")
    test_check("crunch",
        reporter=MultiReporter$new(list(
            SummaryReporter$new(),
            TapReporter$new(file.path(Sys.getenv("WORKSPACE"), "rcrunch.tap"))
        )))
} else {
    test_check("crunch")
}
