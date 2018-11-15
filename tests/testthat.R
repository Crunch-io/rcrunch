library(httptest)
options(crunch.check.updates = FALSE)
if (nchar(Sys.getenv("JENKINS_HOME"))) {
    test_check("crunch",
        reporter = MultiReporter$new(list(
            SummaryReporter$new(),
            JunitReporter$new(file = file.path(Sys.getenv("WORKSPACE"), "rcrunch.xml"))
        ))
    )
} else {
    test_check("crunch")
}
