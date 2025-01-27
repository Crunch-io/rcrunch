tt_read_lines <- function(path, n = -1L, encoding = "UTF-8") {
    base::readLines(path, n = n, encoding = encoding, warn = FALSE)
}

tt_write_lines <- function(text, path) {
    base::writeLines(enc2utf8(text), path, useBytes = TRUE)
}

# trick to get around :: from depends CRAN check
R6Class <- get("R6Class", asNamespace("R6"))

JunitReporterFix <- R6Class(
    "JunitReporterFix",
    inherit = JunitReporter,
    public = list(
        end_reporter = function() {
            super$end_reporter()
            tt_write_lines(
                crayon::strip_style(tt_read_lines(self$out)),
                self$out
            )
        }
    )
)
