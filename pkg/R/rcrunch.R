.onAttach <- function (lib, pkg="pkg") {
    if (is.null(getOption("crunch.api"))) {
        options(crunch.api="http://localhost:8080/api/", warn=1)
    }
    assign("application/json", parseJSONresponse, envir=httr:::parsers)
    invisible()
}
