.onAttach <- function (lib, pkg="pkg") {
    if (is.null(getOption("crunch.api"))) {
        options(crunch.api="https://beta.crunch.io/api/")
    }
    if (is.null(getOption("crunch.max.categories"))) {
        options(crunch.max.categories=256)
    }
    options(warn=1)
    assign("application/json", parseJSONresponse, envir=httr:::parsers)
    invisible()
}
