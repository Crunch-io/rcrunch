.onAttach <- function (lib, pkg="pkg") {
    if (is.null(getOption("crunch.api.endpoint"))) {
        options(crunch.api.endpoint="http://localhost:8080/api/", warn=1)
    }
    invisible()
}

.crunch <- "http://localhost:8080/api/"
.crunch_api <- function () {
    x <- getOption("crunch.api.endpoint")
    if (is.null(x)) x <- .crunch
    return(x)
}