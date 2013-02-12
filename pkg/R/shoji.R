is.shoji.like <- function (x) {
    is.list(x) && all(c("element", "self", "description") %in% names(x))
}

##' @S3method is shoji
is.shoji <- function (x) inherits(x, "shoji")

setOldClass("shoji")