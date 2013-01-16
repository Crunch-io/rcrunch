is.shoji <- function (x) {
    is.list(x) && all(c("element", "self", "specification", "description") %in% names(x))
}