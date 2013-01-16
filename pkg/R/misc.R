is.error <- function (x) inherits(x, "error")

update.list <- function (x, y) {
    x[names(y)] <- y
    return(x)
}