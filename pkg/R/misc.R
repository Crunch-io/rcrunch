is.error <- function (x) inherits(x, "try-error")

update.list <- function (x, y) {
    x[names(y)] <- y
    return(x)
}