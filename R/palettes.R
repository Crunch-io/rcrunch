AnalyticPalettes <- function(x) {
    x <- lapply(x, AnalyticPalette)
    if (is.null(names(x))) names(x) <- vapply(x, function(item) item$name, character(1))
    new("AnalyticPalettes", x)
}

AnalyticPalette <- function(x) {
    x$palette <- unlist(x$palette)
    new("AnalyticPalette", x)
}

is.AnalyticPalette <- function(x) {
    inherits(x, "AnalyticPalette")
}

#' @rdname palettes
#' @export
setMethod("palettes", "CrunchDataset", function(x) {
    AnalyticPalettes(x@body$palette$analysis)
})

#' @rdname palettes
#' @export
setMethod("defaultPalette", "CrunchDataset", function(x, ...) {
    defaultPalette(palettes(x), ...)
})

#' @rdname palettes
#' @export
setMethod("defaultPalette", "AnalyticPalettes", function(x, ...) {
    default <- vapply(x, function(pal) pal$default, logical(1))
    x[[which(default)[1]]]
})
