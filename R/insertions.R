is.insertion <- function (x) inherits(x, "Insertion")

setValidity("Insertion", function (object) {
    is.cat <- all(c("anchor", "name") %in% names(object))
    if (!all(is.cat)) {
        val <- "Not an insertion"
    } else {
        val <- TRUE
    }
    return(val)
})

setValidity("Insertions", function (object) {
    are.inserts <- vapply(object, is.insertion, logical(1))
    if (!all(are.inserts)) {
        badcount <- sum(!are.inserts)
        return(paste0("Invalid insertions: ", badcount,
                      ifelse(badcount>1,
                             " elements are not Crunch insertion objects.",
                             " element is not a Crunch insertion object.")))
    }
    if (any(duplicated(names(object)))) {
        return("Invalid insertion names: must be unique")
    }
    return(TRUE)
})

is.insertions <- function (x) inherits(x, "Insertions")

setAnchor <- function(x, value) {
    halt("not implemented yet.")
}
setCombine <- function(x, value) {
    halt("not implemented yet.")
}



#' @rdname Insertions
#' @export
setMethod("names", "Insertions", function (x) {
    n <- vapply(x, name, character(1))
    return(n)
})

#' @rdname Insertions
#' @export
setMethod("anchor", "Insertion", function (x) {
    n <- x[["anchor"]]
    return(ifelse(is.null(n), NA_character_, n))
})
#' @rdname Insertions
#' @export
setMethod("anchor<-", "Insertion", setAnchor)

#' @rdname Insertions
#' @export
setMethod("combinations", "Insertion", function (x) {
    n <- x[["function"]][["combine"]]
    if (is.null(n)) {
        return(NA)
    }
    return(n)
})
#' @rdname Insertions
#' @export
setMethod("combinations<-", "Insertion", setCombine)