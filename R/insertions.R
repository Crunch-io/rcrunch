is.insertion <- function (x) inherits(x, "Insertion")

setValidity("Insertion", function (object) {
    reqs <- c("anchor", "name")
    mems <- reqs %in% names(object)
    if (!all(mems)) {
        val <- paste0("An Insertion must have at least ",
                      serialPaste(dQuote(reqs)), ". Missing: ",
                      serialPaste(dQuote(reqs[!mems])))
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
                             " elements are not Crunch Insertion objects.",
                             " element is not a Crunch Insertion object.")))
    }
    if (any(duplicated(names(object)))) {
        return("Invalid insertion names: must be unique")
    }
    return(TRUE)
})

is.insertions <- function (x) inherits(x, "Insertions")

validateNewAnchor <- function (anchor) {
    if (!is.numeric(anchor)) {
        halt("an anchor must be a numeric")
    }

    return(anchor)
}

setAnchor <- function (x, value) {
    x[["anchor"]] <- validateNewAnchor(value)
    return(x)
}

validateNewCombine <- function (comb) {
    if (!is.numeric(comb)) {
        halt("a combination must be a numeric")
    }

    return(comb)
}

setCombine <- function (x, value) {
    x[["function"]][["combine"]] <- validateNewCombine(value)
    return(x)
}

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