is.insertion <- function (x) inherits(x, "Insertion")

setValidity("Insertion", function (object) {
    val <- TRUE
    reqs <- c("anchor", "name")
    mems <- reqs %in% names(object)
    if (!all(mems)) {
        val <- paste0("An Insertion must have at least ",
                      serialPaste(dQuote(reqs)), ". Missing: ",
                      serialPaste(dQuote(reqs[!mems])))
    }

    if (!is.na(func(object)) && is.na(args(object))) {
        # add checking so that args is either a vectorOrList of numerics
        val <- paste0("If an Insertion has a ", dQuote("function"),
                      " it must also have ", dQuote("args"))
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

validateNewSubtotal <- function (comb) {
    if (!is.numeric(comb)) {
        halt("a subtotal must be a numeric")
    }
    return(comb)
}

setSubtotal <- function (x, value) {
    x[["args"]] <- validateNewSubtotal(value)
    x[["function"]] <- "subtotal"
    return(x)
}

#' @rdname Insertions
#' @export
setMethod("anchor<-", "Insertion", setAnchor)

#' @rdname Insertions
#' @export
setMethod("subtotals<-", "Insertion", setSubtotal)