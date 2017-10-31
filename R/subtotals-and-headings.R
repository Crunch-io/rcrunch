is.Subtotal <- function (x) inherits(x, "Subtotal")
is.Heading <- function (x) inherits(x, "Heading")

setValidity("Subtotal", function (object) {
    reqs <- c("name", "categories", "after")
    mems <- reqs %in% names(object)
    if (!all(mems)) {
        val <- paste0("A Subtotal must have at least ",
                      serialPaste(dQuote(reqs)), ". Missing: ",
                      serialPaste(dQuote(reqs[!mems])))
    } else {
        val <- TRUE
    }

    return(val)
})

setValidity("Heading", function (object) {
    reqs <- c("name", "after")
    mems <- reqs %in% names(object)
    if (!all(mems)) {
        val <- paste0("A Heading must have at least ",
                      serialPaste(dQuote(reqs)), ". Missing: ",
                      serialPaste(dQuote(reqs[!mems])))
    } else if ("categories" %in% names(object)) {
        val <- paste0("A Heading cannot have ", dQuote("categories"),
                      ". Did you mean to make a Subtotal?")
    } else {
        val <- TRUE
    }

    return(val)
})

getSubtotals <- function (x) {
    inserts <- transforms(x)$insertions

    sub_heads <-lapply(inserts, function (insrt) {
        if (is.null(insrt$`function`) || insrt$`function` == "subtotal") {
            return(insrt)
        }
        return(NULL)
    })
    # remove NULLs
    sub_heads@.Data <- Filter(Negate(is.null), sub_heads@.Data)

    return(sub_heads)
}

#' @rdname SubtotalsHeadings
#' @export
setMethod("subtotals", "CrunchVariable", getSubtotals)
#' @rdname SubtotalsHeadings
#' @export
setMethod("subtotals", "VariableTuple", getSubtotals)

#' @rdname SubtotalsHeadings
#' @export
setMethod("subtotals<-", c("CrunchVariable", "ANY"), function (x, value) {
    if (is.Subtotal(value) | is.Heading(value)) {
        # if the value is not a list, make it into one, in case we got a bare
        # Subtotal() or Heading()
        value <- list(value)
    }
    if (any(unlist(
        Map(function (v) {
            !is.Subtotal(v) & !is.Heading(v)
        }, value)))) {
        halt("value must be a list of Subtotals, Headings, or both.")
    }

    inserts = Insertions(data = lapply(value, makeInsertion, var = x))

    # grab the old inserts so they are not deleted, but modify the list so we
    # don't have *too* many extras
    old_inserts <- transforms(x)$insertions
    if (!is.null(old_inserts)) {
        inserts <- modifyCats(old_inserts, inserts)
    }

    bd <- wrapEntity("view" = list("transform" = list("insertions" = inserts)))
    crPATCH(self(x), body=toJSON(bd))
    dropCache(cubeURL(x))
    return(invisible(x))
})

#' @rdname SubtotalsHeadings
#' @export
setMethod("subtotals<-", c("CrunchVariable", "NULL"), function (x, value) {
    # maintain any non-subtotal insertions
    old_inserts <- transforms(x)$insertions
    subtots <- subtotals(x)
    inserts <- setdiff(old_inserts, subtots)

    bd <- wrapEntity("view" = list("transform" = list("insertions" = inserts)))
    crPATCH(self(x), body=toJSON(bd))
    dropCache(cubeURL(x))
    return(invisible(x))
})

#' @rdname SubtotalsHeadings
#' @export
setMethod("makeInsertion", "Subtotal", function (x, var) {
    x <- callNextMethod(x, var)
    return(Insertion(anchor = x$after, name = x$name, `function` = "subtotal",
                     args = x$categories))
})

#' @rdname SubtotalsHeadings
#' @export
setMethod("makeInsertion", "Heading", function (x, var) {
    x <- callNextMethod(x, var)
    return(Insertion(anchor = x$after, name = x$name))
})

#' @rdname SubtotalsHeadings
#' @export
setMethod("makeInsertion", "ANY", function (x, var) {
    # map chars/nums to ids
    if (is.character(x$after)) {
        x$after <- ids(categories(var)[x$after])
    }

    if (!is.null(x$categories) && is.character(x$categories)) {
        x$categories <- as.list(ids(categories(var)[x$categories]))
    }

    return(x)
})