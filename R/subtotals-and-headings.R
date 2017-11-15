#' Subtotals and headings
#'
#' Subtotals and headings for categorical Variables and
#' CrunchCubes. These are especially useful for making aggregates across
#' multiple categories (sometimes referred to as _nets_, _top box_, or
#' _top 2 box_).
#'
#' To see the subtotals or headings set for a variable, use `subtotals(variable)`
#'
#' Subtotals and headings can be added either by passing a list of `Subtotal`s
#' or `Heading`s, or they can be added one at a time by passing `Subtotal` or
#' `Heading` to `subtotals(variable)` alone.
#'
#' Adding subtotals or headings is additive; meaning that subtotals or headings
#' that are already set on the variable are not removed when new subtotals or
#' headings are added. To remove all subtotals and headings, set
#' `subtotals(variable)` to `NULL`.
#'
#' To get an array of just the subtotal rows from a CrunchCube, use the function
#' `subtotalArray(CrunchCube)`.
#'
#' @param x either a variable or CrunchCube object to add or get subtotal
#' transforms for
#' @param data For the constructor functions `Subtotal` and
#' `Heading`, you can either pass in attributes via `...` or you
#' can create the objects with a fully defined `list` representation of
#' the objects via the `data` argument.
#' @param ... additional arguments to `[`, ignored
#' @param value For `[<-`, the replacement Subtotal to insert
#' @param var the variable to use to make `Insertions` from a `Subtotal` object
#'
#' @examples
#' \dontrun{
#' # given a variable ds$opinion, with categories: Strongly Agree, Somewhat
#' # Agree, Neither Agree nor Disagree, Somewhat Disagree, and Strongly Disagree,
#' # to make two subtotals for Agree and Disagree:
#' subtotals(ds$opinion) <- list(
#'     Subtotal(name = "Agree",
#'              categories = c("Strongly Agree", "Somewhat Agree"),
#'              after = "Somewhat Agree"),
#'     Subtotal(name = "Disagree",
#'              categories = c("Strongly Disagree", "Somewhat Disagree"),
#'              after = "Strongly Disagree")
#' )
#'
#' # headings can also be added:
#' subtotals(ds$opinion) <- Heading(name = "All opinions", after = 0)
#'
#' # to see the subtotals and headings associated with a variable
#' subtotals(ds$opinion)
#' #        anchor         name     func    args
#' # 1      2        Agree subtotal 1 and 2
#' # 2      4     Disagree subtotal 4 and 5
#' # 3      0 All opinions     <NA>      NA
#'
#' # when you use a variable with subtotals and headings in a cube, you see them
#' # by default
#' opinion_cube <- crtabs(~opinion, ds)
#' opinion_cube
#' #               All opinions   
#' #             Strongly Agree 23
#' #             Somewhat Agree 24
#' #                      Agree 47
#' # Neither Agree nor Disagree 18
#' #          Strongly Disagree 19
#' #                   Disagree 35
#' #          Somewhat Disagree 16
#' 
#'
#' # to get just the subtotals,
#' subtotalArray(opinion_cube)
#' #    Agree Disagree 
#' #       47       35 
#'
#' # to remove all subtotals and headings
#' subtotals(ds$opinion) <- NULL
#' crtabs(~opinion, ds)
#' #             Strongly Agree 23
#' #             Somewhat Agree 24
#' # Neither Agree nor Disagree 18
#' #          Strongly Disagree 19
#' #          Somewhat Disagree 16
#' 
#' 
#' # if you want to temporarily remove subtotals and headings, you can with `noTransforms`
#' noTransforms(crtabs(~opinion, ds))
#' #             Strongly Agree             Somewhat Agree Neither Agree nor Disagree 
#' #                         23                         24                         18 
#' #          Strongly Disagree          Somewhat Disagree 
#' #                         19                         16 
#' }
#'
#' @name SubtotalsHeadings
#' @aliases subtotals subtotals<- makeInsertion
NULL

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

init.Subtotal <- function (.Object, ...) {
    .Object <- callNextMethod()
    # unlist, to flatten user inputs like list(1:2)
    .Object$categories <- unlist(.Object$categories)
    return(.Object)
}
setMethod("initialize", "Subtotal", init.Subtotal)

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
    
    if (is.null(transforms(x)) || is.null(inserts)) {
        return(NULL)
    }

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