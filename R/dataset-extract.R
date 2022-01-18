#' @rdname crunch-extract
#' @export
setMethod("[", c("CrunchDataset", "ANY"), function(x, i, ..., drop = FALSE) {
    x@variables <- variables(x)[i]
    return(x)
})

#' @rdname crunch-extract
#' @export
setMethod(
    "[", c("CrunchDataset", "logical", "missing"),
    function(x, i, j, ..., drop = FALSE) {
        ## See [.data.frame: this is similar to how it distinguishes x[i]
        ## from x[i,] Ignoring the possibility of x[i, drop=TRUE].
        ## x[i, drop=TRUE] should be x[[i]]
        if (nargs() == 2L) {
            ## x[i]. So subset the variables, list-wise
            x@variables <- variables(x)[i]
            return(x)
        }
        ## else: x[i,]
        ## TODO: generalize the logic and do similar for "numeric" method
        if (length(i)) {
            if (length(i) == 1) {
                if (isTRUE(i)) {
                    ## Keep all rows, so no filter
                    return(x)
                } else {
                    ## FALSE or NA. Reject it?
                    halt("Invalid logical filter: ", i)
                }
            } else if (length(i) == nrow(x)) {
                if (all(i)) {
                    ## Keep all rows, so no filter
                    return(x)
                }
                i <- CrunchLogicalExpr(
                    dataset_url = datasetReference(x),
                    expression = .dispatchFilter(i)
                )
                return(x[i, ])
            } else {
                halt(
                    "Logical filter vector is length ", length(i),
                    ", but dataset has ", nrow(x), " rows"
                )
            }
        } else {
            ## If you reference a variable in a dataset that doesn't exist, you
            ## get NULL, and e.g. NULL == something becomes logical(0).
            ## That does awful things if you try to send to the server. So don't.
            halt("Invalid expression: ", deparseAndFlatten(match.call()$i))
        }
        return(x)
    }
)
#' @rdname crunch-extract
#' @export
setMethod("[", c("CrunchDataset", "character"), function(x, i, ..., drop = FALSE) {
    w <- findVariablesInDataset(x, i)
    if (any(is.na(w))) {
        halt("Undefined columns selected: ", serialPaste(i[is.na(w)]))
    }
    x@variables <- allVariables(x)[w]
    return(x)
})

#' @rdname crunch-extract
#' @export
setMethod("[", c("CrunchDataset", "VariableGroup"), function(x, i, ..., drop = FALSE) {
    ## Do allVariables because Group/Order may contain refs to hidden vars
    x@variables <- allVariables(x)[i]
    return(x)
})

#' @rdname crunch-extract
#' @export
setMethod("[", c("CrunchDataset", "VariableOrder"), function(x, i, ..., drop = FALSE) {
    x@variables <- allVariables(x)[i]
    return(x)
})


#' @rdname crunch-extract
#' @export
setMethod(
    "[", c("CrunchDataset", "missing", "ANY"),
    function(x, i, j, ..., drop = FALSE) {
        x[j]
    }
)

.updateActiveFilter <- function(x, i, j, ..., drop = FALSE) {
    ## x[i] where i is CrunchLogicalExpr and x may already have an active filter
    f <- activeFilter(x)
    if (length(zcl(f))) {
        ## & together the expressions, as long as i has the same active filter
        ## as f is
        if (identical(zcl(f), zcl(activeFilter(i)))) {
            ## Ensure that they have the same filter on the objects, then & them
            activeFilter(i) <- activeFilter(f)
            i <- f & i
        } else {
            callstring <- deparseAndFlatten(tail(sys.calls(), 1)[[1]])
            halt(
                "In ", callstring, ", object and subsetting expression have ",
                "different filter expressions"
            )
        }
    }
    activeFilter(x) <- i
    return(x)
}

#' @rdname crunch-extract
#' @export
setMethod("[", c("CrunchDataset", "CrunchLogicalExpr", "missing"), .updateActiveFilter)

#' @rdname crunch-extract
#' @export
setMethod(
    "[", c("CrunchDataset", "CrunchLogicalExpr", "ANY"),
    function(x, i, j, ..., drop = FALSE) {
        ## Do the filtering of rows, then cols
        x <- x[i, ]
        return(x[j])
    }
)

#' @rdname crunch-extract
#' @export
setMethod("subset", "CrunchDataset", function(x, ...) {
    x[..1, ]
})

#' @rdname crunch-extract
#' @export
setMethod("[[", c("CrunchDataset", "ANY"), function(x, i, ..., drop = FALSE) {
    out <- variables(x)[[i]]
    if (!is.null(out)) {
        out <- CrunchVariable(out, filter = activeFilter(x))
    }
    return(out)
})
#' @rdname crunch-extract
#' @export
setMethod("[[", c("CrunchDataset", "character"), function(x, i, ..., drop = FALSE) {
    out <- allVariables(x)[[findVariablesInDataset(x, i)]]
    if (!is.null(out)) {
        out <- CrunchVariable(out, filter = activeFilter(x))
        if (alias(out) %in% hiddenVariables(x, "alias") && getOption("crunch.warn.hidden", TRUE)) {
            warning("Variable ", alias(out), " is hidden", call. = FALSE)
        }
        if (alias(out) %in% privateVariables(x, "alias") && getOption("crunch.warn.private", TRUE)) { # nolint
            warning("Variable ", alias(out), " is private", call. = FALSE)
        }
    }
    return(out)
})
#' @rdname crunch-extract
#' @export
setMethod("$", "CrunchDataset", function(x, name) x[[name]])


findVariablesInDataset <- function(x, i) {
    allvars <- allVariables(x)
    ## Handle "namekey", which should be deprecated
    if (getOption("crunch.namekey.dataset", "alias") == "name") {
        alt <- names(allvars)
    } else {
        alt <- aliases(allvars)
    }

    return(whichNameOrURL(allvars, i, alt))
}
