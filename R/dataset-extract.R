#' Subset datasets and extract variables
#'
#' @param x a CrunchDataset
#' @param i As with a \code{data.frame}, there are two cases: (1) if no other
#' arguments are supplied (i.e \code{x[i]}), \code{i} provides for
#' \code{as.list} extraction: columns of the dataset rather than rows. If
#' character, identifies variables to extract based on their aliases (by
#' default: set \code{options(crunch.namekey.dataset="name")} to use variable
#' names); if numeric or logical,
#' extracts variables accordingly. Alternatively, (2) if \code{j} is specified
#' (as either \code{x[i, j]} or \code{x[i,]}), \code{i} is an object of class
#' \code{CrunchLogicalExpr} that will define a subset of rows.
#' @param j columnar extraction, as described above
#' @param name columnar extraction for \code{$}
#' @param drop logical: autmatically simplify a 1-column Dataset to a Variable?
#' Default is FALSE, and the TRUE option is in fact not implemented.
#' @param ... additional arguments
#' @return \code{[} yields a Dataset; \code{[[} and \code{$} return a Variable
#' @name dataset-extract
#' @aliases dataset-extract
NULL

#' @rdname dataset-extract
#' @export
setMethod("[", c("CrunchDataset", "ANY"), function (x, i, ..., drop=FALSE) {
    x@variables <- variables(x)[i]
    return(x)
})

#' @rdname dataset-extract
#' @export
setMethod("[", c("CrunchDataset", "logical", "missing"), function (x, i, j, ..., drop=FALSE) {
    ## See [.data.frame: this is similar to how it distinguishes x[i] from x[i,]
    ## Ignoring the possibility of x[i, drop=TRUE]. x[i, drop=TRUE] should be x[[i]]
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
            i <- CrunchLogicalExpr(dataset_url=datasetReference(x),
                expression=.dispatchFilter(i))
            return(x[i,])
        } else {
            halt("Logical filter vector is length ", length(i),
                ", but dataset has ", nrow(x), " rows")
        }
    } else {
        ## If you reference a variable in a dataset that doesn't exist, you
        ## get NULL, and e.g. NULL == something becomes logical(0).
        ## That does awful things if you try to send to the server. So don't.
        halt("Invalid expression: ", deparseAndFlatten(match.call()$i))
    }
    return(x)
})
#' @rdname dataset-extract
#' @export
setMethod("[", c("CrunchDataset", "character"), function (x, i, ..., drop=FALSE) {
    w <- findVariablesInDataset(x, i)
    if (any(is.na(w))) {
        halt("Undefined columns selected: ", serialPaste(i[is.na(w)]))
    }
    x@variables <- allVariables(x)[w]
    return(x)
})

#' @rdname dataset-extract
#' @export
setMethod("[", c("CrunchDataset", "VariableGroup"), function (x, i, ..., drop=FALSE) {
    ## Do allVariables because Group/Order may contain refs to hidden vars
    x@variables <- allVariables(x)[i]
    return(x)
})

#' @rdname dataset-extract
#' @export
setMethod("[", c("CrunchDataset", "VariableOrder"), function (x, i, ..., drop=FALSE) {
    x@variables <- allVariables(x)[i]
    return(x)
})


#' @rdname dataset-extract
#' @export
setMethod("[", c("CrunchDataset", "missing", "ANY"), function (x, i, j, ..., drop=FALSE) {
    x[j]
})

.updateActiveFilter <- function (x, i, j, ..., drop=FALSE) {
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
            halt("In ", callstring, ", object and subsetting expression have different filter expressions")
        }
    }
    activeFilter(x) <- i
    return(x)
}

#' @rdname dataset-extract
#' @export
setMethod("[", c("CrunchDataset", "CrunchLogicalExpr", "missing"), .updateActiveFilter)

#' @rdname dataset-extract
#' @export
setMethod("[", c("CrunchDataset", "CrunchLogicalExpr", "ANY"), function (x, i, j, ..., drop=FALSE) {
    ## Do the filtering of rows, then cols
    x <- x[i,]
    return(x[j])
})

#' @rdname dataset-extract
#' @export
setMethod("subset", "CrunchDataset", function (x, ...) {
    x[..1,]
})

#' @rdname dataset-extract
#' @export
setMethod("[[", c("CrunchDataset", "ANY"), function (x, i, ..., drop=FALSE) {
    out <- variables(x)[[i]]
    if (!is.null(out)) {
        out <- CrunchVariable(out, filter=activeFilter(x))
    }
    return(out)
})
#' @rdname dataset-extract
#' @export
setMethod("[[", c("CrunchDataset", "character"), function (x, i, ..., drop=FALSE) {
    out <- allVariables(x)[[findVariablesInDataset(x, i)]]
    if (!is.null(out)) {
        out <- CrunchVariable(out, filter=activeFilter(x))
        if (tuple(out)$discarded) {
            warning("Variable ", alias(out), " is hidden", call.=FALSE)
        }
    }
    return(out)
})
#' @rdname dataset-extract
#' @export
setMethod("$", "CrunchDataset", function (x, name) x[[name]])


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
