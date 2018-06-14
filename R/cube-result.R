setMethod("initialize", "CrunchCube", function (.Object, ...) {
    .Object <- callNextMethod(.Object, ...)
    ## Fill in these reshaped values if loading an API response
    if (!length(.Object@dims)) .Object@dims <- cubeDims(.Object)
    if (!length(.Object@arrays)) {
        ## Get the "measures" from the response
        m <- .Object$result$measures
        ## Add the "bases", which aren't included in "measures"
        m[[".unweighted_counts"]] <- list(data=.Object$result$counts)
        ## Transform the flat arrays into N-d arrays for easier use
        .Object@arrays <- lapply(m, cToA, dims=.Object@dims)
    }
    return(.Object)
})

#' Modify cube missing behavior
#'
#' By default, CrunchCubes do not show entries for missing categories. You can
#' include missing values in a `cube` with `showMissing(cube)` and hide them
#' again with `hideMissing(cube)`.
#'
#' @param cube a CrunchCube
#' @name cube-missingness
#' @aliases showMissing hideMissing showIfAny
NULL

#' @rdname cube-missingness
#' @export
setMethod("showMissing", "CrunchCube", function(cube) setCubeNA(cube, "always"))

#' @rdname cube-missingness
#' @export
setMethod("hideMissing", "CrunchCube", function(cube) setCubeNA(cube, "no"))

#' @rdname cube-missingness
#' @export
setMethod("showIfAny", "CrunchCube", function(cube) setCubeNA(cube, "ifany"))

setCubeNA <- function(cube, value = c("always", "no", "ifany")){
    value <- match.arg(value)
    cube@useNA <- value
    return(cube)
}

#' @rdname cube-methods
#' @export
setMethod("dim", "CrunchCube", function (x) dim(as.array(x)))

# ---- Cube To Array ----

#' @rdname cube-methods
#' @export
setMethod("dimnames", "CrunchCube", function (x) dimnames(dimensions(x)))

cToA <- function (x, dims) {
    ## Just make an array from the cube "measure's" data. Nothing else.
    ## This function takes a flat array from the JSON response and shapes it
    ## into an N-dimensional `array` class object. It does not prune this array
    ## in any way--any extra values that don't ever get displayed to users, and
    ## any "missing" elements, which may or may not be displayed--remain.
    ## Those extra values are sometimes needed to compute the correct margins
    ## for percentaging, so they are kept at this point and removed before
    ## display. But having the data in an array shape now will make those later
    ## calculations more natural to do.
    d <- unlist(x$data)
    ## Identify missing values
    nas <- names(d) %in% "?"
    d[nas & d == -8] <- NaN
    d[nas & d != -8] <- NA

    dimsizes <- dim(dims)
    ndims <- length(dims)
    if (ndims == 0) {
        ## It's a scalar. Just return it
        out <- d
    } else {
        if (ndims > 1) {
            ## Cube arrays come in row-col-etc. order, not column-major.
            ## Keep the labels right here, then aperm the array back to order
            dimsizes <- rev(dimsizes)
        }
        out <- array(d, dim=dimsizes)
        if (ndims > 1) {
            ap <- seq_len(ndims)
            ap <- rev(ap)
            out <- aperm(out, ap)
        }
        dimnames(out) <- dimnames(dims)
    }
    ## Stick any variable metadata we have in here as an attribute so that
    ## `measures()` and `variables()` can access it
    attr(out, "variable") <- cubeVarReferences(x$metadata)
    return(out)
}

cubeToArray <- function (x, measure=1) {
    ## This is the function behind the "as.array" method, as well as what
    ## "bases" does with the ".unweighted_counts". It evaluates all of the logic
    ## that takes the array in the cube response and selects the slices of that
    ## that the human user thinks they're dealing with. This logic includes
    ## two main things:
    ##
    ## (1) Missingness can be optionally included/shown, based on the
    ## "useNA" slot/argument to `crtabs`. And since one value for "useNA" is
    ## "ifany", whether or not NAs are shown may depend on the values in the
    ## table, not just the category/element metadata.
    ## (2) Multiple response. This variable type is presented to users as if
    ## it were categorical, but its data structure isn't, and thus its
    ## representation in the cube response is more complex. We need that
    ## complexity so that we know how to compute percentages correctly, but
    ## here we need to dump it. MR variables return as two
    ## dimensions, like a categorical array. The "category" dimension has been
    ## reduced to special "Selected", "Other", and "No Data" categories.
    ## For reducing the display of the result back to the single dimension we
    ## think of the data, we take the "Selected" slice from the categories
    ## dimension.
    out <- x@arrays[[measure]]
    ## Remove the "variables" metadata stuck in there
    attr(out, "variable") <- NULL
    ## If "out" is just a scalar, skip this
    if (is.array(out)) {
        ## First, take the "Selected" slices, if any
        out <- takeSelectedDimensions(out, x@dims)
        ## Then, figure out which NA values to keep/drop/etc.
        keep.these <- evalUseNA(out, dimensions(x), x@useNA)
        out <- subsetCubeArray(out, keep.these)
    } else if (length(x@dims) == 1) {
        missing <- x@dims[[1]]$missing
        if (x@useNA == "no") {
            out <- out[!missing]
        } else if (x@useNA == "ifany") {
            out <- out[out > 0 | !missing]
        }
    }
    return(out)
}

takeSelectedDimensions <- function (x, dims) {
    ## This function handles the "as_selected" multiple response feature,
    ## dropping the slices other than "Selected". If no "as_selected" MR
    ## variables are present in the cube dimensions, this function does nothing.
    selecteds <- is.selectedDimension(dims)
    if (any(selecteds)) {
        drops <- lapply(selecteds, function (s) {
            ## For "Selected" dimensions, we only want to return "Selected", the 1st element
            ## TODO: Don't assume "selected" is position 1; consider an is.selected attr/vector
            if (s) {
                return(1L)
            } else {
                ## Otherwise keep all
                return(TRUE)
            }
        })
        x <- subsetCubeArray(x, drops, selected_dims=selecteds)
    }
    return(x)
}

subsetCubeArray <- function (array, bools, drop=FALSE, selected_dims=FALSE) {
    ## This is a convenience method around "[" for subsetting arrays,
    ## given a named list of logicals corresponding to the dims.
    ## It has some special handling for "Selected" MR slicing, so that we can
    ## "drop" that dimension but not necessarily "drop" any other dimensions
    ## that are length-1.
    re_shape <- any(selected_dims) &&
        !drop && length(selected_dims) == length(dim(array))
    if (re_shape) {
        ## subset with drop=TRUE to just keep the selected slice(s), then wrap in
        ## array to set dims correctly (so that we don't accidentally drop some other
        ## true length-1 dimension)
        drop <- TRUE
        newdim <- dim(array)[!selected_dims]
        newdimnames <- dimnames(array)[!selected_dims]
    }
    out <- do.call("[", c(list(x=array, drop=drop), bools))
    if (re_shape) {
        out <- array(out, dim=newdim, dimnames=newdimnames)
    }
    return(out)
}

evalUseNA <- function (data, dims, useNA) {
    ## Return dimnames-shaped list of logical vectors indicating which
    ## values should be kept, according to the @useNA parameter

    ## Figure out which dims are non-zero
    margin.counts <- lapply(seq_along(dim(data)),
        function (i) margin.table(data, i))
    keep.these <- mapply(keepWithNA,
        dimension=dims,
        marginal=margin.counts,
        MoreArgs=list(useNA=useNA),
        SIMPLIFY=FALSE,
        USE.NAMES=FALSE)
    names(keep.these) <- names(dims)
    return(keep.these)
}

keepWithNA <- function (dimension, marginal, useNA) {
    ## Returns logicals of which rows/cols/etc. should be kept
    if (useNA == "always") {
        out <- rep(TRUE, length(dimension$missing))
    } else {
        ## !always means either drop missing always, or only keep if there are any
        out <- !dimension$missing
        if (useNA == "ifany") {
            ## Compare against "marginal", the counts, to know which missing
            ## elements have "any"
            out <- out | marginal > 0
        }
    }
    # add names, so we know which categories are being kept
    names(out) <- dimension$name
    return(out)
}

cubeMarginTable <- function (x, margin=NULL, measure=1) {
    ## Given a CrunchCube, get the right margin table for percentaging
    ##
    ## This is the function that `margin.table` calls internally, and like
    ## `cubeToArray` (for `as.array`), it manages the complexity of how we
    ## handle missing data and especially multiple response.
    data <- x@arrays[[measure]]
    dims <- x@dims
    dimnames(data) <- dimnames(dims)
    missings <- is.na(dims)

    ## Check "margin" against number of (non-"selected" invisible MR) dims
    selecteds <- is.selectedDimension(dims)
    check_margins(margin, selecteds)

    which_selected <- which(selecteds)
    margin_map <- which(!selecteds)
    ## Caution: due to the "as_selected" MR behavior, our "real" cube may have
    ## higher dimensionality than the user thinks. E.g. if we have
    ## MR x categorical, the user thinks there are 2 dimensions in the cube,
    ## but there are 3 dimensions in the array we store (MR subvars,
    ## MR "categories" (selected/not/missing), and Cat var). So if the user
    ## says they want margin.table(cube, 2), we have to know that that "2"
    ## translates to dim 3 in the "real" array. The `margin_map` translates
    ## user dims to "real" dims.
    mapped_margins <- margin_map[margin]
    
    drop_na <- x@useNA == "no"
    
    ## This is the core of the function, in which we select the subset of the
    ## "real" cube that we want to aggregate to generate the margin table.
    ## The result of this lapply is a dimnames-shaped list of logical vectors
    ## (like what `evalUseNA` returns).
    args <- lapply(seq_along(missings), function (i) {
        ## Iterating over each dimension i,
        m <- missings[[i]]
        ## Start with all TRUE
        out <- rep(TRUE, length(m))
        ## First, check whether "i" is a "Selection" (pseudo-)dimension
        if (i %in% which_selected) {
            ## If so, switch behavior based on whether the user has requested
            ## a margin.table on the MR variable "margin" that corresponds to
            ## this "selection" dim. "Selections" are always immediately
            ## following the MR variable, so see if i - 1 is in the requested
            ## "margin":
            if ((i - 1) %in% mapped_margins) {
                ## This is the "Selection" dimension that corresponds to the
                ## previous "real" dim
                ## TODO: Don't assume "selected" is position 1; consider an is.selected attr/vector
                out <- 1 ## Just keep "Selected"
            } else if (drop_na) {
                ## Otherwise, check if we're only keeping non-missing entries,
                ## and filter them accordingly.
                out <- !m
            }
            ## Next, for non-selection dimensions, it matters if "i" is in the
            ## user's margin selection. The default, as we said up front, is keep
            ## all, but not if we're sweeping this margin.
        } else if (drop_na && !(i %in% mapped_margins)) {
            ## Not multiple response. Exclude missings if we should
            out <- !m
        }

        return(out)
    })
    names(args) <- names(missings)
    data <- subsetCubeArray(data, args)
    ## Now we have data in a reasonable shape that R's native methods can
    ## handle.

    ## One last MR trick. We have to figure out which margins to actually keep.
    ## Because the "as_selected" MR variables are essentially stacked individual
    ## variables, with independent "base sizes", if they're present in the cube,
    ## they're always present (each subvariable) in the resulting margin table.
    ## That is, a MR x Cat cube, requesting margin 2, actually returns a 2-D
    ## margin table, and not a 1-D margin table like you might expect, because
    ## each subvariable in the MR has a separate "valid" count associated.
    ## It's mind-bending, I know. But we bend our minds so our users don't have
    ## to as much.
    mt_margins <- as_selected_margins(margin, selecteds)
    ## OK. Now we have an array of data and translated margins. We can call the
    ## base `margin.table` method with those.
    mt <- margin.table(data, mt_margins)
    ## Finally, drop missings from the result. Could we do this in one step,
    ## building this into the initial `lapply`? Maybe we can now that we've
    ## removed "selected_array"!
    ## TODO: try to do that.
    keep.these <- evalUseNA(mt, dims[mt_margins], x@useNA)
    out <- subsetCubeArray(mt, keep.these)
    
    # only attempt to apply a transform if the margin is 1 rows for now.
    if (!is.null(margin) && margin == 1) {
        out <- applyTransforms(x, array = out)
    }
    return(out)
}


#' Check validity of margins
#'
#' A helper function to check if margins supplied are compatible with the
#' dimensions
#'
#' @param selecteds which dimensions are selected
#' @param margin the margin(s) being specified
#'
#' @return None
#' @export
#'
#' @keywords internal
check_margins <- function (margin, selecteds) {
    if (!is.null(margin) && max(margin) > sum(!selecteds)) {
        ## Validate the input and give a useful error message.
        ## base::margin.table says:
        ## "Error in if (d2 == 0L) { : missing value where TRUE/FALSE needed"
        ## which is terrible.
        halt("Margin ", max(margin), " exceeds Cube's number of dimensions (",
             sum(!selecteds), ")")
    }
}

as_selected_margins <- function (margin, selecteds, before=TRUE) {
    ## If there are "Selection" dimensions, we always want to include their
    ## partner (position - 1) in the margin table dimensions
    ## margin is always the "real" full cube dimensions even if before=TRUE
    if (!any(selecteds)) {
        ## If there aren't any, no-op
        return(margin)
    }
    which_selected <- which(selecteds)
    if (before) {
        ## "before" means we're returning margins of the "real" cube that
        ## includes the selection dimensions in them.
        margin <- which(!selecteds)[margin]
        mr_margins <- which_selected - 1
    } else {
        ## "after" is after dropping the selection dimensions, so we need to
        ## subtract more than one for each subsiquent MR encountered
        mr_margins <- which_selected - seq_along(which_selected)
    }

    return(sort(union(margin, mr_margins)))
}

#' Work with CrunchCubes, MultitableResults, and TabBookResults
#'
#' These functions provide an interface like [base::margin.table()] and
#' [base::prop.table()] for the CrunchCube object. CrunchCubes contain richer
#' metadata than standard R `array` objects, and they also conceal certain
#' complexity in the data structures from the user. In particular,
#' multiple-response variables are generally represented as single dimensions in
#' result tables, but in the actual data, they may comprise two dimensions.
#' These methods understand the subtleties in the Crunch data types and
#' correctly compute margins and percentages off of them.
#'
#' These functions also generalize to MultitableResults and TabBookResults,
#' which are returned from a [tabBook()] request. When called on one of those
#' objects, they effectively apply over each CrunchCube contained in them.
#'
#' `bases` is an additional method for CrunchCubes. When making weighted
#' requests, `bases` allows you to access the unweighted counts for every cell
#' in the resulting table (array). The `bases` function takes a "margin"
#' argument to work like `margin.table`, or with `margin=0` gives all cell
#' counts.
#'
#' `collapse.dimensions` returns a cube that collapses all dimensions but the one
#' given in `margin`. This is useful if you want to get counts that are
#' equivalent to a univariate cube from a multivariate cube. For example
#' `collapse.dimensions(crtabs(~ fruit + pets, ds), 1)` will be equal to 
#' `crtabs(~ fruit, ds)`.
#'
#' @param x a CrunchCube
#' @param margin index, or vector of indices to generate margin for. See
#'   [base::prop.table()]. `bases()` accepts `0` as an additional valid value
#'   for `margin`, which yields the unweighted counts for the query.
#' @param digits For `round`, the number of decimal places to round to. See
#'   [base::round()]
#'
#' @return When called on CrunchCubes, these functions return an `array`.
#'   Calling prop.table on a MultitableResult returns a list of prop.tables of
#'   the CrunchCubes it contains. Likewise, prop.table on a TabBookResult
#'   returns a list of lists of prop.tables.
#' @name cube-computing
#' @aliases cube-computing margin.table prop.table bases round collapse.dimensions
#' @seealso [base::margin.table()] [base::prop.table()]
NULL

#' @rdname cube-computing
#' @export
setMethod("margin.table", "CrunchCube", function (x, margin=NULL) {
    cubeMarginTable(x, margin)
})

#' @rdname cube-computing
#' @export
setMethod("collapse.dimensions", "CrunchCube", function (x, margin=NULL) {
    ## Check "margin" against number of (non-"selected" invisible MR) dims
    selecteds <- is.selectedDimension(x@dims)
    check_margins(margin, selecteds)
    margins_to_collapse <- user2real(margin, cube = x)

    margins_to_keep <- setdiff(seq_along(x@dims), margins_to_collapse)
    
    # if there are any _items in the margin to collapse, be wary!
    has_items <- endsWith(getDimTypes(x)[margins_to_collapse], "_items")

    out <- x
    out@arrays[] <- lapply(out@arrays, function(arr){
        off_margins <- margins_to_keep
        if (any(has_items)) {
            # mean across any items dimension
            items_to_collapse <- margins_to_collapse[has_items]
            off_margins <- sort(c(off_margins, items_to_collapse))
        }
        
        array <- apply(arr, off_margins, sum)
        
        if (any(has_items)) {
            # mean across any items dimension
            result_dim <- seq_along(dim(array))
            array <- apply(array, setdiff(result_dim, which(off_margins %in% items_to_collapse)), mean)
        }
        return(array)
    })

    # we needed to include the selected dim of MRs in off_margins above, but
    # when subsetting dimensions we definitely do not want them to be included.
    out@dims <- out@dims[margins_to_keep]
    out$query$dimensions <- out$query$dimensions[margins_to_keep]
    out$result$dimensions <- out$result$dimensions[margins_to_keep]
    # names???
    
    # subset the missing dimensions to try and get the n missing correct
    unweighted_counts <- as.array(out@arrays$.unweighted_counts)
    missings <- lapply(
        evalUseNA(unweighted_counts, out@dims, "no"),
        function(x) {
            if (all(x)) {
                # if all are TRUE, return all TRUE so we don't eliminate all
                # cells
                return(x)
            } else {
                # otherwise, return the opposite because we only want
                # missings
                return(!x)
            }
        })
    all_missings <- subsetCubeArray(unweighted_counts, missings)

    # sum across non-item dimensions, mean across item dimensions 
    out_dims <- seq_along(out@dims)
    out_other <- out_dims[!endsWith(getDimTypes(out), "_items")]

    all_missings <- apply(all_missings, out_other, mean)
    all_missings <- sum(all_missings)
    out$result$missing <- all_missings 
                                                            
    # update measures
    ap <- rev(out_dims)
    out$result$measures$count$data <-  as.list(aperm(out@arrays$count, ap))
    out$result$measures$count$n_missing <- out$result$missing
    out$result$counts <-  as.list(aperm(out@arrays$.unweighted_counts, ap))

    return(out)
})


user2real <- function(margin, dimTypes = getDimTypes(cube), cube) {
    if (is.null(margin)) { 
        # If margin is null, return null
        return(NULL)
    }
    full_margins <- seq_along(dimTypes)
    narrow_margins <- seq_along(dimTypes[dimTypes != "mr_selections"])
    which_selected <- which(dimTypes == "mr_selections")
    # things?
    mr_margins <- which_selected - seq_along(which_selected)
    margin_map <- sort(c(narrow_margins, mr_margins))
    return(which(margin_map %in% margin))
}

#' @export
as.array.CrunchCube <- function (x, ...) cubeToArray(x, ...)

#' @rdname cube-computing
#' @export
setMethod("prop.table", "CrunchCube", function (x, margin=NULL) {
    out <- as.array(x)
    out <- applyTransforms(x, array = out)
    marg <- margin.table(x, margin)
    actual_margin <- as_selected_margins(margin, is.selectedDimension(x@dims),
        before=FALSE)
    # Check if there are any actual_margins and if the dims are identical, we
    # don't need to sweep, and if we are MRxMR we can't sweep.
    if (length(actual_margin) & !identical(dim(out), dim(marg))) {
        out <- sweep(out, actual_margin, marg, "/", check.margin=FALSE)
    } else {
        ## Don't just divide by sum(out) like the default does.
        ## cubeMarginTable handles missingness, any/none, etc.
        out <- out/marg
    }

    return(out)
})

#' @rdname cube-computing
#' @export
setMethod("round", "CrunchCube", function (x, digits=0) {
    round(as.array(x), digits)
})

#' @rdname cube-computing
#' @export
setMethod("bases", "CrunchCube", function (x, margin=NULL) {
    if (length(margin) == 1 && margin == 0) {
        ## Unlike margin.table. This just returns the "bases", without reducing
        return(cubeToArray(x, ".unweighted_counts"))
    } else if (length(dimensions(x)) == 0) {
        ## N dims == 0 is for univariate stats
        if (!is.null(margin)) {
            halt("Margin ", max(margin),
                " exceeds Cube's number of dimensions (0)")
        }
        return(cubeToArray(x, ".unweighted_counts"))
    } else {
        return(cubeMarginTable(x, margin, measure=".unweighted_counts"))
    }
})
