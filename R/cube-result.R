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

#' @rdname cube-methods
#' @export

setMethod("[", "CrunchCube", function (x, i, j, ..., drop = TRUE) {

    subset <- eval(substitute(alist(i, j, ...)))
    subset <- replaceMissingWithTRUE(subset)
    translated_subset <- translateCubeIndex(x, subset, drop)
    #translated_subset <- skipMissingCategories(x, translated_subset)

    out <- x
    out@arrays$count <- subsetByList(out@arrays$count, translated_subset, drop)
    out@arrays$.unweighted_counts <- subsetByList(out@arrays$.unweighted_counts,
        translated_subset, drop)

    out@dims[] <- mapply(subsetArrayDimension,
        dim = x@dims,
        idx = translated_subset,
        SIMPLIFY = FALSE)

    if (drop) {

        keep_args <- vapply(translated_subset, function(a) {
            length(a) != 1 || isTRUE(a)}, FUN.VALUE = logical(1))
        #keep_dims <- names(dimnames(x@arrays$count))[keep_args]
        #out@dims <- out@dims[(names(out@dims) %in% keep_dims)]
        out@dims <- out@dims[keep_args]
    }
    return(out)
})

#' Replace missing elements with TRUE
#'
#' When subsetting missingness stands in for selecting all of the elements of that
#' dimension. This gets tricky when you are selecting n dimensions and so need to
#' capture the dimensions with `...`.  This function checks if an element of a list
#' is missing and replaces that element with `TRUE` in order to handle this case.
#'
#' @param l a list
#' @return a list
replaceMissingWithTRUE <- function(l){
    out <- lapply(l, function(x){
        if (is.symbol(x)) {
            x <- tryCatch(eval(x), error = function(c){
                msg <- conditionMessage(c)
                if (msg == "argument is missing, with no default") {
                    return(TRUE)
                } else {
                    stop(c)
                }
            })
        }
        return(eval(x))
    })
    return(out)
}

subsetByList <- function(arr, arglist, drop){
    #suppresses "named arguments other than drop are discouraged" warning
    suppressWarnings(
        do.call('[', c(list(x = arr, drop = drop), arglist))
        )
}

subsetArrayDimension <- function(dim, idx){
    dim$any.or.none <- dim$any.or.none[idx]
    dim$missing <- dim$missing[idx]
    dim$name <- dim$name[idx]
    return(dim)
}

#' Translate user facing cube subset to programmatic cube subset
#'
#' Cubes that include multiple response variables create a special kind of complexity.
#' Multiple response variables are actually 2d arrays with the selections along one dimension (`cat`, `dog`, `fish``)
#' and the selection status along the second dimension (`selected`, `not_selected`).
#' When an MR variable is crossed with a categorical variable it creates a 3d array
#' with the categorical variable's categories along one dimension and the MR dimensions
#' on the other two.
#'
#' The complexity is that while the real MR cube includes two dimensions per MR,
#' we only show the user one dimension per MR which represents only the `selected` cases.
#' This means that every cube has two different representations, the low dimensional user
#' cube, and the higher dimensional programmatic cube. This function translates user cube subsets
#' into the higher dimensional subset. In the case above, the user would see a 2d cube
#' and subset it with `user_cube[1:2, 1:2]` in order to subset the programmatic cube
#' we need to translate this to `prog_cube[1:2, 1:2, ]` in order to select the right variables
#' of the high dimensional cube.
#' @param x  a Crunch Cube
#' @param subset a list
#' @param drop whether to drop unnecessary dimensions.
#'
#' @return a list
translateCubeIndex <- function(x, subset, drop) {
    user_names <- names(dimnames(as.array(x))) #the user facing cube
    if (length(subset) != length(user_names)) {
        halt("You supplied ",
            length(subset),
            " dimensions to subset a ",
            length(user_names),
            " dimensional cube.")
    }
    prog_names <- names(dimnames(x@arrays$count)) #the higher dimensional internal cube
    if (length(prog_names) == length(user_names)) {
        #no MR variables so no need to translate the subset
        return(subset)
    }
    out <- as.list(rep(TRUE, length(prog_names)))
    out[match(user_names, prog_names)] <- subset
    vars <- as.data.frame(variables(x))
    mr_vars <- vars$alias[vars$type == "subvariable_items"]
    if (drop) {
        for (i in seq_along(prog_names)) {
            if (i == 1) {
                next
            }
            if (prog_names[i] %in% mr_vars &&             # Check if MR variable
                    prog_names[i] == prog_names[i - 1] && # Check if MR selection variable
                    length(out[[i - 1]]) == 1 &&          # Check if MR indicator variable is a single number
                    !isTRUE(out[[i - 1]])) {
                out[i] <- 1 #assign subset value to "Selected" along the mr_selection dimension
            }
        }
    }
    return(out)
}

skipMissingCategories <- function(cube, subset){
    missing <- lapply(cube@dims, function(x) x$missing)
    mapply(function(miss, sub){
        if (length)
        out <- miss
        out[!miss][sub] <- rep(TRUE, length(sub))
        return(out)
    }, miss = missing, sub = subset)
}


#' @rdname cube-methods
#' @export
setMethod("dim", "CrunchCube", function (x) dim(dimensions(x)))

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
    ## here we need to dump it. Multiple response (MR) extra features in the
    ## cube come in two forms:
    ## (a) For the "selected_array" method of computing (legacy), MR variables
    ## return as one dimension in the output but have extra special
    ## pseudo-categories "__any__" and "__none__", which tell you the rows that
    ## have valid values (even if no responses are "selected"). This is what
    ## we use for determining the denominator for percentage calculations, but
    ## they are suppressed from display.
    ## (b) For the "as_selected" method (new), MR variables return as two
    ## dimensions, like a categorical array. The "category" dimension has been
    ## reduced to special "Selected", "Not Selected", and "No Data" categories.
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

    ## Always drop __any__ and __none__, regardless of other missingness
    out <- !dimension$any.or.none

    if (useNA != "always") {
        ## !always means either drop missing always, or only keep if there are any
        valid.cats <- !dimension$missing
        if (useNA == "ifany") {
            ## Compare against "marginal", the counts, to know which missing
            ## elements have "any"
            valid.cats <- valid.cats | marginal > 0
        }
        ## But still drop __any__ or __none__
        out <- valid.cats & out
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
    aon <- anyOrNone(dims)
    missings <- is.na(dims)

    ## Check "margin" against number of (non-"selected" invisible MR) dims
    selecteds <- is.selectedDimension(dims)
    if (!is.null(margin) && max(margin) > sum(!selecteds)) {
        ## Validate the input and give a useful error message.
        ## base::margin.table says:
        ## "Error in if (d2 == 0L) { : missing value where TRUE/FALSE needed"
        ## which is terrible.
        halt("Margin ", max(margin), " exceeds Cube's number of dimensions (",
            sum(!selecteds), ")")
    }

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
    ## If multiple response, sum __any__ + __none__ (and missing, if included)
    ## Else, sum all
    args <- lapply(seq_along(aon), function (i) {
        ## Iterating over each dimension i,
        a <- aon[[i]]
        ## Start with all TRUE
        out <- rep(TRUE, length(a))
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
                out <- 1 ## Just keep "Selected"
            } else if (drop_na) {
                ## Otherwise, check if we're only keeping non-missing entries,
                ## and filter them accordingly.
                out <- !missings[[i]]
            }
        ## Next, for non-selection dimensions, it matters if "i" is in the
        ## user's margin selection. The default, as we said up front, is keep
        ## all, but not if we're sweeping this margin.
        } else if (!(i %in% mapped_margins)) {
            if (any(a)) {
                ## Any "any-or-none" means we have the other form of MR query.
                ## If this is not a margin we're requesting a table for, that
                ## means we just want the valid counts for this dimension. That
                ## means "any selected" + "none selected", or the value of
                ## `anyOrNone(dim)`
                out <- a
                ## Add missings if not "no"
                if (!drop_na) {
                    out <- out | missings[[i]]
                }
            } else if (drop_na) {
                ## Not multiple response. Exclude missings if we should
                out <- !missings[[i]]
            }
        }

        return(out)
    })
    names(args) <- names(aon)
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
    ## building this into the initial `lapply`? Maybe, but I think there's some
    ## combination of "selected_array" multiple response with useNA=="ifany"
    ## for which that would do the wrong thing.
    keep.these <- evalUseNA(mt, dims[mt_margins], x@useNA)
    out <- subsetCubeArray(mt, keep.these)

    # only attempt to apply a transform if the margin is 1 rows for now.
    if (!is.null(margin) && margin == 1) {
        out <- applyTransforms(x, array = out)
    }
    return(out)
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
#' These functions provide an interface like [base::margin.table()]
#' and [base::prop.table()] for the CrunchCube object. CrunchCubes contain
#' richer metadata than standard R `array` objects, and they also conceal
#' certain complexity in the data structures from the user. In particular,
#' multiple-response variables are generally represented as single dimensions
#' in result tables, but in the actual data, they may comprise two dimensions.
#' These methods understand the subtleties in the Crunch data types and
#' correctly compute margins and percentages off of them.
#'
#' These functions also generalize to MultitableResults and TabBookResults,
#' which are returned from a [tabBook()] request. When called on one of those
#' objects, they effectively apply over each CrunchCube contained in them.
#'
#' `bases` is an additional method for CrunchCubes. When making weighted
#' requests, `bases` allows you to access the unweighted counts for every
#' cell in the resulting table (array). The `bases` function takes a
#' "margin" argument to work like `margin.table`, or with `margin=0`
#' gives all cell counts.
#'
#' @param x a CrunchCube
#' @param margin index, or vector of indices to generate margin for. See
#' [base::prop.table()]. `bases()` accepts `0` as an additional valid
#' value for `margin`, which yields the unweighted counts for the
#' query.
#' @param digits For `round`, the number of decimal places to round to. See
#' [base::round()]
#'
#' @return When called on CrunchCubes, these functions return an `array`.
#' Calling prop.table on
#' a MultitableResult returns a list of prop.tables of the CrunchCubes it
#' contains. Likewise, prop.table on a TabBookResult returns a list of lists of
#' prop.tables.
#' @name cube-computing
#' @aliases cube-computing margin.table prop.table bases round
#' @seealso [base::margin.table()] [base::prop.table()]
NULL

#' @rdname cube-computing
#' @export
setMethod("margin.table", "CrunchCube", function (x, margin=NULL) {
    cubeMarginTable(x, margin)
})

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
