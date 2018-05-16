#' @rdname showTransforms
#' @export
setMethod("showTransforms", "CrunchCube", function (x) {
    if (all(is.null(transforms(x)))) {
        print(cubeToArray(x))
        return(invisible(x))
    } else {
        appliedTrans <- applyTransforms(x)
        # if the row dimension is categorical, make styles
        if (index(variables(x))[[1]]$type == "categorical") {
            row_cats <- Categories(data=index(variables(x))[[1]]$categories)
            row_styles <- transformStyles(transforms(x)[[1]], row_cats[!is.na(row_cats)])
        } else {
            # otherwise punt, because this is an array or MR var.
            row_styles <- NULL
        }

        # if the columns dimension is categorical, make styles
        if (length(dim(x)) > 1 && index(variables(x))[[2]]$type == "categorical") {
            col_cats <- Categories(data=index(variables(x))[[2]]$categories)
            col_styles <- transformStyles(transforms(x)[[2]], col_cats[!is.na(col_cats)])
        } else {
            # otherwise punt, because this is an array or MR var.
            col_styles <- NULL
        }        
        
        if (length(dim(appliedTrans)) <= 2) {
            out <- prettyPrint2d(
                appliedTrans,
                row_styles = row_styles,
                col_styles = col_styles
            )
            cat(unlist(out), sep="\n")
        } else {
            # styling is hard
            print(appliedTrans)
        }

        return(invisible(appliedTrans))
    }
})

#' @rdname applyTransforms
#' @export
setMethod("subtotalArray", "CrunchCube", function(x, headings = FALSE) {
    includes <- c("subtotals")
    if (headings) {
        includes <- c(includes, "headings")
    }
    applyTransforms(x, include = includes)
})

#' Calculate the transforms from a CrunchCube
#'
#' `applyTransforms` calculates transforms (e.g. [Subtotals][SubtotalsHeadings]) on a CrunchCube.
#' Currently only the row transforms are supported. This is useful if you want
#' to use the values from the subtotals of the CrunchCube in an analysis.
#'
#' Including the `include` argument allows you to specify which parts of the
#' CrunchCube to return. The options can be any of the following: "cube_cells"
#' for the untransformed values from the cube itself, "subtotals" for the
#' subtotal insertions, and "headings" for any additional headings. Any
#' combination of these can be given, by default all will be given.
#'
#' `subtotalArray(cube)` is a convenience function that is equivalent to
#' `applyTransforms(cube, include = c("subtotals"))`
#'
#' @param x a CrunchCube
#' @param array an array to use, if not using the default array from the cube
#' itself. (Default: not used, pulls an array from the cube directly)
#' @param ... arguments to pass to `calcTransforms`, for example `include`
#' @param headings for `subtotalArray`: a logical indicating if the headings
#' should be included with the subtotals (default: `FALSE`)
#'
#' @return an array with any transformations applied
#'
#' @examples
#' \dontrun{
#' # to get an array of just the subtotals
#' subtotalArray(crtabs(~opinion, ds))
#' #    Agree Disagree
#' #       47       35
#'
#' # to get the full array with the subtotals but not headings
#' applyTransforms(crtabs(~opinion, ds), include = c("cube_cells", "subtotals"))
#' #             Strongly Agree             Somewhat Agree                      Agree
#' #                         23                         24                         47
#' # Neither Agree nor Disagree          Strongly Disagree                   Disagree
#' #                         18                         19                         35
#' #          Somewhat Disagree
#' #                         16
#' # to get the full array with the headings but not subtotals
#' applyTransforms(crtabs(~opinion, ds), include = c("cube_cells", "headings"))
#' #               All opinions             Strongly Agree             Somewhat Agree
#' #                         NA                         23                         24
#' # Neither Agree nor Disagree          Strongly Disagree          Somewhat Disagree
#' #                         18                         19                         16
#' }
#'
#' @aliases subtotalArray
#' @export
applyTransforms <- function (x, array = cubeToArray(x), ...) {
    dim_names  <- names(dimnames(array))
    ndims <- length(dim(array))
    
    # 1 dimensional cubes are special, and need to be t
    if (ndims == 1) {
        try({
            match_ind <- which(aliases(variables(x)) == dim_names[[1]])
            row_trans <- transforms(x)[[match_ind]]
            if (!is.null(row_trans)) {
                var_cats <- Categories(data=variables(x)[[match_ind]]$categories)
                array <- as.array(calcTransforms(array, row_trans, var_cats, ...))
                array <- subsetTransformedCube(array, x)
            }
        })
        return(array)
    } 

    # Try to calculate the transforms for any dimensiton that have them, but
    # fail silently if they aren't calcuable. We use a for loop so that failing
    # one dimension doesn't break others. We could possibly use something like
    # abind::abind here and bind together the insertion vectors in array form,
    # but that would add a dependency
    for (d in seq_len(ndims)) {
        try({
            match_ind <- which(aliases(variables(x)) == dim_names[[d]])
            
            trans <- transforms(x)[[match_ind]]
            if (!is.null(trans)) {
                var_cats <- Categories(data=variables(x)[[match_ind]]$categories)
                
                # TODO: calculate category/element changes
                
                # find the off-margins to calculate over
                off_margins <- seq_along(dim(array))[-d]
                
                # calculate the transforms
                array <- apply(array, off_margins, calcTransforms, trans, var_cats, ...)
                
                # aperm necesary because anything other than when off_margins is
                # not c(2), c(2,3), etc. will return in an incorrect order
                # microbenchmarking indicates that aperm is not particularly
                # expensive even on large arrays
                array <- aperm(array, append(2:ndims, 1, after=d-1))
                
                # re-attach names 
                names(dimnames(array)) <- dim_names
                
                array <- subsetTransformedCube(array, x)
            }
        }, silent = TRUE)
    }
    
    return(array)
}

subsetTransformedCube <- function (array, cube) {
    # subset variable categories to only include non-na
    dims <- dim(array)
    keep_all <- lapply(seq_along(dims),
                       function (i) {
                           out <- rep(TRUE, dims[i])
                           names(out) <- dimnames(array)[[i]]
                           return(out)
                       })
    names(keep_all) <- names(dimensions(cube))[seq_along(keep_all)]
    keep_these_cube_dims <- evalUseNA(array, dimensions(cube)[seq_along(keep_all)], cube@useNA)

    # remove the categories determined to be removable above
    keep_these <- mapply(function(x, y) {
        not_ys <- y[!y]
        x[names(not_ys)] <- not_ys
        return(x)
    }, keep_all,
    keep_these_cube_dims,
    SIMPLIFY=FALSE,
    USE.NAMES=TRUE)

    # match up those in keep_these that are already in array
    array_dimnames <- dimnames(array)
    for (i in seq_along(array_dimnames)) {
        to_keep <- names(keep_these[[i]]) %in% array_dimnames[[i]]
        keep_these[[i]] <- keep_these[[i]][to_keep]
    }

    array <- subsetCubeArray(array, keep_these)

    return(array)
}

#' @rdname Transforms
#' @export
setMethod("transforms", "CrunchCube", function(x) { transforms(variables(x)) })

#' @rdname Transforms
#' @export
setMethod("transforms", "VariableCatalog", function (x) {
    transes <- lapply(x, function (i) {i$view$transform})

    if (all(unlist(lapply(transes, is.null)))) {
        return(NULL)
    }
    transes_out <- lapply(transes, function (i) {
        # TODO: when other transforms are implemented, this should check those too.

        # if insertions are null return NULL
        if (is.null(i$insertions) || length(i$insertions) == 0) {
            return(NULL)
        }

        # get the insertions
        inserts <- Insertions(data=i$insertions)
        # subtype insertions so that Subtotal, Heading, etc. are their rightful selves
        inserts <- subtypeInsertions(inserts)

        return(Transforms(insertions = inserts,
                          categories = NULL,
                          elements = NULL)
        )
    })

    names(transes_out) <- aliases(x)
    return(transes_out)
})

#' @rdname Transforms
#' @export
setMethod("transforms<-", c("CrunchCube", "list"), function (x, value) {
    dims <- dimensions(x)
    dimnames <- names(dims)

    # check if the names of the dimensions and the names of the transforms line up
    validateNamesInDims(names(value), x, what = "transforms")

    # replace the transforms for each dimension
    dims <- CubeDims(lapply(dimnames, function (dim_name) {
        dim_out <- dims[[dim_name]]
        if (dim_name %in% names(value)) {
            # grab the matching insertions, make sure they are proper Insertions
            # and then add them to the dimensions to return.
            one_trans <- value[[dim_name]]
            vars <- variables(x)
            cats <- categories(vars[[which(aliases(vars) == dim_name)]])
            one_trans$insertions <- Insertions(
                data = lapply(one_trans$insertions, makeInsertion,
                              var_categories = cats)
            )
            dim_out$references$view$transform <- jsonprep(one_trans)
        }
        return(dim_out)
    }))

    # rename, replace the dimensions and return the cube
    names(dims) <- dimnames
    dimensions(x) <- dims
    return(invisible(x))
})

#' error iff the names are not a dimension in the cube provided
#'
#' @param names the names to check for in the cube
#' @param cube a CrunchCube object to check
#' @param what a character describing what is being checked (default: transforms
#' ) to include in the error to make it easier for users to see what is failing.
#' 
#' @keywords internal
validateNamesInDims <- function (names, cube, what = "transforms") {
    dimnames <- names(dimensions(cube))

    # check if the names of the dimensions and the names of the transforms line up
    if (any(!(names %in% dimnames))) {
        halt("The names of the ", what, " supplied (",
             serialPaste(dQuote(names))
             ,") do not match the dimensions of the cube (",
             serialPaste(dQuote(dimnames)) ,").")
    }
}


#' @rdname Transforms
#' @export
setMethod("transforms<-", c("CrunchCube", "NULL"), function (x, value) {
    dims <- dimensions(x)
    dimnames <- names(dims)
    dims <- CubeDims(lapply(dims, function (x) {
        if (!is.null(x$references$view$transform)) {
            x$references$view$transform <- value
        }
        return(x)
    }))
    names(dims) <- dimnames
    dimensions(x) <- dims
    return(invisible(x))
})

# TODO: add an easy way to append insertions, etc. to a cube's transforms

#' Remove transformations from a CrunchCube
#'
#' @section Removing transforms:
#' `noTransforms()` is useful if you don't want to see or use any transformations like
#' Subtotals and Headings. This action only applies to the CrunchCube object in
#' R: it doesn't actually change the variables on Crunch servers or the query
#' that generated the CrunchCube.
#'
#' @param cube a CrunchCube
#'
#' @return the CrunchCube with no transformations
#'
#' @examples
#' \dontrun{
#' # A CrunchCube with a heading and subtotals
#' crtabs(~opinion, ds)
#' #               All opinions
#' #             Strongly Agree 23
#' #             Somewhat Agree 24
#' #                      Agree 47
#' # Neither Agree nor Disagree 18
#' #          Somewhat Disagree 16
#' #          Strongly Disagree 19
#' #                   Disagree 35
#'
#' noTransforms(crtabs(~opinion, ds))
#' #             Strongly Agree             Somewhat Agree Neither Agree nor Disagree
#' #                         23                         24                         18
#' #          Somewhat Disagree          Strongly Disagree
#' #                         16                         19
#' }
#'
#' @export
noTransforms <- function (cube) {
    transforms(cube) <- NULL
    return(cube)
}
