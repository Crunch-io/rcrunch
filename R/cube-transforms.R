#' Show the variable transformations on a CrunchCube
#'
#' @param x a CrunchCube
#'
#' @return an array with any transformations applied
#'
setMethod("showTransforms", "CrunchCube", function (x) {
    if (is.null(transforms(x))) {
        print(cubeToArray(x))
        return(invisible(x))
    } else {
        appliedTrans <- applyTransforms(x)
        row_cats <- Categories(data=index(variables(x))[[1]]$categories)
        row_styles <- transformStyles(transforms(x)[[1]], row_cats[!is.na(row_cats)])

        if (length(dim(appliedTrans)) <= 2) {
            out <- prettyPrint2d(appliedTrans, row_styles = row_styles)
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
#' `applyTransforms` calculates transforms (e.g. subtotals) on a CrunchCube.
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
#' transformed_array <- applyTransforms(crtabs(~opinion, ds))
#'
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
    # if there are row transforms, calculate them and display them
    row_trans <- transforms(x)[[1]]
    if (!is.null(row_trans)) {
        var_cats <- Categories(data=index(variables(x))[[1]]$categories)

        # TODO: calculate category/element changes

        if (length(dim(array)) > 1) {
            off_margins <- seq_along(dim(array))[-1]
            dim_names  <- names(dimnames(array))
            array <- apply(array, off_margins, calcTransforms, row_trans, var_cats, ...)
            names(dimnames(array)) <- dim_names
        } else {
            array <- as.array(calcTransforms(array, row_trans, var_cats, ...))
        }

        array <- subsetTransformedCube(array, x)
    }

    # TODO: calculate column transforms
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
        # get the insertions
        inserts <- Insertions(data=i$insertions)
        # subtype insertions so that Subtotal, Heading, etc. are their rightful selves
        inserts <- subtypeInsertions(inserts)
        
        Transforms(insertions = inserts,
                   categories = NULL,
                   elements = NULL)
    })
    
    names(transes_out) <- aliases(x)
    return(transes_out)
})

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

#' Remove transformations from a cube
#'
#' This is useful if you don't want to see or use any transformations like Subtotals and Headings
#'
#' @param cube a CrunchCube
#'
#' @return the cube with no transformations
#'
#' @examples 
#' \dontrun{
#' noTransforms(crtabs(~opinion, ds))
#' #             Strongly Agree             Somewhat Agree Neither Agree nor Disagree 
#' #                         23                         24                         18 
#' #          Strongly Disagree          Somewhat Disagree 
#' #                         19                         16 
#' }
#'
#' @export
noTransforms <- function (cube) {
    transforms(cube) <- NULL
    return(cube)
}
