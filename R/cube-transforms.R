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
        row_styles <- transformStyles(transforms(x), row_cats[!is.na(row_cats)])

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
#' @param ary an array to use, if not using the default array from the cube
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
#' }
#'
#' @aliases subtotalArray
#' @export
applyTransforms <- function (x, ary = cubeToArray(x), ...) {
    # if there are row transforms, calculate them and display them
    row_trans <- tryCatch(Transforms(data=index(variables(x))[[1]]$view$transform), error = function(e) NULL)
    if (!is.null(row_trans)) {
        var_cats <- Categories(data=index(variables(x))[[1]]$categories)

        # TODO: calculate category/element changes

        if (length(dim(ary)) > 1) {
            off_margins <- seq_along(dim(ary))[-1]
            dim_names  <- names(dimnames(ary))
            ary <- apply(ary, off_margins, calcTransforms, row_trans, var_cats, ...)
            names(dimnames(ary)) <- dim_names
        } else {
            ary <- as.array(calcTransforms(ary, row_trans, var_cats, ...))
        }

        ary <- subsetTransformedCube(ary, x)
    }

    # TODO: calculate column transforms


    return(ary)
}

subsetTransformedCube <- function (ary, cube) {
    # subset variable categories to only include non-na
    dims <- dim(ary)
    keep_all <- lapply(seq_along(dims),
                       function (i) {
                           out <- rep(TRUE, dims[i])
                           names(out) <- dimnames(ary)[[i]]
                           return(out)
                       })
    names(keep_all) <- names(dimensions(cube))[seq_along(keep_all)]
    keep_these_cube_dims <- evalUseNA(ary, dimensions(cube)[seq_along(keep_all)], cube@useNA)

    # remove the categories determined to be removable above
    keep_these <- mapply(function(x, y) {
        not_ys <- y[!y]
        x[names(not_ys)] <- not_ys
        return(x)
    }, keep_all,
    keep_these_cube_dims,
    SIMPLIFY=FALSE,
    USE.NAMES=TRUE)

    # match up those in keep_these that are already in ary
    ary_dimnames <- dimnames(ary)
    for (i in seq_along(ary_dimnames)) {
        to_keep <- names(keep_these[[i]]) %in% ary_dimnames[[i]]
        keep_these[[i]] <- keep_these[[i]][to_keep]
    }

    ary <- subsetCubeArray(ary, keep_these)

    return(ary)
}

#' @rdname Transforms
#' @export
setMethod("transforms", "CrunchCube", function (x) {
    trans <- tryCatch(Transforms(data=index(variables(x))[[1]]$view$transform), error = function(e) NULL)
    return(trans)
})

#' @rdname Transforms
#' @export
setMethod("transforms<-", c("CrunchCube", "NULL"), function (x, value) {
    # hacked access to delete the transforms. This should have a more systematic
    # accessor
    x@dims@.Data[[1]]$references$view$transform <- value
    invisible(x)
})

#' Remove transformations from a cube
#'
#' This is useful if you don't want to see or use any transformations like Subtotals and Headings
#'
#' @param cube a CrunchCube
#'
#' @return the cube with no transformations
#'
#' @export
noTransforms <- function (cube) {
    transforms(cube) <- NULL
    return(cube)
}