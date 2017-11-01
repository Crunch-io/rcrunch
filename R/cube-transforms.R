#' Show the variable transformations on a CrunchCube
#'
#' @param x a CrunchCube
#'
#' @return an array with any transformations applied
#'
#' @export
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

#' From a cube, calculate the transforms and return an array
#'
#' `applyTransforms` calculates transforms (e.g. subtotals) on a CrunchCube.
#' Currently only the row transforms are supported. This is useful if you want
#' to use the values from the subtotals of the CrunchCube in an analysis.
#'
#' @param x a CrunchCube
#' @param ary an array to use, if not using the default array from the cube
#' itself. (Default: not used, pulls an array from the cube directly)
#' @param ... arguments to pass to `cubeToArray`
#'
#' @return an array with any transformations applied
#'
#' @examples
#' \dontrun{
#' transformed_array <- applyTransforms(crtabs(~opinion, ds))
#' }
#'
#' @export
applyTransforms <- function (x, ary, ...) {
    # TODO: add options for not adding headings, only return subtotals
    if (missing(ary)) {
        ary <- cubeToArray(x, ...)
    }
    # if there are row transforms, calculate them and display them
    row_trans <- tryCatch(Transforms(data=index(variables(x))[[1]]$view$transform), error = function(e) NULL)
    if (!is.null(row_trans)) {
        var_cats <- Categories(data=index(variables(x))[[1]]$categories)
        # TODO: calculate category/element changes

        if (length(dim(ary)) > 1) {
            off_margins <- seq_along(dim(ary))[-1]
            dim_names  <- names(dimnames(ary))
            ary <- apply(ary, off_margins, calcTransform, row_trans, var_cats)
            names(dimnames(ary)) <- dim_names
        } else {
            ary <- calcTransform(ary, row_trans, var_cats)
        }
    }

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