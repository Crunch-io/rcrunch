#' Show the variable transformations on a CrunchCube
#'
#' @param x a CrunchCube
#'
#' @return an array with any transformations applied
#'
#' @export
setMethod("showTransforms", "CrunchCube", function (x) applyTransforms(x))

#' From a cube, calculate the transforms and return an array
#'
#' @param x a CrunchCube
#' @param ary an array to use, if not using the cube itself. (Default: not used, pulls an array from the cube directly)
#' @param ... arguments to pass to `cubeToArray`
#'
#' @return an array with any transformations applied
#'
#' @export
applyTransforms <- function (x, ary, ...) {
    if (missing(ary)) {
        ary <- cubeToArray(x, ...)
    }

    # if there are transforms, calculate them and display them
    trans <- tryCatch(Transforms(data=index(variables(x))[[1]]$view$transform), error = function(e) NULL)
    if (!is.null(trans)) {
        var_cats <- Categories(data=index(variables(x))[[1]]$categories)
        # TODO: calculate category/element changes

        # NA missing cells
        ary[names(var_cats[is.na(var_cats)])] <- NA

        ary <- calcTransform(ary, trans, var_cats)
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