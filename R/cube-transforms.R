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
#' @param ary an array to use, if not using the cube itself. (Default: not used, pulls an array from the cube using [cubeToArray()])
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
