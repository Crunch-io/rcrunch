#' Show the variable transformations on a CrunchCube
#'
#' @param x a CrunchCube
#'
#' @return an array with any transformations applied
#'
#' @export
setMethod("showTransforms", "CrunchCube", function (x) {
    ary <- x@arrays$count
    trans <- tryCatch(Transforms(data=index(variables(x))[[1]]$view$transform), error = function(e) NULL)
    var_cats <- Categories(data=index(variables(x))[[1]]$categories)
    # TODO: calculate category/element changes

    # NA missing cells
    ary[names(var_cats[is.na(var_cats)])] <- NA

    ary_out <- calcTransform(ary, trans, var_cats)

    return(ary_out)
})