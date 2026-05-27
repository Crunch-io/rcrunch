#' @import rlang
#' @import vctrs
#' @importFrom generics as.factor
#' @importFrom stats setNames

.onLoad <- function(libname, pkgname) {
    backports::import(pkgname, c("%||%"))
    s3_register("pillar::pillar_shaft", "crunch_categorical_variable")
    s3_register("dplyr::dplyr_col_modify", "crunch_array_variable")
    s3_register("dplyr::dplyr_reconstruct", "crunch_categorical_array_variable")
    s3_register("dplyr::dplyr_reconstruct", "crunch_numeric_array_variable")
}
