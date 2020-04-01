#' @rdname variable-as-methods
#' @export
setMethod("as.Numeric", "CrunchExpr", function(x) {
  return(zfuncExpr("cast", x, "numeric"))
})

#' @rdname variable-as-methods
#' @export
as.double.CrunchExpr <- function(x, ...) as.Numeric(x)

# #' @rdname variable-as-methods
# #' @export
# as.character.CrunchExpr <- function(x, ...) as.Text(x)
