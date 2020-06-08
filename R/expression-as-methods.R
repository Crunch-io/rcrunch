#' @rdname variable-as-methods
#' @export
setMethod("as.Numeric", "CrunchExpr", function(x) {
  return(zfuncExpr("cast", x, "numeric"))
})

#' @rdname variable-as-methods
#' @export
setMethod("as.Text", "CrunchExpr", function(x, format) {
    # format is for DateTimes only, but we have to rely on user
    # doing this correctly because we can't tell for an expression
    if (!missing(format)) {
        return(zfuncExpr("format_datetime", x, list(value = format)))
    }

    return(zfuncExpr("cast", x, "text"))
})

#' @rdname variable-as-methods
#' @export
setMethod("as.Categorical", "CrunchExpr", function(x, format) {
    # format is for DateTimes only, but we have to rely on user
    # doing this correctly because we can't tell for an expression
    # no direct datetime to cat ZCL function, so must format first then cast
    if (!missing(format)) {
        x <- zfunc("format_datetime", x, list(value = format))
    }

    return(zfuncExpr("cast", x, "categorical"))
})

#' @rdname variable-as-methods
#' @export
setMethod(
    "as.Datetime", "CrunchExpr",
    function(x, format, resolution, offset) {
        # datetimes are special, so each source type must be determined
        # resolution & offset nonmissing indicate numeric
        # format non-missing indicates categorical/text
        if (missing(format) & !missing(resolution) & !missing(offset)) {
            validateResolution(resolution)

            args <- list("numeric_to_datetime", x, list(value = resolution))
            if (!missing(offset)) {
                args <- append(args, list(list(value = offset)))
            }

            return(do.call(zfuncExpr, args))
        } else if (!missing(format) & missing(resolution) & missing(offset)) {
            return(zfuncExpr("parse_datetime", x, list(value = format)))
        }

        halt(
            "Invalid arguments to `as.Datetime`. Must provide either both `resolution` and ",
            "`offset` for Numeric expressions or `format` for Categorical/Text expressions."
        )
    }
)

#' @rdname variable-as-methods
#' @export
as.double.CrunchExpr <- function(x, ...) as.Numeric(x)

#' @rdname variable-as-methods
#' @export
as.character.CrunchExpr <- function(x, ...) as.Text(x)
