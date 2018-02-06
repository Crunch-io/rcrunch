#' as.* methods for variables
#'
#' Use the as.* family of functions to make a derived copy of a variable that 
#' has been converted into a new type.
#' 
#' Each type of Crunch variable (text, numeric, categorical, etc.) has an `as.*`
#' function (`as.Text`, `as.Numeric`, and `as.Categorical` respectively) that 
#' takes the input given as `x`, and makes a new derived variable that is now of 
#' the type specified. See below for detailed examples.
#' 
#' For `as.Text` and `as.Numeric`, aliases to the R-native functions 
#' `as.character` and `as.numeric` are provided for convenience.
#'
#' @param x a Crunch variable to derive and convert to a new type
#' @param format for `as.Datetime`, when the variable in `x` is a text or 
#' categorical variable, the format that the date-time is formatted in (default: 
#' \code{"\%Y-\%m-\%d \%H:\%M:\%S"}); for `as.Text` and `as.Categorical`, the 
#' format that the datetime should be formatted as (default: 
#' \code{"\%Y-\%m-\%d \%H:\%M:\%S"}).
#' @param resolution for `as.Datetime`, when the variable in `x` is a numeric 
#' variable, the resolution of the number (e.g. `"ms"` for milliseconds, `"s"` 
#' for seconds, etc. link)
#' @param ... additional arguments for `as.character` and `as.numeric`, ignored when used with 
#' Crunch variables 
#'
#' @return a `VariableDefinition` to be used as the derivation 
#'
#' @examples
#' \dontrun{
#' # ds$v1 is of type Text
#' is.Text(ds$v1)
#' # [1] TRUE
#' 
#' # that has strings of numbers
#' as.vector(ds$v1)
#' # [1] "32"   "8"    "4096" "1024"
#' 
#' # convert this to a numeric variable with the alias `v1_numeric`
#' ds$v1_numeric <- as.Numeric(ds$v1)
#' 
#' # the values are the same, but are now numerics and the type is Numeric
#' as.vector(ds$v1_numeric)
#' # [1]   32    8 4096 1024
#' is.Numeric(ds$v1_numeric)
#' # [1] TRUE
#' 
#' # this new variable is derived, so if new data is appended or streamed, the
#' # new rows of data will be updated.
#' is.derived(ds$v1_numeric)
#' # [1] TRUE
#' }
#' @name variable-as-methods
#' @aliases as.Categorical as.Datetime as.Numeric as.Text
NULL


#' @rdname variable-as-methods
#' @export
setMethod("as.Numeric", "CrunchVariable", function (x) {
    haltIfArray(x, callingFunc = "as.Numeric()")
    
    # datetimes are special
    if (is.Datetime(x)) {
        return(zfuncExpr("datetime_to_numeric", x))
    }
    
    return(zfuncExpr("cast", x, "numeric"))
})

#' @rdname variable-as-methods
#' @export
setMethod("as.Text", "CrunchVariable",
          function (x, format = "%Y-%m-%d %H:%M:%S") {
    haltIfArray(x, callingFunc = "as.Text()")
    
    # datetimes are special
    if (is.Datetime(x)) {
        return(zfuncExpr("format_datetime", x, list(value = format)))
    }
    
    return(zfuncExpr("cast", x, "text"))
})

#' @rdname variable-as-methods
#' @export
setMethod("as.Categorical", "CrunchVariable",
          function (x, format = "%Y-%m-%d %H:%M:%S") {
    haltIfArray(x, callingFunc = "as.Categorical()")
    
    # datetimes are special
    if (is.Datetime(x)) {
        # no direct datetime to cat ZCL function, but can just cast the string
        return(zfuncExpr("cast",
                         zfunc("format_datetime", x, list(value = format)),
                         "categorical"))
    }
    
    return(zfuncExpr("cast", x, "categorical"))
})

#' @rdname variable-as-methods
#' @export
setMethod("as.Datetime", "CrunchVariable", function (x, format, resolution) {
    haltIfArray(x, callingFunc = "as.Datetime()")
    
    # datetimes are special, so each source type must be determined
    if (is.Numeric(x)) {
        # TODO: validate resolution
        return(zfuncExpr("numeric_to_datetime", x, list(value = resolution)))
    } else if (is.Text(x) | is.Categorical(x)) {
        return(zfuncExpr("parse_datetime", x, list(value = format)))
    }
    
    halt("Can't derive Datetime variables from ", dQuote(type(x)), " variables.")
})

#' @rdname variable-as-methods
#' @export
as.double.CrunchVariable <- function (x, ...) as.Numeric(x)

#' @rdname variable-as-methods
#' @export
as.character.CrunchVariable <- function (x, ...) as.Text(x)

# halt if variable is an array variable. If callingFunc is provided, provide the function that
# the user used to get to this point. 
haltIfArray <- function (variable, callingFunc) {
    # if the variable is not an array type, return quickly
    # TODO: should this also short-circuit if variable is not a variable?
    if (!is.Array(variable)) {
        return(TRUE)
    }
    
    # Add the callingFunc in, if present
    if (!missing(callingFunc)) {
        halt("Array-like variables can't be used with function `",
             callingFunc, "`.")
    }
    
    # can't determine the function or haltIfArray is being called directly,
    # still error
    halt("Array-like variables can't be used.")
}
