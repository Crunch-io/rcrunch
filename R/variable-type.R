#' @rdname crunch-is
#' @export
is.variable <- function(x) inherits(x, "CrunchVariable")

#' @rdname crunch-is
#' @export
is.Numeric <- function(x) inherits(x, "NumericVariable")

#' @rdname crunch-is
#' @export
is.Categorical <- function(x) inherits(x, "CategoricalVariable")

#' @rdname crunch-is
#' @export
is.Text <- function(x) inherits(x, "TextVariable")

#' @rdname crunch-is
#' @export
is.Datetime <- function(x) inherits(x, "DatetimeVariable")

#' @rdname crunch-is
#' @export
is.Multiple <- function(x) inherits(x, "MultipleResponseVariable")

#' @rdname crunch-is
#' @export
is.MR <- is.Multiple

#' @rdname crunch-is
#' @export
is.MultipleResponse <- is.Multiple

#' @rdname crunch-is
#' @export
is.CA <- function(x) {
    ## so it doesn't return true for MultipleResponse
    return(identical(class(x), class(CategoricalArrayVariable())))
}

#' @rdname crunch-is
#' @export
is.CategoricalArray <- is.CA

#' @rdname crunch-is
#' @export
is.NumericArray <- function(x) inherits(x, "NumericArrayVariable")

#' @rdname crunch-is
#' @export
is.Array <- function(x) inherits(x, "ArrayVariable")

has.categories <- function(x) {
    if (!is.character(x)) {
        ## Let you pass in either a type name string or a variable. If variable,
        ## get its type
        x <- type(x)
    }
    return(x %in% c("categorical_array", "multiple_response", "categorical"))
}

is.subvariable <- function(x) {
    ## TODO: server should support a better way of determining this
    grepl("subvariables", self(x))
}

CASTABLE_TYPES <- c("numeric", "text", "categorical") ## Add datetime when server supports

ARRAY_TYPES <- c("categorical_array", "multiple_response", "numeric_array")

#' Change Crunch variable types
#'
#' Numeric, text, and categorical variables can be cast to one another by
#' assigning them a new "type". This modifies the storage of the data on the
#' server and should only be done in narrow circumstances, as in when importing
#' data from a different file format has resulted in incorrect types being
#' specified.
#'
#' @param x a Variable
#' @param value For the setter, a character value in c("numeric", "text",
#' "categorical")
#' @return Getter returns character; setter returns `x` duly modified.
#' @name type
#' @aliases type type<-
NULL

#' @rdname type
#' @export
setMethod("type", "CrunchVariable", function(x) type(tuple(x)))

#' @rdname type
#' @export
setMethod("type", "VariableEntity", function(x) x@body$type)

# It's also nice sometimes if a list of variables has a `types` method
#' @export
#' @rdname describe-catalog
setMethod("types", "list", function(x) {
    vapply(x, type, character(1), USE.NAMES = FALSE)
})

castVariable <- function(x, value) {
    if (!(type(x) %in% CASTABLE_TYPES)) {
        halt("Cannot change the type of a ", class(x), " by type<-")
    }
    if (!(value %in% CASTABLE_TYPES)) {
        halt(
            dQuote(value), " is not a Crunch variable type that can be assigned.",
            " Valid types are ", serialPaste(dQuote(CASTABLE_TYPES))
        )
    }
    if (type(x) != value) {
        ## no-op if cast type is same as current type
        crPOST(shojiURL(x, "views", "cast"), body = toJSON(list(cast_as = value)))
        x <- refresh(x)
    }
    invisible(x)
}

#' @rdname type
#' @export
setMethod("type<-", "CrunchVariable", castVariable)
