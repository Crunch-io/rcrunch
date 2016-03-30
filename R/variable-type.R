CASTABLE_TYPES <- c("numeric", "text", "categorical") ## Add datetime when server supports

##' Change the type of Crunch variables
##'
##' Numeric, text, and categorical variables can be cast to one another by
##' assigning them a new "type". This modifes the storage of the data on the
##' server and should only be done in narrow circumstances, as in when importing
##' data from a different file format has resulted in incorrect types being
##' specified.
##'
##' @param x a Variable
##' @param value For the setter, a character value in c("numeric", "text",
##' "categorical")
##' @return Getter returns character; setter returns \code{x} duly modified.
##' @name type
##' @aliases type type<-
NULL

##' @rdname type
##' @export
setMethod("type", "CrunchVariable", function (x) type(tuple(x)))

castVariable <- function (x, value) {
    if (!(type(x) %in% CASTABLE_TYPES)) {
        halt("Cannot change the type of a ", class(x), " by type<-")
    }
    if (!(value %in% CASTABLE_TYPES)) {
        halt(dQuote(value), " is not a Crunch variable type that can be assigned.",
            " Valid types are ", serialPaste(dQuote(CASTABLE_TYPES)))
    }
    if (type(x) != value) { ## no-op if cast type is same as current type
        crPOST(shojiURL(x, "views", "cast"), body=toJSON(list(cast_as=value)))
        x <- refresh(x)
    }
    invisible(x)
}

##' @rdname type
##' @export
setMethod("type<-", "CrunchVariable", castVariable)
