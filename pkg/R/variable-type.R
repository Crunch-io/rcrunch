VARIABLE_TYPES <- c("numeric", "text", "categorical")

setGeneric("type", function (x) standardGeneric("type"))
setMethod("type", "CrunchVariable", function (x) x@body$family)
## do type casting as type<-

castVariable <- function (x, to) {
    if (!(to %in% VARIABLE_TYPES)) {
        stop(sQuote(to), " is not a valid Crunch variable type. Valid types ",
            "are ", serialPaste(sQuote(VARIABLE_TYPES)))
    }
    POST(cast_url(x), body=toJSON(list(cast_as=to)))
    invisible(refresh(x))
}

cast_url <- function (x) x@urls$cast_url

setGeneric("type<-", function (x, value) standardGeneric("type<-"))
setMethod("type<-", "CrunchVariable", 
    function (x, value) castVariable(x, value))

translateVariableType <- function (x, to.crunch=TRUE) {
    if (to.crunch) {
        crunchType(x)
    } else {
        RType(x)
    }
}

crunchType <- function (x) {
    if (is.data.frame(x)) {
        return(vapply(x, crunchType, character(1)))
    } else if (is.numeric(x)) {
        return("numeric")
    } else {
        return("text")
    }
}