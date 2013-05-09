VARIABLE_TYPES <- c("numeric", "text", "categorical")

setGeneric("type", function (x) standardGeneric("type"))
setMethod("type", "CrunchVariable", function (x) x@body$type)
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

# translateVariableType <- function (x, to.crunch=TRUE) {
#     if (to.crunch) {
#         crunchType(x)
#     } else {
#         RType(x)
#     }
# }

crunchType <- function (x) {
    if (is.data.frame(x)) {
        return(vapply(x, crunchType, character(1)))
    } else if (is.numeric(x)) {
        return("numeric")
    } else if (is.factor(x)) {
        return("categorical")
    } else {
        return("text")
    }
}

setGeneric("preUpload", function (x) standardGeneric("preUpload"), signature="x")
setMethod("preUpload", "data.frame", function (x) {
    x[] <- lapply(x, preUpload)
    return(x)
})
setMethod("preUpload", "ANY", function (x) x)
setMethod("preUpload", "factor", function (x) as.numeric(x))

setGeneric("postUpload", 
    function (source.var, crunch.var) standardGeneric("postUpload"),
    signature="source.var")
setMethod("postUpload", "ANY", function (source.var, crunch.var) {
    castVariable(crunch.var, "text")
})
setMethod("postUpload", "numeric", function (source.var, crunch.var) {
    castVariable(crunch.var, "numeric")
})
setMethod("postUpload", "factor", function (source.var, crunch.var) {
    castVariable(crunch.var, "categorical")
})