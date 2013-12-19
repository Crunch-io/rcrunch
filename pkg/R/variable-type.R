VARIABLE_TYPES <- c("numeric", "text", "categorical") ## Add datetime when server supports

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

setMethod("type<-", "CrunchVariable", 
    function (x, value) castVariable(x, value))

##' Methods that prepare a data.frame to be written out as CSV and uploaded
setMethod("preUpload", "data.frame", function (x) {
    x[] <- lapply(x, preUpload)
    return(x)
})
setMethod("preUpload", "ANY", function (x) x)
setMethod("preUpload", "factor", function (x) as.numeric(x))
setMethod("preUpload", "Date", function (x) as.character(x))
setMethod("preUpload", "POSIXt", function (x) as.character(x))

##' Functions that modify remote Crunch variables after uploading CSV so that
##' all metadata R knows is translated to Crunch.

setMethod("postUpload", "ANY", function (source.var, crunch.var) {
    castVariable(crunch.var, "text")
})
setMethod("postUpload", "numeric", function (source.var, crunch.var) {
    castVariable(crunch.var, "numeric")
})
setMethod("postUpload", "factor", function (source.var, crunch.var) {
    casted <- castVariable(crunch.var, "categorical")
    ## Rename the categories
    cats <- categories(casted)
    v <- values(cats)
    n <- names(cats)
    n[!is.na(v)] <- levels(source.var)[v[!is.na(v)]]
    names(cats) <- n    
    categories(casted) <- cats
    invisible(casted)
})
setMethod("postUpload", "Date", function (source.var, crunch.var) {
    # castVariable(crunch.var, "datetime")  ## server autodetects
    crunch.var
})
setMethod("postUpload", "POSIXt", function (source.var, crunch.var) {
    # castVariable(crunch.var, "datetime")  ## server autodetects
    crunch.var
})