## Functions to make ZZ9 Command Language query objects

r2zcl <- function (x) {
    ## Convert an R vector to a value/column to be sent in a ZCL request.
    ## Called inside the zcl() method.
    v <- toVariable(x)
    attributes(v$values) <- NULL

    ## If there is a single value, call it "value". Else it is a "column" array
    if (length(x) == 1) {
        out <- list(value=v$values)
    } else {
        out <- list(column=v$values)
    }

    return(out)
}

## Methods to convert various objects to ZCL
setMethod("zcl", "CrunchExpr", function (x) x@expression)
setMethod("zcl", "CrunchVariable", function (x) list(variable=self(x)))
setMethod("zcl", "VariableTuple", function (x) list(variable=self(x)))
setMethod("zcl", "numeric", r2zcl)
setMethod("zcl", "character", r2zcl)
setMethod("zcl", "Date", r2zcl)
setMethod("zcl", "POSIXt", r2zcl)
setMethod("zcl", "logical", function (x) {
    x[is.na(x)] <- FALSE
    out <- list(column=I(x), type=list(class="boolean"))
    return(out)
})
setMethod("zcl", "NULL", function (x) NULL)
setOldClass("zcl")
setMethod("zcl", "zcl", function (x) x)
setMethod("zcl", "list", function (x) x) ## is this a good idea?
setMethod("zcl", "CrunchFilter", function (x) x@body$expression)

zfunc <- function (func, ...) {
    ## Wrapper that creates ZCL function syntax
    return(list(`function`=func, args=lapply(list(...), zcl)))
}
