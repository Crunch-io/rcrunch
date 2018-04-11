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
    if (length(x)) {
        out <- r2zcl(x)
        out$type <- list(class="categorical", categories=.selected.cats)
        return(out)
    } else {
        ## If you reference a variable in a dataset that doesn't exist, you
        ## get NULL, and e.g. NULL == something becomes logical(0).
        ## That does awful things if you try to send to the server. So don't.
        halt("Invalid expression. Probably a reference to a variable that doesn't exist.")
    }
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
