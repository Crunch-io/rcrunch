r2zcl <- function (x) {
    v <- toVariable(x)
    attributes(v$values) <- NULL
    
    zztype <- attr(x, "typeof")
    if (length(x) == 1) {
        out <- list(value=v$values)
    } else {
        out <- list(column=v$values)
    }
    if (!is.null(zztype)) {
        out$type <- zztype
    } else {
        out$type <- list(class=v$type)
    }
    return(out)
}

setMethod("zcl", "CrunchExpression", function (x) x@expression)
setMethod("zcl", "CrunchVariable", function (x) zcl(tuple(x)))
setMethod("zcl", "VariableTuple", function (x) list(variable=x$id))
setMethod("zcl", "numeric", r2zcl)
setMethod("zcl", "character", r2zcl)
setMethod("zcl", "Date", r2zcl)
setMethod("zcl", "POSIXt", r2zcl)
setMethod("zcl", "logical", function (x) {
    x[is.na(x)] <- FALSE
    out <- list(column=I(x), type=list(class="boolean"))
    cat(toJSON(out))
    return(out)
})
setMethod("zcl", "NULL", function (x) NULL)
setOldClass("zcl")
setMethod("zcl", "zcl", function (x) x)

typeof <- function (x, variable) {
    if (is.character(variable)) {
        variable <- list(type=list(class=variable))
    } else if (is.variable(variable) || inherits(variable, "VariableTuple")) {
        variable <- zfunc("typeof", variable)
    }
    attr(x, "typeof") <- variable
    return(x)
}

zfunc <- function (func, ...) {
    return(list(`function`=func, args=lapply(list(...), zcl)))
}
