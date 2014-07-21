setMethod("zcl", "CrunchExpression", function (x) x@expression)
setMethod("zcl", "CrunchVariable", function (x) zcl(tuple(x)))
setMethod("zcl", "VariableTuple", function (x) list(variable=x$id))
setMethod("zcl", "numeric", function (x) {
    zztype <- attr(x, "typeof")
    attributes(x) <- NULL
    if (length(x) == 1) {
        out <- list(value=x)
    } else {
        out <- list(column=x)
    }
    if (!is.null(zztype)) {
        if (is.variable(zztype) || inherits(zztype, "VariableTuple")) {
            zztype <- zfunc("typeof", zztype)
        }
        out$type <- zztype
    }
    return(out)
})

typeof <- function (x, variable) {
    if (is.character(variable)) {
        variable <- list(type=list(class=variable))
    }
    attr(x, "typeof") <- variable
    return(x)
}

zfunc <- function (func, ...) {
    return(list(`function`=func, args=lapply(list(...), zcl)))
}
