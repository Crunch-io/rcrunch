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
        x[is.na(x)] <- FALSE
        out <- list(column=I(x), type=list(class="boolean"))
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

findExpressionVariables <- function (x) {
    ## Get the unique set of URLs of variables referenced in a CrunchExpr
    x <- zcl(x)
    finder <- function (expr) {
        if (is.list(expr)) {
            if ("variable" %in% names(expr)) {
                return(expr[["variable"]])
            } else {
                return(lapply(expr, finder))
            }
        } else {
            return(NULL)
        }
    }
    return(unique(unlist(lapply(x, finder))))
}

variableURLToExpr <- function (u) {
    ## Given a variable URL, make a valid CrunchExpr that points to it
    CrunchExpr(expression=list(variable=u), dataset_url=datasetReference(u))
}

validFilterForExpr <- function (expr) {
    ## Given a CrunchExpr, return a CrunchLogicalExpr that is TRUE wherever all
    ## variables referenced in `expr` are non-missing. E.g. for expression
    ## var1 == "Yes" & var2 > 4, returns an expression equivalent to
    ## !is.na(var1) & !is.na(var2).
    urls <- findExpressionVariables(expr)
    return(Reduce("&", lapply(urls, function (u) !is.na(variableURLToExpr(u)))))
}
