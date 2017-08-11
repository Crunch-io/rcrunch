setMethod("derivation", "CrunchVariable", function(x) {
    if (!is.derived(x)) {
        return(NULL)
    }

    expr <- CrunchExpr(expression=entity(x)@body$derivation)
    expr@expression <- absolutifyVariables(expr@expression, x)

    return(expr)
})

absolutifyVariables <- function (expr, var) {
    if ("function" %in% names(expr)) {
        expr[["args"]] <- absolutifyExpressionArgs(expr[["args"]], var)
        return(expr)
    } else {
        ## Dunno what this is
        return("[Complex expression]")
    }
}

absolutifyExpressionArgs <- function (args, var) {
    args <- lapply(args, function (x, var) {
        if ("variable" %in% names(x)) {
            ## GET URL, get alias from that
            x$variable <- absoluteURL(x$variable, self(var))
            return(x)
        } else if ("value" %in% names(x)) {
            return(x)
        } else if ("function" %in% names(x)) {
            return(absolutifyVariables(x, var))
        }
    }, var)
    return(args)
}
