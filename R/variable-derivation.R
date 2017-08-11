#' Get or set a derived variable's `CrunchExpr`
#'
#' Get a derived variable's derivation formula as a `CrunchExpr` with
#' `derivation([variable])`. Set (change) a derived variable's derivation with
#' `derivation([variable]) <- [expression]`
#'
#' @param x a variable
#' @param value a `CrunchExpr` to be used as the derivation (for the setter only)
#'
#' @return a `CrunchExpr` of the derivation
#'
#' @examples
#' \dontrun{
#'
#' ds$derived_v1 <- ds$v1 + 5
#'
#' derivation(ds$derived_v1)
#' # Crunch expression: v1 + 5
#'
#' derivation(ds$derived_v1) <- ds$v1 + 10
#' derivation(ds$derived_v1)
#' # Crunch expression: v1 + 10
#' }
#' @name derivations
#' @aliases derivation derivation<-
NULL

#' @export
#' @rdname derivations
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

#' @export
#' @rdname derivations
setMethod("derivation<-", "CrunchVariable", function(x, value) {
    if (!is.derived(x)) {
        halt("The variable ", dQuote(name(x)), " must already be a derived variable.")
    }
    if (!is.CrunchExpr(value)) {
        halt("The value ", dQuote(substitute(value)), " must be a CrunchExpr, got a ",
             class(value), " instead.")
    }

    payload <- toJSON(list(derivation=value@expression))
    crPATCH(self(x), body=payload)

    ## Refresh and return
    dropCache(datasetReference(x))
    invisible(refresh(x))
})
