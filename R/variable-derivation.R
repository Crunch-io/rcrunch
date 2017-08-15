#' Get or set a derived variable's `CrunchExpr`
#'
#' Get a derived variable's derivation formula as a (`CrunchExpr`)[expressions] with
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
#'
#' # to integrate or instantiate the variable in place (remove the link between
#' # variable v1 and the derivation) you can:
#' derivation(ds$derived_v1) <- NULL
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
    # self(x) is needed and not variableCatalog because the variables are stored
    # as '../varid/' and when absoluteURL concats them, the varid from self(x)
    # is automatically removed.
    expr@expression <- absolutifyVariables(expr@expression, self(x))

    return(expr)
})


absolutifyVariables <- function (expr, base.url) {
    if (is.list(expr)) {
        # Recurse.
        lists <- vapply(expr, is.list, logical(1))
        expr[lists] <- lapply(expr[lists], absolutifyVariables, base.url)
        # For others, look for "variable" references
        if (length(names(expr))) {
            vars <- names(expr) %in% "variable"
            expr[!lists & vars] <- lapply(expr[!lists & vars], absoluteURL, base.url)
            return(expr)
        }
    }
    return(expr)
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

# sets derived to FALSE, which integrates / instantiates the variable's values
integrateDerivedVar <- function (x) {
    if (is.derived(x)) {
        payload <- toJSON(list(derived=FALSE))
        crPATCH(self(x), body=payload)

        ## Refresh and return
        dropCache(datasetReference(x))
        invisible(refresh(x))
    }
}

#' @export
#' @rdname derivations
setMethod("derivation<-", c("CrunchVariable", "NULL"), function(x, value) integrateDerivedVar(x))

#' @rdname crunch-is
#' @aliases is.derived
#' @export
setMethod("is.derived", "CrunchVariable", function (x) {
    stopifnot(is.variable(x))
    isTRUE(tuple(x)$derived)
})

#' @rdname crunch-is
#' @aliases is.derived<-
#' @export
setMethod("is.derived<-", c("CrunchVariable", "logical"), function(x, value) {
   if (!value) {
       integrateDerivedVar(x)
   }
})
