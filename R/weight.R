#' Dataset weights
#'
#' `weight` lets you view and set your user's currently applied weight on the
#' server. `weightVariables` lets you view all of the variables that have been
#' designated as valid to use as weights.
#' @param x a Dataset
#' @param value a Variable to set as weight, or NULL to remove the existing
#' weight
#' @return For the `weight` getter, a Variable if there is a weight, otherwise the function
#' returns `NULL`. For the setter, the dataset's weights are modified accordingly.
#' `weightVariables` returns the aliases (or names, according to `options(crunch.namekey.dataset)`),
#' of the variables designated as weights.
#' @aliases weightVariables
#' @export
weight <- function (x) {
    stopifnot(is.dataset(x))
    prefs <- ShojiEntity(crGET(shojiURL(x, "fragments", "preferences")))
    w <- prefs$weight
    if (!is.null(w)) {
        w <- CrunchVariable(allVariables(x)[[w]], filter=activeFilter(x))
    }
    return(w)
}

#' @rdname weight
#' @export
`weight<-` <- function (x, value) {
    stopifnot(is.dataset(x))
    if (is.variable(value)) {
        value <- self(value)
    } else if (!is.null(value)) {
        halt("Weight must be a Variable or NULL")
    }
    currentWeight <- ShojiEntity(crGET(shojiURL(x, "fragments", "preferences")))$weight
    if (!identical(value, currentWeight)) {
        crPATCH(shojiURL(x, "fragments", "preferences"), body=toJSON(list(weight=value)))
        x <- refresh(x)
    }
    return(x)
}

#' @rdname weight
#' @export
setMethod("weightVariables", "CrunchDataset",
    function (x) weightVariables(allVariables(x)))

#' @rdname weight
#' @export
setMethod("weightVariables", "VariableCatalog", function (x) {
    ## Get weight variable order
    ord <- VariableOrder(crGET(shojiURL(x, "orders", "weights")))
    ## Subset weight variable catalog with it
    vars <- x[ord]
    ## Return the names/aliases
    if (length(vars)) {
        return(sort(vapply(index(vars), vget(namekey(x)), character(1),
            USE.NAMES=FALSE)))
    } else {
        return(c())
    }
})
