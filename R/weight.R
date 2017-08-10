#' Dataset weights
#'
#' "weight" lets you view and set your user's currently applied weight on the
#' server. "weightVariables" lets you view all of the variables that have been
#' designated as valid to use as weights.
#' @param x a Dataset
#' @param value a Variable to set as weight, or NULL to remove the existing
#' weight
#' @return For the \code{weight} getter, a Variable if there is a weight, else
#' NULL. For the setter, x, modified accordingly. \code{weightVariables} returns
#' the aliases (or names, according to \code{options(crunch.namekey.dataset)}),
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

#' Title
#'
#' @param ... 
#' @param ds 
#' @param name 
#'
#' @return
#' @export
#'
#' @examples
makeWeight <- function(..., name) {
    expr_list <- list(...)
    lapply(expr_list, validateExpression)
    l <- lapply(expr_list, generateWeightEntry)
    out <- list(
        name = name,
        derivation = list(
            `function` = "rake",
            args = lapply(expr_list, generateWeightEntry))
        )
    class(out) <- "VariableDefinition"
    out
}

#' Title
#'
#' @param expr 
#'
#' @return
#' @export
#'
#' @examples
validateExpression <- function(expr) {
    if (length(expr) != 3) {
        halt("Invalid expression, use the form ds$var ~ c(10, 20, 30)")
    }
    var     <- eval(expr[[2]])
    varname <- deparse(expr[[2]])
    targets <- eval(expr[[3]]) 
    if (!is.Categorical(var)) {
        halt(paste(varname, "is not a categorical crunch variable"))
    }
    n_categories <- length(categories(var))
    if (length(targets) != n_categories) {
        halt(paste("Number of targets does not match number of categories for", varname))
    }
    if (sum(targets) != 100) {
        halt(paste("Targets do not add up to 100% for", varname))
    }
    invisible(expr)
}

#' Title
#'
#' @param expr 
#'
#' @return
#' @export
#'
#' @examples
generateWeightEntry <- function(expr) {
    var <- eval(expr[[2]])
    targets <- eval(expr[[3]]) / 100
    id <- self(var)
    
    target_list <- vector("list", length(targets))
    for (i in seq_along(targets)) {
        target_list[[i]] <- c(i, targets[i])
    }
    list(
        variable = self(var),
        targets = target_list
    )
}