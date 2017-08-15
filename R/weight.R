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

#' Generate a weight vector
#'
#' This function allows you to generate a weight vector by supplying a set of
#' categorical variables and the target distribution for each of the variable's categories.
#' For instance if you wanted to create a weight variable which equally weighted four categories stored
#' in `ds$var` you would call `makeWeight(ds$var ~ c(25, 25, 25, 25), name = "weight")`.
#'
#' @param ...
#' A series of expressions of the form `variable ~ target_weights`. The variable must
#' be a categorical crunch variable, and the target weights must be a numeric vector whose
#' length is equal to the number of categories contained in the variable, and whose sum is equal to 100.
#'
#' @param name
#' The name of the resulting variable
#' @rdname makeWeight
#' @return
#' A crunch Variable Definiton of the weight variable
#' @export
#'
#' @examples
#' \dontrun{
#' mtcars$cyl <- as.factor(mtcars$cyl)
#' mtcars$gear <- as.factor(mtcars$gear)
#' ds <- newDataset(cars, "cars_fact")
#' ds$weight <- makeWeight(ds$cyl ~ c(30, 30, 40, 0), ds$gear ~ c(20, 20, 60, 0), name = "weight" )
#' as.vector(ds$weight)
#' }
makeWeight <- function(..., name) {
    expr_list <- list(...)
    lapply(expr_list, validateWeightExpression)
    out <- list(
        name = name,
        derivation = list(
            `function` = "rake",
            args = lapply(expr_list, generateWeightEntry))
        )
    class(out) <- "VariableDefinition"
    out
}

#' Validate an expression passed to makeWeight
#'
#' Utility function to catch formula errors passed to [makeWeight].
#'
#' @param expr
#' An expression
#' @return
#' NULL value, function is called for its side effects
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' validateWeightExpression(ds$var ~ c(10, 30, 60))
#' }
#'
validateWeightExpression <- function(expr) {
    if (length(expr) != 3) {
        halt(paste(expr, "is an invalid expression, use the form ds$var ~ c(10, 20, 30)"))
    }
    var     <- eval(expr[[2]], environment(expr))
    varname <- deparse(expr[[2]])
    targets <- eval(expr[[3]])
    n_categories <- length(categories(var))
    if (n_categories == 0) {
        halt(paste(varname, "is not a categorical crunch variable"))
    }
    if (length(targets) != n_categories) {
        halt(paste("Number of targets does not match number of categories for", varname))
    }
    if (!all(is.numeric(targets))) {
        halt(paste("Targets are not numeric for", varname))
    }
    if (sum(targets) != 100) {
        halt(paste("Targets do not add up to 100% for", varname))
    }
    invisible(expr)
}

#' Utility function to generate entries for makeWeight
#'
#' @param expr
#' An expression
#' @keywords internal
#' @return
#' A list of the variable id and the target weights.
#'
#'
generateWeightEntry <- function(expr) {
    #TODO use formula evaluation functions like evalLHS when PR #65 is merged
    var <- eval(expr[[2]], environment(expr))
    targets <- eval(expr[[3]]) / 100

    target_list <- vector("list", length(targets))
    for (i in seq_along(targets)) {
        target_list[[i]] <- c(i, targets[i])
    }

    list(
        variable = self(var),
        targets = target_list
    )
}