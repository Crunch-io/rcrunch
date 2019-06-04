#' Dataset weights
#'
#' `weight` lets you view and set your user's currently applied weight on the
#' server. `weightVariables` lets you view all of the variables that have been
#' designated as valid to use as weights.
#' @param x a Dataset
#' @param value a Variable, VariableDefinition, or `NULL`. If a
#' VariableDefinition is passed, the variable will first be created and then set
#' as the datasets weight. Set to `NULL` to remove existing weights from the
#' dataset.
#' @return For the `weight` getter, a Variable if there is a weight, else
#' NULL. For the setter, x, modified accordingly. `weightVariables` returns
#' the aliases (or names, according to `options(crunch.namekey.dataset)`),
#' of the variables designated as weights.
#' @seealso [weightVariables()] [makeWeight()]
#' @name weight
#' @aliases is.weight<-
#' @export
weight <- function(x) {
    stopifnot(is.dataset(x))
    prefs <- ShojiEntity(crGET(shojiURL(x, "fragments", "preferences")))
    w <- prefs$weight
    if (!is.null(w)) {
        # we need the full dataset here because we might have been given a
        # subset of the dataset, and we still need to be able to find the
        # weight. `loadDataset` shouldn't be costly because the dataset is
        # already cached.
        full_ds <- loadDataset(datasetReference(x))
        w <- CrunchVariable(allVariables(full_ds)[[w]], filter = activeFilter(x))
    }
    return(w)
}

#' @rdname weight
#' @export
`weight<-` <- function(x, value) {
    stopifnot(is.dataset(x))

    if (inherits(value, "VariableDefinition")) {
        x <- addVariables(x, value)
        value <- x[[value$name]]
    }
    if (is.variable(value)) {
        value <- self(value)
    } else if (!is.null(value)) {
        halt("Weight must be a Variable or NULL")
    }
    currentWeight <- ShojiEntity(crGET(shojiURL(x, "fragments", "preferences")))$weight
    if (!identical(value, currentWeight)) {
        crPATCH(shojiURL(x, "fragments", "preferences"),
            body = toJSON(list(weight = value))
        )
        x <- refresh(x)
    }
    return(x)
}

#' @rdname weight
#' @export
is.weight <- function(x) {
    if (is.variable(x)) {
        ds <- loadDataset(datasetReference(x))
        return(identical(weight(ds), x))
    } else {
        return(FALSE)
    }
}

#' @rdname weight
#' @export
setMethod("is.weight<-", "NumericVariable", function(x, value) {
    stopifnot(is.TRUEorFALSE(value))
    ds <- loadDataset(datasetReference(x))
    if (value) {
        weight(ds) <- x
    } else if (is.weight(x)) {
        weight(ds) <- NULL
    }
    return(x)
})

#' Get a dataset's weightVariables
#'
#' @param x a CrunchDataset
#' @return `weightVariables` returns a character vector of the aliases of the
#' variables that are eligible to be used as weights.
#' @seealso [weight()] [makeWeight()] [modifyWeightVariables()]
#' @name weightVariables
#'
#' @export
NULL

#' @rdname weightVariables
#' @export
setMethod(
    "weightVariables", "CrunchDataset",
    function(x) weightVariables(allVariables(x))
)

#' @rdname weightVariables
#' @export
setMethod("weightVariables", "VariableCatalog", function(x) {
    ## Get weight variable order
    ord <- VariableOrder(crGET(shojiURL(x, "orders", "weights")))
    ## Subset weight variable catalog with it
    vars <- x[ord]
    ## Return the names/aliases
    if (length(vars)) {
        return(sort(vapply(index(vars), vget(namekey(x)), character(1),
            USE.NAMES = FALSE
        )))
    } else {
        return(c())
    }
})

#' Change which variables can be set as a dataset's weight.
#'
#' `modifyWeightVariables` allows you to change the variables which are eligible
#' to be used as a dataset's weight. You can also add variables to the weight variables
#' catalog by assignment with `weightVariables(ds) <- "weight"` or
#' `is.weightVariable(ds$weight) <- TRUE`.
#'
#' Editors can change which variables can be set as the weighting variable
#' for a dataset. For instance if several weights have been calculated they
#' can let the user choose which of those variables to use a weight, but prevent
#' the user from choosing other variables as weight. This function allows you
#' to change the `weightVariables` of a dataset.
#'
#' @param x a CrunchDataset
#' @param vars Variables to add or remove  this can be a numeric Crunch variable,
#' list of numeric Crunch variables or a character vector with the aliases of
#' numeric Crunch variables. Setting vars to `NULL` clears a datasets `weightVariables`
#' @param value For the `weightVariables()` and `is.weightVariable` setters the
#' variables to append to a dataset's weightVariables.
#' @param type a character string determining how the weightVariables
#' will be modified:
#' - `"append"` : add `vars` to the current weight variables
#' - `"remove"` : remove `vars` from the current list of weight variables
#' - `"replace"`: replace the current weight variables with `vars`
#' @return a CrunchDataset
#' @name modifyWeightVariables
#' @aliases is.weightVariable<- weightVariables<-
#' @examples
#' \dontrun{
#' modifyweightVariables(ds, "weight", "append")
#' weightVariables(ds) <- list(ds$weight, ds$weight2)
#' weightVariables(ds) <- NULL
#' weightVariables(ds) <- c("weight", "weight2")
#' is.weightVariables(ds$weight) <- TRUE
#' }
#' @export
modifyWeightVariables <- function(x, vars, type = "append") {
    varcat <- allVariables(x)
    new <- old <- crGET(shojiURL(varcat, "orders", "weights"))

    ## Values can be NULL in order to clear the weight variables, a
    ## character vector to indicate the aliases which should be set as weight variables,
    ## a single variable to add to the list of weight variables or a list of the
    ## variables that you want to set as weightVariable. This processes those
    ## variaous inputs into a list of variables.
    if (is.null(vars)) {
        # If NULL change type to replace to clear the weight variables
        new$graph <- NULL
        type <- "replace"
    } else {
        if (is.variable(vars) || (length(vars) == 1) & !is.character(vars)) {
            ## Wrap single variables in a list
            vars <- list(vars)
        }
        if (is.character(vars)) {
            ## Get variables from aliases
            vars <- lapply(vars, function(v) {
                if (v %in% names(varcat)) {
                    x[[v]]
                } else {
                    ## strings which aren't aliases need to be returned for errors
                    v
                }
            })
        }
        all_var <- vapply(vars, is.Numeric, logical(1))
        if (!all(all_var)) {
            var_names <- vapply(vars, function(v) {
                if (is.variable(v)) {
                    return(name(v))
                } else {
                    return(as.character(v))
                }
            }, character(1))
            err_text <- " is not a numeric Crunch variable."
            if (sum(!all_var) > 1) {
                err_text <- " are not numeric Crunch variables."
            }
            halt(serialPaste(var_names[!all_var]), err_text)
        }
        if (type == "append") {
            new$graph <- unique(c(old$graph, lapply(vars, self)))
        } else if (type == "remove") {
            new$graph <- setdiff(old$graph, lapply(vars, self))
        } else if (type == "replace") {
            new$graph <- lapply(vars, self)
        }
    }
    if (!identical(old, new)) {
        crPUT(shojiURL(varcat, "orders", "weights"),
            body = toJSON(new)
        )
    }
    return(x)
}

#' @rdname modifyWeightVariables
#' @export
setMethod("weightVariables<-", "CrunchDataset", function(x, value) {
    return(modifyWeightVariables(x, value, type = "append"))
})

#' @rdname modifyWeightVariables
#' @export
is.weightVariable <- function(x) {
    if (is.variable(x)) {
        ds <- loadDataset(datasetReference(x))
        return(alias(x) %in% weightVariables(ds))
    } else {
        return(FALSE)
    }
}

#' @rdname modifyWeightVariables
#' @export
setMethod("is.weightVariable<-", "NumericVariable", function(x, value) {
    stopifnot(is.TRUEorFALSE(value))
    ds <- loadDataset(datasetReference(x))
    ds <- modifyWeightVariables(ds, x, ifelse(value, "append", "remove"))
    return(x)
})

#' Generate a weight variable
#'
#' This function allows you to generate a weight variable by supplying a set of
#' categorical variables and the target distribution for each of the variables'
#' categories. Weights are computed by iteratively 'raking' conditional 'cells'
#' to the provided marginal targets.
#'
#' For instance, if you wanted to create a
#' weight variable which equally weighted four categories stored in `ds$var`
#' you would call `ds$weight1 <- makeWeight(ds$var ~ c(25, 25, 25, 25), name = "weight1")`.
#' Note that `makeWeight` returns a `VariableDefinition`, an expression that
#' when assigned into your Dataset becomes a derived variable. This does not on
#' its own set the new variable as "the weight" for your dataset. To set that
#' attribute, use [weight()]. Alternatively, you can also create the variable
#' and set the weight attribute in one step with
#' `weight(ds) <- makeWeight(ds$var ~ c(25, 25, 25, 25), name = "weight1")`.
#'
#' @param ... A series of expressions of the form `variable ~ target_weights`.
#' The variable must be a categorical Crunch variable, and the target weights
#' must be a numeric vector whose length should be equal to the number of
#' categories contained in the variable, and whose sum is equal to 100 or 1. If
#' you supply fewer target weights than there are categories `makeWeight` will
#' pad the target weight vector with 0s.
#'
#' @param name The name of the resulting variable
#' @rdname makeWeight
#' @return
#' A crunch [VariableDefinition()] of the weight variable
#' @export
#'
#' @examples
#' \dontrun{
#' mtcars$cyl <- as.factor(mtcars$cyl)
#' mtcars$gear <- as.factor(mtcars$gear)
#' ds <- newDataset(mtcars)
#' # Create a new "raked" variable
#' ds$weight <- makeWeight(ds$cyl ~ c(30, 30, 40, 0),
#'     ds$gear ~ c(20, 20, 60, 0),
#'     name = "weight"
#' )
#' summary(ds$weight)
#' # ds$weight is not "the weight" for the dataset unless you set it:
#' weight(ds) <- ds$weight
#' # Or, you can create the variable and set as weight in one step:
#' weight(ds) <- makeWeight(ds$var ~ c(25, 25, 25, 25), name = "weight2")
#' }
#' @seealso [weight<-()]; [settings()] for the "default weight" for other
#' dataset viewers.
makeWeight <- function(..., name) {
    all_dots <- list(..., name = name)
    named_entries <- names(all_dots) != ""
    out <- all_dots[named_entries]
    expr_list <- all_dots[!named_entries]

    # args below must be an unnamed list, so we remove the names from expr_list
    names(expr_list) <- NULL
    out$derivation <- list(
        `function` = "rake",
        args = lapply(expr_list, generateWeightEntry)
    )
    class(out) <- "VariableDefinition"
    return(out)
}

#' Generate entry for [makeWeight]
#'
#' Utility function to catch formula errors passed to [makeWeight], and to
#' generate the appropriate entry.
#'
#' @param expr An expression
#' @return A list of the variable id and the target weights.
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' validateWeightExpression(ds$var ~ c(10, 30, 60))
#' }
generateWeightEntry <- function(expr) {
    formula <- try(as.formula(expr), silent = TRUE)
    if (is.error(formula)) {
        halt(
            dQuote(substitute(expr)),
            " is not a valid formula. Use the form ds$var ~ c(50, 20, 30)"
        )
    }
    var <- eval(expr[[2]], environment(expr))
    varname <- deparse(expr[[2]])
    targets <- eval(expr[[3]], environment(expr))

    if (!is.Categorical(var)) {
        halt(varname, " is not a categorical Crunch variable")
    }

    n_categories <- length(categories(var))

    if (length(targets) > n_categories) {
        halt(
            "Number of targets does not match number of categories for ",
            varname
        )
    }
    # Pad with zeros if the user hasn't suppied enough categories
    if (length(targets) < n_categories) {
        targets <- c(targets, rep(0, n_categories - length(targets)))
    }
    if (!all(is.numeric(targets))) {
        halt("Targets are not numeric for ", varname)
    }
    if (any(is.na(targets))) {
        halt(dQuote(deparse(expr)), " contains NA values.")
    }
    if (!(sum(targets) == 100 || sum(targets) == 1)) {
        halt("Targets do not add up to 100% for ", varname)
    }
    if (sum(targets) != 1) {
        targets <- targets / 100
    }

    # the targets must be the ids of the categories to align correctly
    target_list <- mapply(
        function(id, target) c(id, target),
        id = ids(categories(var)),
        target = targets,
        SIMPLIFY = FALSE
    )

    return(list(
        variable = self(var),
        targets = target_list
    ))
}
