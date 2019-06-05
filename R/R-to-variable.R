#' @include variable-definition.R
NULL

#' Generic method for converting objects to Crunch representations
#'
#' R objects are converted to Crunch objects using the following rules:
#'
#' - Character vectors are converted into Crunch text variables
#' - Numeric vectors are converted into Crunch numeric variables
#' - Factors are converted to categorical variables
#' - Date and POSIXt vectors are converted into Crunch datetime variables
#' - Logical vectors are converted to Crunch categorical variables
#' - [VariableDefinition()]s are not converted, but the function can still
#' append additional metadata
#'
#' If you have other object types you wish to convert to Crunch variables,
#' you can declare methods for `toVariable`.
#' @param x An R vector you want to turn into a Crunch variable
#' @param ... Additional metadata fields for the variable, such as "name" and
#' "description". See the [API documentation](http://docs.crunch.io/endpoint-reference/endpoint-variable.html#post-catalog)
#' for a complete list of valid attributes.
#' @return A `VariableDefinition` object. To add this to a dataset, either
#' assign it into the dataset (like `ds$newvar <- toVariable(...)`) or call
#' [addVariables()]. If you're adding a column of data to a dataset, it must be
#' as long as the number of rows in the dataset, or it may be a single value to
#' be recycled for all rows.
#' @rdname toVariable
#' @aliases toVariable
#' @seealso [VariableDefinition()] [addVariables()]
#' @examples
#' var1 <- rnorm(10)
#' toVariable(var1)
#' toVariable(var1, name = "Random", description = "Generated in R")
#' \dontrun{
#' ds$random <- toVariable(var1, name = "Random")
#' # Or, this way:
#' ds <- addVariables(ds, toVariable(var1, name = "Random"))
#' }
#' @export
setGeneric("toVariable", function(x, ...) standardGeneric("toVariable"))

#' @rdname toVariable
#' @export
setMethod("toVariable", "CrunchExpr", function(x, ...) {
    structure(list(derivation = zcl(x), ...), class = "VariableDefinition")
})

#' @rdname toVariable
#' @export
setMethod("toVariable", "character", function(x, ...) {
    return(VariableDefinition(values = x, type = "text", ...))
})
#' @rdname toVariable
#' @export
setMethod("toVariable", "numeric", function(x, ...) {
    return(VariableDefinition(values = x, type = "numeric", ...))
})
#' @rdname toVariable
#' @export
setMethod("toVariable", "factor", function(x, ...) {
    return(VariableDefinition(
        values = as.categorical.values(x), type = "categorical",
        categories = categoriesFromLevels(levels(x)), ...
    ))
})
#' @rdname toVariable
#' @export
setMethod("toVariable", "Date", function(x, ...) {
    return(VariableDefinition(
        values = as.character(x), type = "datetime",
        resolution = "D", ...
    ))
})
#' @rdname toVariable
#' @export
setMethod("toVariable", "POSIXt", function(x, ...) {
    return(VariableDefinition(
        values = strftime(x, "%Y-%m-%dT%H:%M:%OS3"),
        type = "datetime",
        resolution = "ms", ...
    ))
})

#' @rdname toVariable
#' @export
setMethod("toVariable", "AsIs", function(x, ...) {
    class(x) <- class(x)[-match("AsIs", class(x))]
    return(toVariable(x, ...))
})

#' @rdname toVariable
#' @export
setMethod("toVariable", "VariableDefinition", function(x, ...) {
    return(modifyList(x, list(...)))
})
#' @rdname toVariable
#' @export
setMethod("toVariable", "logical", function(x, ...) {
    vals <- as.categorical.values(x)
    cats <- .selected.cats
    ## Pre-3VL category names
    ## Note that with the extra strict definition of `is.3vl`, this won't
    ## register as a "logical" type yet and so as.vector will continue to return
    ## this as categorical, not logical
    cats[[1]]$name <- "True"
    cats[[2]]$name <- "False"
    return(VariableDefinition(
        values = vals, type = "categorical",
        categories = cats, ...
    ))
})

# haven::haven_labelled* are S3 classes, so we have to register them the
# labelled* classes are depricated in haven 2.0, but we are keeping them here
# for backwards and forward compatibility
setOldClass("labelled")
setOldClass("haven_labelled")

haven_labelled_func <- function(x, ...) {
    # TODO: what if the values are numeric? Is it possible to tell these apart
    # from the labelled object?
    return(toVariable(as.factor(x), ...))
}

#' @rdname toVariable
#' @export
setMethod("toVariable", "labelled", haven_labelled_func)

#' @rdname toVariable
#' @export
setMethod("toVariable", "haven_labelled", haven_labelled_func)


setOldClass("labelled_spss")
setOldClass("haven_labelled_spss")

haven_labelled_spss_func <- function(x, ...) {
    # TODO: what if the values are numeric? Is it possible to tell these apart
    # from the labelled object?

    # convert to factor quickly (the recommended workflow for labelled objects
    # from haven, since there are few methods for labelled objects)
    x_factor <- as.factor(x)

    categories <- categoriesFromLevels(levels(x_factor))
    # grab the user missing levels
    user_missings <- levels(droplevels(x_factor[is.na(x)]))
    # we aren't
    categories <- lapply(categories, function(cat) {
        if (cat$name %in% user_missings) {
            cat$missing <- TRUE
        }
        return(cat)
    })

    return(VariableDefinition(
        values = as.categorical.values(x_factor),
        type = "categorical",
        categories = categories,
        ...
    ))
}

#' @rdname toVariable
#' @export
setMethod("toVariable", "labelled_spss", haven_labelled_spss_func)

#' @rdname toVariable
#' @export
setMethod("toVariable", "haven_labelled_spss", haven_labelled_spss_func)

as.categorical.values <- function(x) {
    vals <- as.integer(x)
    vals[is.na(vals)] <- -1L
    return(vals)
}

#' Convert a factor's levels into Crunch categories.
#'
#' Crunch categorical variables have slightly richer metadata than R's
#' factor variables. This function generates a list of category data from
#' a factor's levels which can then be further manipulated in R before being
#' imported into Crunch.
#'
#' @param level_vect A character vector containing the levels of a factor. Usually
#' obtained by running [base::levels()]
#'
#' @return A list with each category levels id, name, numeric_value, and missingness.
#' @rdname categoriesFromLevels
#' @export
#'
#' @examples
#'
#' categoriesFromLevels(levels(iris$Species))
categoriesFromLevels <- function(level_vect) {
    if (anyDuplicated(level_vect)) {
        warning(
            "Duplicate factor levels given: disambiguating them ",
            "in translation to Categorical type"
        )
        level_vect <- uniquify(level_vect)
    }
    return(c(lapply(seq_along(level_vect), function(i) {
        list(id = i, name = level_vect[i], numeric_value = i, missing = FALSE)
    }), list(.no.data)))
}
