#' @include variable-definition.R
NULL

#' @rdname toVariable
#' @export
setMethod("toVariable", "character", function (x, ...) {
    return(structure(list(values=x, type="text", ...),
        class="VariableDefinition"))
})
#' @rdname toVariable
#' @export
setMethod("toVariable", "numeric", function (x, ...) {
    return(structure(list(values=x, type="numeric", ...),
        class="VariableDefinition"))
})
#' @rdname toVariable
#' @export
setMethod("toVariable", "factor", function (x, ...) {
    nlevels <- length(levels(x))
    out <- structure(list(values=as.integer(x), type="categorical",
        categories=categoriesFromLevels(levels(x)), ...),
        class="VariableDefinition")
    return(NAToCategory(out, useNA="always"))
})
#' @rdname toVariable
#' @export
setMethod("toVariable", "Date", function (x, ...) {
    return(structure(list(values=as.character(x), type="datetime",
        resolution="D", ...),
        class="VariableDefinition"))
})
#' @rdname toVariable
#' @export
setMethod("toVariable", "POSIXt", function (x, ...) {
    return(structure(list(values=strftime(x, "%Y-%m-%dT%H:%M:%OS3"),
        type="datetime",
        resolution="ms", ...),
        class="VariableDefinition"))
})

#' @rdname toVariable
#' @export
setMethod("toVariable", "AsIs", function (x, ...) {
    class(x) <- class(x)[-match("AsIs", class(x))]
    return(toVariable(x, ...))
})

#' @rdname toVariable
#' @export
setMethod("toVariable", "VariableDefinition", function (x, ...) {
    return(modifyList(x, list(...)))
})
#' @rdname toVariable
#' @export
setMethod("toVariable", "logical", function (x, ...) {
    ## Make it categorical
    out <- structure(list(values=2L-as.integer(x), type="categorical",
        categories=categoriesFromLevels(c("True", "False")),
        ...),
        class="VariableDefinition")
    return(NAToCategory(out, useNA="always"))
})

labelledToVariable <- function (x, ...) {
    # TODO: check that forcats is installed

    x_values <- as.vector(x)
    x_factor <- forcats::as_factor(x)
    # grab the user missing levels
    user_missings <- levels(droplevels(x_factor[is.na(x)]))

    if (length(user_missings) == 0) {
        user_missings <- NULL
    }

    if (is.numeric(x_values)) {
        numeric_values <- unique(x_values)
        # don't keep any of the NAs
        # TODO: see if this will work with user-missing
        numeric_values <- numeric_values[!is.na(numeric_values)]
    } else {
        numeric_values <- NULL
    }

    out <- structure(
        list(values=as.integer(x_factor), type="categorical",
             categories=categoriesFromLevels(
                 levels(x_factor),
                 numeric_values = numeric_values,
                 missing_levels = user_missings),
             ...),
        class="VariableDefinition")
    return(NAToCategory(out, useNA="always"))
}

# haven::labelled* are S3 classes, so we have to register them
setOldClass("labelled")
#' @rdname toVariable
#' @export
setMethod("toVariable", "labelled", labelledToVariable)

setOldClass("labelled_spss")
#' @rdname toVariable
#' @export
setMethod("toVariable", "labelled_spss", labelledToVariable)

#' Convert a factor's levels into Crunch categories.
#'
#' Crunch categorical variables have slightly richer metadata than R's
#' factor variables. This function generates a list of category data from
#' a factor's levels which can then be further manipulated in R before being
#' imported into Crunch.
#'
#' @param level_vect A character vector containing the levels of a factor. Usually
#' obtained by running [base::levels()]
#' @param numeric_values A numeric vector containing the numeric values to be
#' used. (Default: `NULL`, which will use the ids as default numeric values)
#' @param missing_levels A character vector containing any of the levels that
#' should be marked missing (Default: `NULL`, which will mark none missing)
#'
#' @return A list with each category levels id, name, numeric_value, and missingness.
#' @rdname categoriesFromLevels
#' @export
#'
#' @examples
#'
#' categoriesFromLevels(levels(iris$Species))
#'
categoriesFromLevels <- function (level_vect,
                                  numeric_values = NULL,
                                  missing_levels = NULL) {
    if (anyDuplicated(level_vect)) {
        warning("Duplicate factor levels given: disambiguating them ",
            "in translation to Categorical type")
        level_vect <- uniquify(level_vect)
    }

    # TODO: check if lengths of numeric_values and missing are the same.
    if (is.null(numeric_values)) {
        numeric_values <- seq_along(level_vect)
    }

    if (is.null(missing_levels)) {
        missings <- FALSE
    } else {
        missings <- level_vect %in% missing_levels
    }

    cat_metadata <- mapply(
        function (i, n, m) {
            list(id=i, name=level_vect[i], numeric_value=n, missing=m)
        },
        i = seq_along(level_vect), n = numeric_values, m = missings,
        SIMPLIFY = FALSE)

    return(cat_metadata)
}

NAToCategory <- function (var.metadata, useNA=c("ifany", "always")) {
    useNA <- match.arg(useNA)
    if (useNA == "always" || any(is.na(var.metadata$values))) {
        var.metadata$values[is.na(var.metadata$values)] <- -1L
        var.metadata$categories[[length(var.metadata$categories)+1]] <- .no.data
    }
    return(var.metadata)
}
