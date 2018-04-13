#' @include variable-definition.R
NULL

#' @rdname toVariable
#' @export
setMethod("toVariable", "character", function (x, ...) {
    return(VariableDefinition(values=x, type="text", ...))
})
#' @rdname toVariable
#' @export
setMethod("toVariable", "numeric", function (x, ...) {
    return(VariableDefinition(values=x, type="numeric", ...))
})
#' @rdname toVariable
#' @export
setMethod("toVariable", "factor", function (x, ...) {
    return(VariableDefinition(values=as.integer(x), type="categorical",
        categories=categoriesFromLevels(levels(x)), ...))
})
#' @rdname toVariable
#' @export
setMethod("toVariable", "Date", function (x, ...) {
    return(VariableDefinition(values=as.character(x), type="datetime",
        resolution="D", ...))
})
#' @rdname toVariable
#' @export
setMethod("toVariable", "POSIXt", function (x, ...) {
    return(VariableDefinition(values=strftime(x, "%Y-%m-%dT%H:%M:%OS3"),
        type="datetime",
        resolution="ms", ...))
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
    if (getOption("crunch.3vl", FALSE)) {
        ## Make it 3VL categorical
        vals <- as.integer(x)
        vals[is.na(vals)] <- -1L
        return(VariableDefinition(values=vals, type="categorical",
            categories=.selected.cats, ...))
    } else {
        ## Pre-3VL actual categorical
        return(VariableDefinition(values=2L-as.integer(x), type="categorical",
            categories=categoriesFromLevels(c("True", "False")), ...))
    }
})

# haven::labelled* are S3 classes, so we have to register them
setOldClass("labelled")
#' @rdname toVariable
#' @export
setMethod("toVariable", "labelled", function (x, ...) {
    # TODO: what if the values are numeric? Is it possible to tell these apart
    # from the labelled object?
    return(toVariable(as.factor(x), ...))
})

setOldClass("labelled_spss")
#' @rdname toVariable
#' @export
setMethod("toVariable", "labelled_spss", function (x, ...) {
    # TODO: what if the values are numeric? Is it possible to tell these apart
    # from the labelled object?

    # convert to factor quickly (the recommended workflow for labelled objects
    # from haven, since there are few methods for labelled objects)
    x_factor <- as.factor(x)
    nlevels <- length(levels(x_factor))
    categories <- categoriesFromLevels(levels(x_factor))
    # grab the user missing levels
    user_missings <- levels(droplevels(x_factor[is.na(x)]))
    # we aren't
    categories <- lapply(categories, function (cat) {
        if (cat$name %in% user_missings) {
            cat$missing <- TRUE
        }
        return(cat)
    })

    return(VariableDefinition(values=as.integer(x_factor), type="categorical",
                          categories=categories, ...))
})


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
#'
categoriesFromLevels <- function (level_vect) {
    if (anyDuplicated(level_vect)) {
        warning("Duplicate factor levels given: disambiguating them ",
            "in translation to Categorical type")
        level_vect <- uniquify(level_vect)
    }
    return(c(lapply(seq_along(level_vect), function (i) {
        list(id=i, name=level_vect[i], numeric_value=i, missing=FALSE)
    }), list(.no.data)))
}
