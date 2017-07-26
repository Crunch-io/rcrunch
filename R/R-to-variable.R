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

#' Convert a factor's levels into Crunch categories. 
#' 
#' Crunch categorical variables have slightly richer metadata than R's
#' factor variables. This function generates a list of category data from
#' a factor's levels which can then be further manipulated in R before being
#' imported into Crunch.
#'
#' @param levels A character vector containing the levels of a factor. Usually
#' obtained by running \code{levels()}
#'
#' @return A list with each category levels id, name, numeric_value, and missingness. 
#' @rdname categoriesFromLevels
#' @export
#'
#' @examples
#' 
#' categoriesFromLevels( levels(iris$Species))
#' 
categoriesFromLevels <- function(levels) {
    if (anyDuplicated(levels)) {
        warning("Duplicate factor levels given: disambiguating them ",
            "in translation to Categorical type")
        levels <- uniquify(levels)
    }
    return(lapply(seq_along(levels), function (i) {
        list(id=i, name=levels[i], numeric_value=i, missing=FALSE)
    }))
}

NAToCategory <- function (var.metadata, useNA=c("ifany", "always")) {
    useNA <- match.arg(useNA)
    if (useNA == "always" || any(is.na(var.metadata$values))) {
        var.metadata$values[is.na(var.metadata$values)] <- -1L
        var.metadata$categories[[length(var.metadata$categories)+1]] <- .no.data
    }
    return(var.metadata)
}
