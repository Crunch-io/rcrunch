#' Expression to fill a variable from another
#'
#' Given a categorical variable, assign one or more categories to be filled in by
#' another existing variable or crunch expression. (Categories not filled in will
#' remain unchanged ).
#'
#' @param x a Categorical (Array) variable
#' @param fills a list of lists that each have a "fill" item that is a variable or expression
#'   as well as one of an "id", "name", or "value" that will be matched to the categories of x
#'   If x is an expression, the fills must be an id
#' @param ... A sequence of named expressions or variables to use as fills, where the name will be
#'   matched to the existing categories
#' @param data (optional) a crunch dataset to use. Specifying this means you don't have to put
#'   `dataset$` in front of each variable name
#' @param type The type of the variable to output (either "categorical" or "numeric"), only
#' required if all fills are expressions and so their type cannot be guessed automatically.
#' @return A `CrunchExpression` that assigns categories to filling variables
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' fillExpr(
#'     ds$v1,
#'     fills = list(
#'         list(fill = ds$v2, name = "dog")
#'     )
#' )
#' fillExpr(v1, "dog" = ds$v2)
#'
#' # the dataset can be specified with data=
#' fillExpr(v1, "dog" = v2, data = ds)
#' }
fillExpr <- function(x, fills, ..., data = NULL, type = NULL) {
    dots <- as.list(substitute(list(...)))[-1L]

    if (length(dots) == 0 & missing(fills)) {
        halt("Must pass either named arguments to ... or a list to fills")
    }
    if (length(dots) > 0 & !missing(fills)) {
        halt("Cannot pass both named arguments to ... and a list to fills")
    }

    x <- evalSide(substitute(x), data = data, eval_env = parent.frame())

    if (length(dots) > 0) {
        dots <- lapply(dots, evalSide, dat = data, eval_env = parent.frame())
        fills <- lapply(
            seq_along(dots),
            function(dot_num) list(name = names(dots)[dot_num], fill = dots[[dot_num]])
        )
    } else {
        fills <- evalSide(substitute(fills), data, parent.frame())
    }
    fill_map <- lapply(fills, conform_fill_list)
    type <- validate_fill_types(fills, type)
    categories <- if (is.variable(x)) categories(x) else NULL
    names(fill_map) <- vapply(fills, conform_fill_id, categories = categories, "")

    func_name <- if (type == "numeric") "numeric_fill" else "fill"

    zfuncExpr(func_name, x, list(map = fill_map))
}

validate_fill_types <- function(fills, type) {
    actual_fill_types <- vapply(fills, function(x) {
        if (is.variable(x$fill)) {
            return(type(x$fill))
        } else if (is.numeric(x$fill)) {
            return("numeric")
        } else {
            return(NA_character_)
        }
    }, character(1))

    unique_fill_types <- unique(actual_fill_types[!is.na(actual_fill_types)])

    if (!is.null(type) && length(unique_fill_types) == 0) {
        return(type)
    }

    if (!is.null(type) && any(unique_fill_types != type)) {
        halt("Fills must all be of type ", dQuote(type))
    }

    if (length(unique_fill_types) > 1) {
        halt("Fills must all be of the same type")
    }

    if (length(unique_fill_types) == 0) {
        halt("If all fills are expressions, must provide the `type`")
    }

    unique_fill_types
}

conform_fill_list <- function(x) {
    if (!("fill" %in% names(x))) {
        halt("All fills must have a fill variable or expression")
    }

    out <- zcl(x$fill)
    # Possibly a bug in zcl, but need to send type with numeric_fill when
    # sending a value
    if (is.numeric(x$fill)) out$type <- "numeric"
    out
}

conform_fill_id <- function(x, categories) {
    if ("id" %in% names(x) && is.null(categories)) {
        # No validation possible
        return(as.character(x$id))
    }

    if (is.null(categories)) {
        halt("fills must specify id when categories are not available")
    }

    if ("id" %in% names(x)) {
        match <- ids(categories) %in% x$id
        attempt <- x$id
        type_used <- "id"
    } else if ("name" %in% names(x)) {
        match <- names(categories) %in% x$name
        attempt <- x$name
        type_used <- "name"
    } else if ("value" %in% names(x)) {
        match <- values(categories) %in% x$value
        attempt <- x$value
        type_used <- "value"
    } else {
        halt("All fills must have a category id, name or value assigned to it")
    }

    if (sum(match) != 1) {
        halt("category ", type_used, " '", attempt, "' does not uniquely identify a category.")
    }

    as.character(ids(categories)[match])

}
