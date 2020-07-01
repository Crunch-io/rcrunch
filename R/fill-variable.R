#' Fill a variable from another
#'
#' Given a categorical variable, assign one or more categories to be filled in by
#' another existing variable or crunch expression. (Categories not filled in will
#' remain unchanged).
#'
#' @param x a Categorical (Array) variable
#' @param fills a list of lists that each have a "fill" item that is a variable or expression
#'   as well as one of an "id", "name", or "value" that will be matched to the categories of x
#'   If x is an expression, the fills must be an id
#' @param ... A sequence of named expressions or variables to use as fills, where the name will be
#'   matched to the existing categories
#' @param data (optional) a crunch dataset to use. Specifying this means you don't have to put
#'   `dataset$` in front of each variable name
#' @param name A character to use as the name of the variable to create
#'
#' @return A [`VariableDefinition`] that will create the new fill variable when assigned into
#'   the Dataset.
#' @export
#'
#' @examples
#' \dontrun{
#' makeFillVariable(
#'     ds$v1,
#'     fills = list(
#'         list(fill = ds$v2, name = "dog")
#'     ),
#'     name = "new fill"
#' )
#' makeFillVariable(v1, "dog" = ds$v2, name = "new fill")
#'
#' # the dataset can be specified with data=
#' makeFillVariable(v1, "dog" = v2, data = ds, name = "new fill")
#' }
makeFillVariable <- function(x, fills, ..., data = NULL, name) {
    x <- evalSide(substitute(x), data = data, eval_env = parent.frame())
    dots <- as.list(substitute(list(...)))[-1L]
    dots <- lapply(dots, evalSide, dat = data, eval_env = parent.frame())
    is_expr_or_var <- function(x) {
        is.Expr(x) || is.variable(x)
    }

    args <- Filter(is_expr_or_var, dots)
    args$x <- x
    if (!missing(fills)) {
        fills <- evalSide(substitute(fills), data, parent.frame())
        args$fills <- fills
    }
    derivation <- do.call(fillExpr, args)
    meta <- Filter(Negate(is_expr_or_var), dots)

    do.call(VarDef, c(meta, list(name = name, data = derivation)))
}

#' @rdname makeFillVariable
#' @export
fillExpr <- function(x, fills, ...) {
    dots <- list(...)
    if (length(dots) == 0 & missing(fills)) {
        halt("Must pass either named arguments to ... or a list to fills")
    }
    if (length(dots) > 0 & !missing(fills)) {
        halt("Cannot pass both named arguments to ... and a list to fills")
    }

    if (length(dots) > 0) {
        fills <- lapply(
            seq_along(dots),
            function(dot_num) list(name = names(dots)[dot_num], fill = dots[[dot_num]])
        )
    }

    fill_map <- lapply(fills, conform_fill_list)
    categories <- if (is.variable(x)) categories(x) else NULL
    names(fill_map) <- vapply(fills, conform_fill_id, categories = categories, "")


    zfuncExpr("fill", x, list(map = fill_map))
}

conform_fill_list <- function(x) {
    if (!("fill" %in% names(x))) {
        halt("All fills must have a fill variable or expression")
    }

    zcl(x$fill)
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
