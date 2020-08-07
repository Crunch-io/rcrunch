#' Create a variable from categorical variables or categories based on conditions
#'
#' Conditions are specified using a series of formulas: the left-hand side is
#' the condition that must be true (a `CrunchLogicalExpr`) and the right-hand
#' side is where to get the value if the condition on the left-hand side is
#' true. This must be either a Crunch Categorical variable or a Category.
#'
#' @param ... formulas where the left hand side is a `CrunchLogicalExpression` (or `TRUE`
#'   to indicate the "else" case that will be met if all the other expression are
#'   not met) and the right hand side is a CrunchVariable that should be filled in,
#'   a `Category` object describing the Category it should be used, a string
#'   which will be the name of the `Category` or `NA` to indicate that it should
#'   be replaced with the system missing value. For `makeCaseWhenVariable()`
#'   non-formula arguments will be passed to `[VarDef()]`
#' @param data A CrunchDataset to use if variable aliases are left bare in the
#'   formulas.
#' @param cases A list of formulas that match the description in `...` or a list of
#'   lists with named items, "expression" (like the left-hand side of the formulas above),
#'   "fill" for a variable to fill in, or "name", "id", and other items that describe a
#'   category.
#' @param name For `makeCaseWhenVariable()` the name of the variable to create.
#'
#' @return `makeCaseWhenVariable()` returns a `VariableDefinition` and
#'   `caseWhenExpr()` returns an expression
#' @export
#' @examples
#' \dontrun{
#' ds$new_var <- makeCaseWhenVariable(
#'    ds$x %in% c("a", "b") ~ ds$y, # can fill with a variable
#'    ds$x %in% c("c", "d") ~ Category(name = "c or d", numeric_value = 10), # or a Category
#'    # If none of the categories match, will be set to missing unless you
#'    # specify an "else" case with `TRUE` in the left hand side
#'    TRUE ~ Category(name = "catch all"),
#'    name = "combined x and y"
#' )
#'
#' ds$brand_x_pref <- makeCaseWhenVariable(
#'    ds$brand[[1]] == "Brand X" ~ ds$pref[[1]],
#'    ds$brand[[2]] == "Brand X" ~ ds$pref[[2]],
#'    ds$brand[[3]] == "Brand X" ~ ds$pref[[3]]
#'    name = "brand x preference"
#' )
#'
#' ds$x_among_aware <- makeCaseWhenVariable(
#'    ds$aware_x == "Yes" ~ ds$x,
#'    TRUE ~ Category(name = "(Not aware)", missing = TRUE),
#'    name = "x (among respondents aware of x)"
#' )
#'
#' # caseWhenExpr can be used inside other expressions
#' ds$brand_x_prefer_high <- VarDef(
#'    selectCategories(
#'        caseWhenExpr(
#'            ds$brand_shown[[1]] == "Brand X" ~ ds$ratings[[1]],
#'            ds$brand_shown[[2]] == "Brand X" ~ ds$ratings[[2]],
#'            ds$brand_shown[[3]] == "Brand X" ~ ds$ratings[[3]]
#'        ),
#'        c("Best", "Very Good")
#'    ),
#'    name = "Rate X highly"
#' )
#'
#' # Using lists in `cases` argument can be helpful when working programmatically
#' source_var <- ds$x
#' inclusion_condition <- ds$skipped_x != "Yes"
#'
#' ds$x2_among_aware <- makeCaseWhenVariable(
#'    cases = list(list(fill = source_var, expression = inclusion_condition)),
#'    name = "x2 among aware"
#' )
#' }
makeCaseWhenVariable <- function(..., data = NULL, cases = NULL, name) {
    dots <- list(...)
    formula_dots <- vapply(dots, function(x) inherits(x, "formula"), logical(1))

    args <- list(
        data = caseWhenExpr(data = data, cases = c(cases, unname(dots[formula_dots]))),
        name = name
    )
    args <- c(args, dots[!formula_dots])

    do.call(VarDef, args)
}

#' @export
#' @rdname makeCaseWhenVariable
caseWhenExpr <- function(..., data = NULL, cases = NULL) {
    cases <- unname(c(cases, list(...)))
    case_fills <- lapply(cases, parse_case_when_formula, data = data)

    # Get set of unique IDs that fill in for when IDs are missing
    used_ids <- vapply(case_fills, function(x) x$id %||% NA, numeric(1))
    case_ids <- used_ids
    case_ids[is.na(used_ids)] <- setdiff(
        seq_along(case_fills),
        used_ids
    )[seq_len(sum(is.na(used_ids)))]

    cases <- mapply(function(case_fill, case_id) {
        # Make a temporary cases for expressions that will be filled in
        if ("fill" %in% names(case_fill)) {
            list(
                expression = case_fill$expression,
                id = as.integer(case_id),
                name = paste0("casefill__internal", case_id)
            )
        } else {
            case_fill
        }
    }, case_fills, case_ids, SIMPLIFY = FALSE)

    need_fills <- vapply(case_fills, function(x) "fill" %in% names(x), logical(1))

    if (!any(need_fills)) return(caseExpr(cases = cases))

    fills <- lapply(which(need_fills), function(cf_num) {
        case_fill <- case_fills[[cf_num]]
        list(fill = case_fill$fill, id = case_ids[cf_num])
    })

    fillExpr(caseExpr(cases = cases), fills = fills)
}

parse_case_when_formula <- function(formula, data) {
    if (is.list(formula)) {
        if (identical(formula$expression, TRUE)) formula$expression <- "else"
        return(formula)
    }

    if (length(formula) != 3) {
        halt(
            "The condition provided must be a proper formula: ",
            deparseAndFlatten(formula)
        )
    }

    expr <- evalLHS(formula, data)
    if (!inherits(expr, c("logical", "CrunchLogicalExpr"))) {
        halt(
            "The left-hand side provided must be a logical or a ",
            "CrunchLogicalExpr: ", dQuote(LHS_string(formula))
        )
    }
    if (identical(expr, TRUE)) expr <- "else"

    rhs <- evalRHS(formula, data)
    if (is.variable(rhs)) {
        rhs <- list(fill = rhs)
    } else if (inherits(rhs, "Category")) {
        rhs <- lapply(rhs, identity)
    } else if (is.character(rhs)) {
        rhs <- list(name = rhs)
    } else if (is.na(rhs)) {
        list(name = "No Data", missing = TRUE)
    } else {
        halt(
            "The right-hand side provided must be a Category, CrunchVariable ",
            "string, or `NA`: ", dQuote(RHS_string(formula))
        )
    }

    c(list(expression = expr), rhs)
}
