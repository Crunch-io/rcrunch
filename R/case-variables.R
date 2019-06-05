#' Make a case variable
#'
#' The `makeCaseVariable` function derives a variable using values from other
#' variables. These are evaluated in the order they are supplied in the list
#' as the `cases` argument (they proceed in an IF, ELSE IF, ELSE IF, ..., ELSE
#' fashion); the first one that matches selects the corresponding value from
#' the case list.
#'
#' There are two ways to specify cases, but you must pick only one (note these
#' two will produce the same case variable):
#'
#' 1. When you just want to specify conditions, you can use named conditions:
#' `makeCaseVariable(case1=ds$v1 == 1, case2=ds$v2 == 2, name="new case")`
#'
#' 1. You can also use the `cases` argument, which is useful when you want to
#' provide category ids, numeric values, or missingness:
#' `makeCaseVariable(cases=list(list(expression=ds$v1 == 1, name="case1"), list(expression=ds$v2 == 2, name="case2")), name="new case")`
#'
#' Rows in the dataset that do not match any of the provided "cases" will
#' be assigned to an "else" category. By default, Crunch will use the system
#' missing "No Data" category. Alternatively, you can provide an else
#' case definition for these rows by including as the last "case" you provide
#' one with its `expression` set to the string "else". See the examples for
#' details.
#'
#' @param ... a sequence of named expressions to use as cases as well as other
#' properties to pass about the case variable (i.e. alias, description)
#' @param cases a list of lists with each case condition to use each must
#' include at least a `name` and an `expression` element. Cases may also include
#' `missing` (logical) and `numeric_value` (numeric).
#' @param data (optional) a crunch dataset to use. Specifying this means you
#' don't have to put `dataset$` in front of each variable name.
#' @param name a character to use as the name of the case variable to create
#'
#' @return A [`VariableDefinition`] that will create the new
#' case variable when assigned into the Dataset.
#' @examples
#' \dontrun{
#' makeCaseVariable(case1 = ds$v1 == 1, case2 = ds$v2 == 2, name = "new case")
#' makeCaseVariable(
#'     cases = list(
#'         list(expression = ds$v1 == 1, name = "case1"),
#'         list(expression = ds$v2 == 2, name = "case2")
#'     ),
#'     name = "new case"
#' )
#'
#' # different ways to specify else cases
#' makeCaseVariable(
#'     cases = list(
#'         list(expression = ds$v1 == 1, name = "case1"),
#'         list(expression = ds$v2 == 2, name = "case2"),
#'         list(expression = "else", name = "other")
#'     ),
#'     name = "new case"
#' )
#' makeCaseVariable(case1 = ds$v1 == 1, case2 = ds$v2 == 2, other = "else", name = "new case")
#'
#' # the dataset can be specified with data=
#' makeCaseVariable(case1 = v1 == 1, case2 = v2 == 2, data = ds, name = "new case")
#' }
#' @export
makeCaseVariable <- function(..., cases, data = NULL, name) {
    ## Gather the new variable's metadata fields (and possibly expressions)
    # -1 to remove the list primative
    dots <- as.list(substitute(list(...)))[-1L]
    casevar <- lapply(dots, evalSide, dat = data, eval_env = parent.frame())
    casevar$name <- name
    is_expr <- function(x) {
        inherits(x, "CrunchLogicalExpr") || x %in% magic_else_string
    }
    exprs <- Filter(is_expr, casevar)
    if (length(exprs) > 0) {
        if (missing(cases)) {
            ## Remove the expressions from the variable definition
            casevar <- Filter(Negate(is_expr), casevar)
            cases <- mapply(function(e, n) list(
                    name = n,
                    expression = e
                ),
            e = exprs, n = names(exprs),
            SIMPLIFY = FALSE, USE.NAMES = FALSE
            )
        } else {
            halt(
                "can't have case conditions both in ", dQuote("..."),
                " as well as in the ", dQuote("cases"), " argument, please use ",
                "one or the other."
            )
        }
    } else if (missing(cases) || all(substitute(cases) == "")) {
        # check substitute(cases) == "" in case the cases need to be
        # evaluated, length 1 means no cases are present.
        halt(
            "must supply case conditions in either ", dQuote("..."), " or ",
            "the ", dQuote("cases"), " argument, please use one or the other."
        )
    }

    cases <- evalSide(substitute(cases), data, parent.frame())
    cases <- ensureValidCases(cases)

    # create the new categorical variable
    new_cat_type <- list(
        value = list(
            class = "categorical",
            categories = lapply(cases, function(case) {
                case[c("id", "name", "numeric_value", "missing")]
            })
        )
    )
    new_cat_ids <- vapply(cases, vget("id"), integer(1))
    new_cat <- list(column = I(new_cat_ids), type = new_cat_type)

    casevar$derivation <- zfunc("case", new_cat)

    # add case_expressions, remove nulls (should only be from the else case)
    case_exprs <- lapply(cases, function(x) zcl(x$expression))
    case_exprs <- Filter(Negate(is.null), case_exprs)
    casevar$derivation$args <- c(casevar$derivation$args, case_exprs)

    class(casevar) <- "VariableDefinition"
    return(casevar)
}

magic_else_string <- "else"

is_else_case <- function(case) {
    identical(case[["expression"]], magic_else_string)
}

ensureValidCases <- function(cases) {
    # find the magical expression='else' if it exists.
    is_else <- vapply(cases, is_else_case, logical(1))
    # check if there is more than one else case
    if (sum(is_else) > 1) {
        halt(
            "you can only provide a single else case; you have more ",
            "than one in either ", dQuote("cases"), " or ", dQuote("...")
        )
    }
    # check that there are no else cases except for the last element of cases
    if (any(is_else[-length(is_else)])) {
        halt(
            "The else case must be the last element of ", dQuote("cases"),
            " or ", dQuote("...")
        )
    }
    cases <- lapply(cases, ensureValidCase)

    cases <- fillIds(cases)

    all_names <- vapply(cases, vget("name"), character(1))
    if (anyDuplicated(all_names) > 0) {
        halt("there are duplicate names provided: ", serialPaste(all_names))
    }

    all_exprs <- lapply(cases, vget("expression"))
    if (anyDuplicated(all_exprs) > 0) {
        halt(
            "there are duplicate condition expressions provided: ",
            serialPaste(vapply(all_exprs, formatExpression, character(1)))
        )
    }

    return(cases)
}

#' Validate case statements for case variables
#'
#' @param case a list for one case to test
#'
#' List elements:
#' 1. `id`: an integer to use for this category when a case variable is made
#' (default: none, one will automatically be assigned when the case variable
#' is made)
#' 1. `name`: a character identifier for this case
#' 1. `expression`: a `CrunchLogicalExpr` that sets the conditions for the case, or "else"
#' 1. `numeric_value`: a numeric that is the value this case should take on
#' when analyzing the data numerically (mean, standard deviation, etc.)
#' 1. `missing` a logical indicating if this case should be treated as missing
#'
#' @keywords internal
ensureValidCase <- function(case) {
    if (!is.list(case)) {
        halt("A case must be a list")
    }

    defaults <- list(
        id = NULL, name = NULL, expression = NULL,
        numeric_value = NULL, missing = FALSE
    )
    wrong_case_names <- setdiff(names(case), names(defaults))
    if (is.null(wrong_case_names) || "" %in% names(case)) {
        # if no names are found, some thing is wrong with the format of cases.
        # one possibility is that someone is trying to embed the cases too
        # many times, or that else_cases has more than one case.
        halt(
            "could not find correct names for a case; this might be because ",
            "the cases were embedded in too many lists or because not all ",
            "the elements of the cases have names. The first offending case ",
            "is:\n", case
        )
    } else if (length(wrong_case_names) > 0) {
        halt(
            "each case must have at most an id, name, expression, ",
            "numeric_value, and missing element. The errant arguments were: ",
            serialPaste(wrong_case_names)
        )
    }
    # set defaults
    case <- modifyList(defaults, case)

    # check if any of the elements of case have more than one element
    multi_elems <- vapply(case, length, integer(1)) > 1
    if (any(multi_elems)) {
        halt(
            "each case attribute (name, expression, etc.) should have a single element. ",
            "There is more than one attribute for ",
            serialPaste(names(case[multi_elems]))
        )
    }

    if (!is.null(case$id) & !is.whole(case$id)) {
        halt("a case's id must be an integer")
    }
    if (!is.character(case$name)) {
        halt("a case's name must be a character")
    }

    if (is_else_case(case)) {
        case$expression <- NULL
    } else if (!inherits(case$expression, "CrunchLogicalExpr")) {
        # if this is not an else case, check that expression is an expression
        halt("a case's expression must be a CrunchLogicalExpr")
    }
    if (!is.null(case$numeric_value) & !is.numeric(case$numeric_value)) {
        halt("a case's numeric_value must be a numeric")
    }
    if (!is.logical(case$missing) | is.na(case$missing)) {
        halt("a case's missing must be a logical")
    }

    return(case)
}

## Check for "integers" but don't require true integer storage
is.whole <- function(x) is.numeric(x) && floor(x) == x

# make ids if the cases don't have ids
fillIds <- function(cases) {
    need_ids <- vapply(cases, function(x) is.null(x$id), logical(1))
    supplied_ids <- vapply(cases[!need_ids], vget("id"), numeric(1))
    new_ids <- seq_along(cases) # generate too many ids
    new_ids <- setdiff(new_ids, supplied_ids) # discard ids where there is overlap
    cases[need_ids] <- lapply(seq_along(cases[need_ids]), function(i) {
        cases[need_ids][[i]]$id <- new_ids[i]
        return(cases[need_ids][[i]])
    })

    # check there are no duplicate ids and they are less than 2^15-1
    all_ids <- vapply(cases, vget("id"), numeric(1))
    if (anyDuplicated(all_ids) > 0) {
        halt("there are duplicate ids provided: ", serialPaste(all_ids))
    }
    if (any(all_ids > 32767L)) {
        halt(
            "id must be less than 32,768, this might be a result of too many ",
            "cases being used."
        )
    }
    if (any(all_ids < 1L)) {
        halt("id must not be less than 1")
    }

    return(cases)
}
