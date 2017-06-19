#' Validate case statements for case variables
#'
#' @param case a list for one case to test
#' @param is_else is this case the final (else) case? If so, the expression 
#' must be missing.
#'
#' List elements:
#' id an integer to use for this category when a case variable is made
#' (default: none, one will automatically be assigned when the case variable
#' is made)
#' name a charcater identifier for this case
#' expression `CrunchLogicalExpr` which sets the conditions for the case
#' numeric_value a numeric which is the value this case should take on
#' (useful when made into a case variable)
#' missing a logical indicating if this case should be treated as missing
#'
#' @keywords internal
ensureValidCase <- function(case, is_else=FALSE) {
    if (!is.list(case)) {
        halt("A case must be a list")
    }
    wrong_case_names <- setdiff(names(case), c("id", "name", "expression",
                                               "numeric_value", "missing"))
    if (length(wrong_case_names) > 0) {
        halt("each case must have at most an id, name, expression, ",
             "numeric_value, and missing element. The errant arguments were: ",
             serialPaste(wrong_case_names))
    }
    
    # check if any of the elements of case have more than one element
    multi_elems <- vapply(case, length, integer(1)) > 1
    if (any(multi_elems)) {
        halt("each case attribute (name, expression, etc.) should have a single element. ",
             "There is more than one attribute for ",
             serialPaste(names(case[multi_elems])))
    }
    
    # set defaults
    defaults <- list(id=NULL, name=NULL, expression=NULL,
                     numeric_value=NULL, missing=FALSE)
    case <- modifyList(defaults, case)
    
    if (!is.null(case$id) & !is.whole(case$id)) {
        halt("a case's id must be an integer")
    }
    if (!is.character(case$name)) {
        halt("a case's name must be a character")
    }
    
    if (is_else) {
        # if this is an else case, expression must be NULL
        if (!is.null(case$expression)) {
            halt("else_cases should not have any conditions expression")
        }
    } else if(class(case$expression) != "CrunchLogicalExpr") {
        # if this is not an else case, check that expression is an expression
        halt("a case's expression must be a CrunchLogicalExpr")
    }
    if (!is.null(case$numeric_value) & !is.numeric(case$numeric_value)) {
        halt("a case's numeric_value must be a numeric")
    }
    if (!is.logical(case$missing) | is.na(case$missing)) {
        halt("a case's missing must be a logical")
    }

    # remove the expression if this is an else case
    if (is_else) case$expression <- NULL
    
    return(case)
}

## Check for "integers" but don't require true integer storage
is.whole <- function (x) is.numeric(x) && floor(x) == x

# make ids if the cases don't have ids
fillIds <- function(cases) {
    need_ids <- vapply(cases, function (x) is.null(x$id), logical(1))
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
        halt("id must be less than 32,768, this might be a result of too many cases being used.")
    }
    if (any(all_ids < 1L)) {
        halt("id must not be less than 1")
    }
    
    return(cases)
}

ensureValidCases <- function(cases, else_case) {
    cases <- lapply(cases, ensureValidCase)

    if (!missing(else_case)) {
        else_case <- ensureValidCase(else_case, is_else=TRUE)
        cases <- c(cases, list(else_case))
    }
    
    cases <- fillIds(cases)

    all_names <- vapply(cases, vget("name"), character(1))
    if (anyDuplicated(all_names) > 0) {
        halt("there are duplicate names provided: ", serialPaste(all_names))
    }
    
    all_exprs <- lapply(cases, vget("expression"))
    if (anyDuplicated(all_exprs) > 0) {
        halt("there are duplicate condition expressions provided: ",
             serialPaste(vapply(all_exprs, formatExpression, character(1))))
    }
    
    return(cases)
}


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
#' `makeCaseVaraible(case1=ds$v1 == 1, case2=ds$v2 == 2, name="new case")`
#'
#' 1. You can also use the `cases` argument, which is useful when you want to
#' prespecify ids, or set numeric_values:
#' `makeCaseVaraible(cases=list(list(expression=ds$v1 == 1, name="case1"), list(expression=ds$v2 == 2, name="case2")), name="new case")`
#'
#' @param ... a sequence of named expressions to use as cases as well as other
#' properties to pass about the case variable (i.e. alias, description)
#' @param cases a list of lists with each case condition to use each must
#' include at least a `name` and an `expression` element.
#' @param else_case a single list that has at least a `name` to serve as the
#' case when no others match (if not specified Crunch will use the system
#' default "No data")
#' @param name a character to use as the name of the case variable to create
#'
#' @return A [`VariableDefinition`] that will create the new
#' case variable.
#'
#' @export
makeCaseVariable <- function (..., cases, else_case, name) {
    casevar <- list(..., name=name)
    is_expr <- function (x) inherits(x, "CrunchLogicalExpr") || x == 'else'
    exprs <- Filter(is_expr, casevar)
    if (!missing(cases) & length(exprs) > 0) {
        halt("can't have case conditions both in ... as well as in the cases ",
             "argument, please use one or the other.")
    }
    if (missing(cases) & length(exprs) == 0) {
        halt("must supply case conditions in either ... or the cases ",
             "argument, please use one or the other.")
    }
    if (missing(cases) & length(exprs) > 0) {
        # save any member of casevar that isn't an expression for use later.
        casevar <- Filter(Negate(is_expr), casevar)
        cases <- mapply(function (expr, name) list(name=name, expression=expr),
                        expr=exprs, name=names(exprs),
                        SIMPLIFY = FALSE, USE.NAMES = FALSE)
    }
    
    # find the magical expressoin='else' if it exists.
    is_else <- vapply(cases, function(case) {
        is.character(case[['expression']]) && case[['expression']] == 'else'
        }, logical(1))
    if (any(is_else)) {
        else_case <- cases[[which(is_else)]]
        cases <- cases[!is_else]
    }
    
    cases <- ensureValidCases(cases, else_case)

    # create the new categorical variable
    new_cat_type <- list(
        value=list(
            class="categorical",
            categories=lapply(cases, function (case) {
                case[c("id", "name", "numeric_value", "missing")]
            })))
    new_cat_ids <- vapply(cases, vget("id"), integer(1))
    new_cat <- list(column=I(new_cat_ids), type=new_cat_type)

    casevar$derivation <- zfunc("case", new_cat)

    # add case_expressions, remove nulls (should only be from the else case)
    case_exprs <- lapply(cases, function(x) zcl(x$expression))
    case_exprs <- Filter(Negate(is.null), case_exprs)
    casevar$derivation$args <- c(casevar$derivation$args, case_exprs)

    class(casevar) <- "VariableDefinition"
    return(casevar)
}
