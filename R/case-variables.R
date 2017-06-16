#' Validate case statements for case variables
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
ensureValidCases <- function(case) {
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
    if (!is.null(case$id) && !is.integer(case$id)) {
        halt("a case's id must be an integer")
    }
    if (!is.character(case$name)) {
        halt("a case's name must be a character")
    }
    if (class(case$expression) != "CrunchLogicalExpr") {
        halt("a case's expression must be a CrunchLogicalExpr")
    }
    if (is.null(case$numeric_value)) {
        case['numeric_value'] <- list(NULL)
    } else if (!is.numeric(case$numeric_value)) {
        halt("a case's numeric_value must be a numeric")
    }
    if (is.null(case$missing)) {
        case['missing'] <- FALSE
    } else if (!is.logical(case$missing)) {
        halt("a case's missing must be a logical")
    }
    
    return(case)
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
#' @export
makeCaseVariable <- function (..., cases, else_case, name) {
    dots <- list(...)
    is_expr <- function (x) inherits(x, "CrunchLogicalExpr")
    exprs <- Filter(is_expr, dots)
    if (!missing(cases) & length(exprs) > 0) {
        halt("can't have case conditions both in ... as well as in the cases ",
             "argument, please use one or the other.")
    }
    if (missing(cases) & length(exprs) == 0) {
        halt("must supply case conditions in either ... or the cases ",
             "argument, please use one or the other.")
    }
    if (missing(cases) & length(exprs) > 0) {
        # save any member of dots that isn't an expression for use later.
        dots <- Filter(Negate(is_expr), dots)
        cases <- mapply(function (expr, name) list(name=name, expression=expr),
                        expr=exprs, name=names(exprs),
                        SIMPLIFY = FALSE, USE.NAMES = FALSE)
    }
    
    # make ids if the cases don't have ids.
    need_ids <- vapply(cases, function (x) is.null(x$id), logical(1))
    supplied_ids <- vapply(cases[!need_ids], function (x) x$id, integer(1))
    new_ids <- seq_along(cases) # generate too many ids
    new_ids <- setdiff(new_ids, supplied_ids) # discard ids where there is overlap
    cases[need_ids] <- lapply(seq_along(cases[need_ids]), function(i) {
        cases[need_ids][[i]]$id <- new_ids[i]
        return(cases[need_ids][[i]])
    })
    
    # check there are no duplicate ids
    all_ids <- vapply(cases, function (x) x$id, integer(1))
    if (anyDuplicated(all_ids) > 0) {
        halt("there are duplicate ids provided: ", serialPaste(all_ids))
    }
    
    cases <- lapply(cases, ensureValidCases)

    if (!missing(else_case)) {
        # assign API default values if not given
        if(is.null(else_case$id)) {
            else_case['id'] <- -1L
        }
        if(is.null(else_case$numeric_value)) {
            else_case['numeric_value'] <- list(NULL)
        }
        if(is.null(else_case$missing)) {
            else_case['missing'] <- FALSE
        }
        if(!is.null(else_case$expression)) {
            halt("else_cases should not have any conditions expression")
        }
        if(!is.character(else_case$name)) {
            halt("else_cases must have a (character) name")
        }
        if(!is.integer(else_case$id)) {
            halt("id must be an integer")
        }
        cases <- append(cases, list(else_case))
    }
    
    # create the new categorical variable
    new_cat_type <- list(
        value=list(class="categorical",
        categories=lapply(cases, function (case) case[c("id", "name", "numeric_value", "missing")])))
    new_cat_ids <- sapply(new_cat_type$value$categories, function(x) x$id)
    new_cat <- list(column=I(new_cat_ids), type=new_cat_type)
    
    casevar <- dots
    casevar$name <- name
    casevar$derivation <- zfunc("case", new_cat)

    # add case_expressions, remove nulls (should only be from the else case)
    case_exprs <- lapply(cases, function(x) zcl(x$expression))
    case_exprs <- Filter(Negate(is.null), case_exprs)
    casevar$derivation$args <- append(casevar$derivation$args, case_exprs)

    class(casevar) <- "VariableDefinition"
    return(casevar)
}
