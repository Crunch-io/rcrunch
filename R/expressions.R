#' Crunch expressions internal
#'
#' See [`expressions`] for more details.
#'
#' @param x,e1,e2 inputs
#' @param table,na.rm,selections,upper,lower,inclusive,regex,ignore_case,selections,collapse,tiers,
#' cases,data,resolution,year,month,day,hours,minutes,seconds,min,max,categories,category_order,
#' subvariables,remove,remove_id,subreferences,subreferences_id
#' Other parameters used in some functions, see details of [`expressions`] for more details.
#' @return Most functions return a CrunchExpr or CrunchLogicalExpr.
#' `as.vector` returns an R vector.
#' @aliases expressions-internal %in% == != !
#' @name expressions-internal
#' @keywords internal
setGeneric("%in%")
# TODO: figure this out.
# Can't "roxygen" these because check says
# Functions or methods with usage in documentation object 'expressions' but not in code:
#   == != !
#
# but if you try standardGeneric, it fails to build with
# Error in setGeneric("==", function(e1, e2) standardGeneric("==")) :
#   ‘==’ dispatches internally;  methods can be defined, but the generic
#   function is implicit, and cannot be changed.
setGeneric("==")
setGeneric("!=")
setGeneric("!")

## "Ops" for Crunch Variables
##
## Most of the indirection here is to programatically create the Ops methods
## for the right combinations of multiple-dispatch signatures

math.exp <- function(e1, e2, operator, ...) {
    ## Generic function that creates CrunchExpr of `e1 %operator% e2`
    if (identical(e1, logical(0)) || identical(e2, logical(0))) {
        ## If you reference a variable in a dataset that doesn't exist, you
        ## get NULL, and e.g. NULL == something becomes logical(0).
        ## That does awful things if you try to send to the server. So don't.
        ##
        ## Because of how this function is invoked, get the offending expression
        ## from the call before this one
        halt(
            "Invalid expression (probably a reference to a variable that ",
            "doesn't exist): ", deparseAndFlatten(tail(sys.calls(), 2)[[1]])
        )
    }
    ex <- zfunc(operator, e1, e2, ...)
    ds.url <- unique(unlist(lapply(list(e1, e2), datasetReference))) %||% ""
    out <- ExprConstructor(operator)(expression = ex, dataset_url = ds.url)
    activeFilter(out) <- getOperationFilter(e1, e2)
    return(out)
}

getOperationFilter <- function(...) {
    ## If any argument is Crunch objects with filters, pass those along,
    ## and if multiple do, make sure that they're the same

    filters <- lapply(list(...), function(x) {
        out <- try(activeFilter(x), silent = TRUE)
        if (is.error(out)) out <- NULL
        out
    })

    filters <- filters[!vapply(filters, is.null, logical(1))]
    if (length(filters) == 0) return(NULL)
    Reduce(
        function(x, y) {
            if (!identical(x, y)) {
                halt("Cannot combine expressions with different filters")
            }
            x
        },
        filters
    )
}

ExprConstructor <- function(operator) {
    ## Based on the operator function, make either CrunchExpr or CrunchLogicalExpr
    logics <- c(
        "in", "<", ">", ">=", "<=", "==", "!=", "and", "or", "ornm", "not",
        "is_missing", "duplicates", "selected", "not_selected",
        "between", "=><=", "all", "any", "anynm", "is_none_of_the_above",
        "contains", "icontains", "~=", "is_valid", "any_missing",
        "all_valid", "all_missing"
    )
    if (operator %in% logics) {
        Constructor <- CrunchLogicalExpr
    } else {
        Constructor <- CrunchExpr
    }
    return(Constructor)
}

crunch.ops <- function(i, ...) {
    ## Create math.exp of Variable x R.object, R.object x Variable, or V x V
    force(i)
    return(function(e1, e2) math.exp(e1, e2, i, ...))
}

.sigs <- list(
    c("TextVariable", "character"),
    c("NumericVariable", "numeric"),
    c("DatetimeVariable", "Date"),
    c("DatetimeVariable", "POSIXt"),
    ## TODO: validate that the "character" is valid 8601?
    c("DatetimeVariable", "character"),
    ## TODO: add cast(x, "numeric") around var for this?
    c("CategoricalVariable", "numeric")
)

.rtypes <- unique(vapply(.sigs, function(a) a[[2]], character(1)))
.nomath <- which(!vapply(
    .sigs,
    function(a) a[[1]] %in% c("TextVariable", "CategoricalVariable"),
    logical(1)
))

for (i in c("+", "-", "*", "/", "<", ">", ">=", "<=")) {
    for (j in .nomath) {
        setMethod(i, .sigs[[j]], crunch.ops(i))
        setMethod(i, rev(.sigs[[j]]), crunch.ops(i))
    }
    for (j in setdiff(.rtypes, "character")) {
        setMethod(i, c("CrunchExpr", j), crunch.ops(i))
        setMethod(i, c(j, "CrunchExpr"), crunch.ops(i))
    }
    setMethod(i, c("CrunchVariable", "CrunchVariable"), crunch.ops(i))
    setMethod(i, c("CrunchExpr", "CrunchVariable"), crunch.ops(i))
    setMethod(i, c("CrunchVariable", "CrunchExpr"), crunch.ops(i))
    setMethod(i, c("CrunchExpr", "CrunchExpr"), crunch.ops(i))
}

setMethod("-", c("DatetimeVariable", "DatetimeVariable"), crunch.ops("difftime"))

#' @rdname expressions-internal
#' @export
crunchDifftime <- function(e1, e2, resolution = NULL) {
    isVarButNotType(e1, "Datetime", "crunchDifftime")
    isVarButNotType(e2, "Datetime", "crunchDifftime")
    crunch.ops("difftime", resolution)(e1, e2)
}

#' @rdname expressions-internal
#' @export
datetimeFromCols <- function(year, month, day, hour = NULL, minute = NULL, second = NULL) {
    isVarButNotType(year, "Numeric", "datetimeFromCols")
    isVarButNotType(month, "Numeric", "datetimeFromCols")
    isVarButNotType(day, "Numeric", "datetimeFromCols")
    if (!is.null(hour)) isVarButNotType(hour, "Numeric", "datetimeFromCols")
    if (!is.null(minute)) isVarButNotType(minute, "Numeric", "datetimeFromCols")
    if (!is.null(second)) isVarButNotType(second, "Numeric", "datetimeFromCols")

    ex <- zfunc("datetime", year, month, day, hour, minute, second)
    ds.url <- unique(unlist(lapply(list(year, month, day), datasetReference))) %||% ""
    out <- ExprConstructor("datetime")(expression = ex, dataset_url = ds.url)
    activeFilter(out) <- getOperationFilter(year, month, day, hour, minute, second)
    return(out)
}

#' @rdname expressions-internal
#' @export
`%ornm%` <- function(e1, e2) {
    crunch.ops("ornm")(e1, e2)
}

setMethod("&", c("CrunchExpr", "CrunchExpr"), crunch.ops("and"))
setMethod("&", c("logical", "CrunchExpr"), crunch.ops("and"))
setMethod("&", c("CrunchExpr", "logical"), crunch.ops("and"))
setMethod("|", c("CrunchExpr", "CrunchExpr"), crunch.ops("or"))
setMethod("|", c("logical", "CrunchExpr"), crunch.ops("or"))
setMethod("|", c("CrunchExpr", "logical"), crunch.ops("or"))

zfuncExpr <- function(fun, x, ...) {
    ## Wrap zfunc(fun, x) in a way that preserves x's active filter
    ## Returns CrunchExpr instead of zcl/list
    ## Currently only implemented for one arg with a filter (x)
    out <- ExprConstructor(fun)(expression = zfunc(fun, x, ...),
        dataset_url = datasetReference(x) %||% "")
    activeFilter(out) <- activeFilter(x)
    return(out)
}

#' Construct Crunch Expressions from Crunch Database Functions
#'
#' Crunch Expressions, i.e. `CrunchExpr` and `CrunchLogicalExpr`,
#' encapsulate derivations of Crunch variables, possibly composed of other functions
#' which are only evaluated when sent to the server when creating a variable using [`VarDef()`]
#' or using [`as.vector()`] to get data. The crunch database functions can be found in the
#' [Crunch API Reference](https://docs.crunch.io/object-reference/object-reference.html),
#' and can be called directly via `crunchdbFunc()`m but many have also been wrapped
#'  in native R functions, and are described in the details section below.
#'
#'
#' Logical expressions
#' - These logical operators `==`, `!=`, `&`, `|`, `!`,`%in%`  work the same way as their
#'   base R counterparts
#'  - `is.selected(x)` return `CrunchLogicalExpr` whether a value is in a selected category
#' - `rowAny(x)` and `rowAll(x)` work row-wise on `MultipleResponse` Variables (and expressions),
#'   though `na.rm` is not implemented for `all(x)`.
#'   `%ornm%` is similar to `|`, but where "not selected" beats "missing" (so `FALSE %ornm% NA`
#'    is `FALSE` instead of `NA` as it would be with `FALSE | NA`)
#'
#' Comparisons
#' - Comparison operators `<`, `<=`, `>`, `>=` work the same way as their base R counterparts.
#' - `crunchBetween(x, lower, upper, inclusive)` to provide lower and upper bounds in a single
#'   expression.
#'
#' Missing data expressions
#' - `is.na(x)`, `is.valid(x)` return `CrunchLogicalExpr` whether a single variable (or expression
#'   that creates one) is missing (or not missing).
#' - `rowAnyNA(x)`, `rowAllNA(x)` return `CrunchLogicalExpr` whether any/all values in an
#'   array variable (or expression that creates one) are missing.
#' - `complete.cases(x)` returns an expression that is "selected" if all cases are non-missing,
#'    "missing" if they are all missing, and "other" otherwise.
#'
#' Selection expressions
#'  - `selectCategories(x, selections, collapse = TRUE)` takes a categorical variable (or array)
#'     and marks categories as selected. `selections` should be a list of category names or
#'     values. If `collapse` is `TRUE`, (the default), it collapses the categories to "selected",
#'      "other" and "missing", but it is `FALSE`, then the old categories are preserved.
#'  - `asSelected(x)` returns an expression that condenses a categorical into 3 categories
#'    ("selected", "other" or "missing")
#'  - `selectedDepth(x)` returns an expression that creates a numeric variable that counts the
#'    number of selections across rows of an array variable (or expression that creates one)
#'  - `arraySelections(x)` returns an expression that takes an array and creates an array with
#'    each variable condensed to "selected", "other" or "missing" and an extra subvariable
#'    "__any__" that indicates whether any is selected.
#'
#' Array expressions
#'  - `makeFrame(x)` an expression that creates an array from existing variables or expressions,
#'    see `deriveArray()` for more details
#'  - `alterCategoriesExpr(x, categories = NULL, category_order = NULL, subvariables = NULL)`
#'     Change the category names, order, or subvariable names of categorical or Array variables
#'     (can only modify existing ones, not add or remove categories or subvariables).
#'     `categories` is a `Categories` object or a list of lists, each with a `name` indicating
#'     the new name, as well as an `id` or `old_name` to identify which category to modify.
#'     `category_order` is either a numeric vector indicating category ids or a character vector
#'     indicating the names of the categories in the order they should be displayed
#'     (note that all categories must be specified). `subvariables` is  a list of lists, each with
#'     a `name` to rename the subvariable and an `alias`, `old_nam` or `id` to identify the
#'     subvariable. When `x` is an expression, all categories and subvariables must be identified
#'     by `id`.
#'  - `arraySubsetExpr(x, subvars, subvar_id = c("alias", "name", "id"))` Take a subset of an
#'    existing array variable, identifying the subvariables by alias, name, or id (if `x` is
#'    an expression, you must use id).
#'
#' Miscellaneous expressions
#'  - `caseExpr(..., cases)` Create a categorical variable from
#'    a set of logical expressions that when met are assigned to a category. See
#'    [`makeCaseVariable()`] for more details.
#'  - `bin(x)` returns a column's values binned into equidistant bins.
#'  - `nchar(x)` returns a numeric value indicating the length of a string (or missing reason)
#'     in a `TextVariable` (or expression that creates one)
#'  - `unmissing(x)` for a `NumericVariable` (or expression that creates one) return the values of
#'    the data, ignoring the ones set to missing.
#'  - `trim(x, min, max)` for a `NumericVariable` (or expression that creates one) return values
#'    that where all values less than `min` have been replaced with `min` and all values greater
#'    than `max` have been
#'  - `crunchDifftime(e1, e2, resolution)` Gets the difference between two datetimes as a number
#'     with specified resolution units (one of `c("Y", "Q", "M", "W", "D", "h", "m", "s", "ms")`).
#'  - `datetimeFromCols(year, month, day, hours, minutes, seconds)` create a `Datetime` variable
#'    from numeric variables or expressions (`year`, `month`, and `day` are required, but `hours`,
#'    `minutes`, and `seconds` are optional)
#'  - `rollup(x, resolution)` sets the resolution of a datetime variable or expression, see
#'    [`resolution()`]
#'
#' @name expressions
#' @param fun The name of the crunch database function to call
#' @param x An input, a crunch variable, expression or R object
#' @param ... Other arguments passed to the database function
#' @export
crunchdbFunc <- zfuncExpr

#' @rdname expressions-internal
#' @export
setMethod("!", "CrunchExpr", function(x) zfuncExpr("not", x))

#' @importFrom utils head tail
.seqCrunch <- function(x, table) {
    ## Given x %in% table, if table is numeric, see if we can/should collapse
    ## it into a range query rather than sending lots of distinct values

    if (is.numeric(table) &&
        length(table) > 2 &&
        all(!is.na(table)) &&
        identical(as.numeric(head(table, 1):tail(table, 1)), as.numeric(table))) {

        # set beg(ining) and end appropriately in case we have been given a rev
        # sequence (eg 20:1) ZCL returns nothing if asked for between 20 and 1
        if (head(table, 1) < tail(table, 1)) {
            beg <- head(table, 1)
            end <- tail(table, 1)
        } else {
            beg <- tail(table, 1)
            end <- head(table, 1)
        }

        return(zfunc(
            "between",
            x,
            beg,
            end,
            list(value = I(c(TRUE, TRUE)))
        ))
    } else {
        return(zfunc(ifelse(length(table) == 1L, "==", "in"), x, table))
    }
}

.inCrunch <- function(x, table) zfuncExpr("selected", math.exp(x, r2zcl(I(table)), "in"))

#' @rdname expressions-internal
#' @export
setMethod(
    "%in%", c("CategoricalVariable", "character"),
    function(x, table) .inCrunch(x, n2i(table, categories(x), strict = FALSE))
)
#' @rdname expressions-internal
#' @export
setMethod(
    "%in%", c("CategoricalVariable", "factor"),
    function(x, table) x %in% as.character(table)
)

## Iterated version of below:
for (i in seq_along(.sigs)) {
    setMethod("%in%", .sigs[[i]], .inCrunch)
}

#' @rdname expressions-internal
#' @export
setMethod("%in%", c("TextVariable", "character"), .inCrunch)
#' @rdname expressions-internal
#' @export
setMethod("%in%", c("NumericVariable", "numeric"), .inCrunch)
#' @rdname expressions-internal
#' @export
setMethod("%in%", c("DatetimeVariable", "Date"), .inCrunch)
#' @rdname expressions-internal
#' @export
setMethod("%in%", c("DatetimeVariable", "POSIXt"), .inCrunch)
#' @rdname expressions-internal
#' @export
setMethod("%in%", c("DatetimeVariable", "character"), .inCrunch)
#' @rdname expressions-internal
#' @export
setMethod("%in%", c("CategoricalVariable", "numeric"), .inCrunch)

for (i in c("==", "!=")) {
    for (j in seq_along(.sigs)) {
        setMethod(i, .sigs[[j]], crunch.ops(i))
        setMethod(i, rev(.sigs[[j]]), crunch.ops(i)) ## is this right?
    }
    for (j in .rtypes) {
        setMethod(i, c("CrunchExpr", j), crunch.ops(i))
        setMethod(i, c(j, "CrunchExpr"), crunch.ops(i))
    }
    setMethod(i, c("CrunchVariable", "CrunchVariable"), crunch.ops(i))
    setMethod(i, c("CrunchExpr", "CrunchVariable"), crunch.ops(i))
    setMethod(i, c("CrunchVariable", "CrunchExpr"), crunch.ops(i))
    setMethod(i, c("CrunchExpr", "CrunchExpr"), crunch.ops(i))
}

#' @rdname expressions-internal
#' @export
setMethod("==", c("CategoricalVariable", "numeric"), function(e1, e2) {
    if (length(e2) == 0) {
        ## The specified category doesn't exist. But `== BAD` breaks server
        ## However, "in []" is fine
        return(math.exp(e1, e2, "in"))
    }
    return(math.exp(e1, e2, "=="))
})

#' @rdname expressions-internal
#' @export
setMethod("==", c("CategoricalVariable", "character"), function(e1, e2) {
    e2 <- n2i(e2, categories(e1), strict = FALSE)
    return(e1 == e2)
})
#' @rdname expressions-internal
#' @export
setMethod(
    "==", c("CategoricalVariable", "factor"),
    function(e1, e2) e1 == as.character(e2)
)

#' @rdname expressions-internal
#' @export
setMethod("!=", c("CategoricalVariable", "numeric"), function(e1, e2) {
    if (length(e2) == 0) {
        ## The specified category was doesn't exist. But `== BAD` breaks server
        ## However, "in []" is fine. So do `not (in [])`
        return(!math.exp(e1, e2, "in"))
    }
    return(math.exp(e1, e2, "!="))
})
#' @rdname expressions-internal
#' @export
setMethod("!=", c("CategoricalVariable", "character"), function(e1, e2) {
    e2 <- n2i(e2, categories(e1), strict = FALSE)
    return(e1 != e2)
})
#' @rdname expressions-internal
#' @export
setMethod(
    "!=", c("CategoricalVariable", "factor"),
    function(e1, e2) e1 != as.character(e2)
)

#' @rdname expressions-internal
#' @export
setMethod("is.na", "CrunchVarOrExpr", function(x) zfuncExpr("is_missing", x))

#' @rdname expressions-internal
#' @export
is.valid <- function(x) zfuncExpr("is_valid", x)

#' @rdname expressions-internal
#' @export
bin <- function(x) zfuncExpr("bin", x)

#' @rdname crunch-extract
#' @export
setMethod("[", c("CrunchExpr", "CrunchLogicalExpr"), .updateActiveFilter)

.updateActiveFilterLogical <- function(x, i, ...) {
    if (length(i)) {
        i <- CrunchLogicalExpr(
            dataset_url = datasetReference(x),
            expression = .dispatchFilter(i)
        )
        return(x[i])
    } else {
        ## If you reference a variable in a dataset that doesn't exist, you
        ## get NULL, and e.g. NULL == something becomes logical(0).
        ## That does awful things if you try to send to the server. So don't.
        halt("Invalid expression: ", deparseAndFlatten(match.call()$i))
    }
}

#' @rdname crunch-extract
#' @export
setMethod("[", c("CrunchExpr", "logical"), .updateActiveFilterLogical)

#' @rdname crunch-extract
#' @export
setMethod("[", c("CrunchExpr", "numeric"), function(x, i, ...) {
    i <- CrunchLogicalExpr(
        dataset_url = datasetReference(x),
        expression = .dispatchFilter(i)
    )
    return(x[i])
})

#' "which" method for CrunchLogicalExpr
#'
#' @param x CrunchLogicalExpr
#' @param arr.ind Ignored
#' @param useNames Ignored
#' @return Integer row indices where \code{x} is true. Note that this does not
#' return a Crunch expression. Use this when you need to translate to R values.
#' For filtering a Crunch expression by \code{x}, don't use \code{which}.
#' @aliases which
#' @name which
NULL

#' @rdname which
setMethod("which", "CrunchLogicalExpr", function(x, arr.ind, useNames) {
    which(as.vector(x))
})

#' "duplicated" method for Crunch objects
#'
#' @param x `CrunchVariable` or `CrunchExpr`
#' @param incomparables Ignored
#' @param ... Ignored
#' @return A `CrunchLogicalExpr` that evaluates `TRUE` for all repeated
#' entries after the first occurrence of a value.
#' @name duplicated
#' @seealso [base::duplicated()]
#' @aliases duplicated
#' @export
NULL

#' @rdname duplicated
#' @export
setMethod("duplicated", "CrunchVariable", function(x, incomparables = FALSE, ...) {
    zfuncExpr("duplicates", x)
})

#' @rdname duplicated
#' @export
setMethod("duplicated", "CrunchExpr", function(x, incomparables = FALSE, ...) {
    zfuncExpr("duplicates", x)
})

#' @rdname expressions-internal
#' @export
selectCategories <- function(x, selections, collapse = TRUE) {
    out <- zfuncExpr("select_categories", x, list(value = I(selections)))
    if (collapse) out <- asSelected(out)
    out
}

#' @rdname expressions-internal
#' @export
crunchBetween <- function(x, lower, upper, inclusive = c(TRUE, FALSE)) {
    isVarButNotType(x, "Numeric", "crunchBetween")
    zfuncExpr("between", x, lower, upper, list(value = I(inclusive)))
}

#' @rdname expressions-internal
#' @export
rowAll <- function(x, na.rm = FALSE) {
    isVarButNotType(x, "Array", "rowAll")
    zfuncExpr("all", x)
}

#' @rdname expressions-internal
#' @export
rowAny <- function(x, na.rm = FALSE) {
    isVarButNotType(x, "Array", "rowAny")
    func_name <- if (na.rm) "anynm" else "any"
    zfuncExpr(func_name, x)
}

#' @rdname expressions-internal
#' @export
rowAnyNA <- function(x) {
    isVarButNotType(x, "Array", "rowAnyNA")
    zfuncExpr("any_missing", x)
}

#' @rdname expressions-internal
#' @export
rowAllNA <- function(x) {
    isVarButNotType(x, "Array", "rowAllNA")
    zfuncExpr("all_missing", x)
}

#' @rdname expressions-internal
#' @export
setMethod("complete.cases", c("CrunchVarOrExpr"), function(x, ...) {
    isVarButNotType(x, "Array", "complete.cases")
    if (length(list(...)) > 0) {
        halt("crunch::complete.cases() only works on arrays so can only take a single argument")
    }

    zfuncExpr("complete_cases", x)
})

#' @rdname expressions-internal
#' @export
setMethod("is.selected", "CrunchVarOrExpr", function(x) {
    isVarButNotType(x, "Categorical", "is.selected")
    zfuncExpr("selected", x)
})

#' @rdname expressions-internal
#' @export
asSelected <- function(x) {
    isVarButNotType(
        x,
        c("Categorical", "Categorical Array", "Multiple Response"),
        "asSelected"
    )
    zfuncExpr("as_selected", x)
}

#' @rdname expressions-internal
#' @export
selectedDepth <- function(x) {
    isVarButNotType(x, "Multiple Response", "selectedDepth")
    zfuncExpr("selected_depth", x)
}

#' @rdname expressions-internal
#' @export
arraySelections <- function(x) {
    isVarButNotType(x, "Multiple Response", "arraySelections")
    zfuncExpr("selections", x)
}

#' @rdname expressions-internal
#' @export
setMethod("nchar", c("CrunchVarOrExpr"), function(x, type = "chars", allowNA = FALSE, keepNA = NA) {
    isVarButNotType(x, "Text", "nchar")

    if (type != "chars") warning("type argument ignored in `crunch::nchar()`")
    if (allowNA) warning("allowNA argument ignored in `crunch::nchar()`")
    if (!is.na(keepNA)) warning("keepNA argument ignored in `crunch::nchar()`")

    zfuncExpr("char_length", x)
})

#' @rdname expressions-internal
#' @export
trim <- function(x, min, max) {
    isVarButNotType(x, "Numeric", "trim")
    zfuncExpr("trim", x, min, max)
}

#' @rdname expressions-internal
#' @export
alterCategoriesExpr <- function(
    x,
    categories = NULL,
    category_order = NULL,
    subvariables = NULL
) {
    isVarButNotType(x, c("Array", "Categorical"), "alterCategoriesExpr")

    args <- list()
    if (!is.null(categories)) args$categories <- alter_cats_get_cat_ids(x, categories)
    if (!is.null(order)) args$order <- alter_cats_get_order_ids(x, category_order)
    if (!is.null(subvariables)) {
        args$subvariables <- alter_cats_get_subvar_ids(x, subvariables)
    }

    zfuncExpr("alter_categories", x, list(value = I(args)))
}

alter_cats_get_cat_ids <- function(x, categories) {
    if (is.null(categories)) return(categories)

    if (is.variable(x)) {
        old_names <- setNames(as.list(ids(categories(x))), names(categories(x)))
    } else {
        old_names <- NULL
    }

    categories <- lapply(categories, function(cat) {
        if ("id" %in% names(cat)) {
            cat[!names(cat) %in% "selected"]
        } else if ("old_name" %in% names(cat)) {
            if (is.null(old_names)) {
                halt("Must use category ids when modifying categories of an expression")
            }
            id <- getElement(old_names, cat$old_name)
            if (is.null(id)) {
                halt("Could not find category with old name '", cat$old_name, "'")
            }
            c(list(id = id), cat[!names(cat) %in% c("selected", "old_name")])
        } else {
            halt("Must specify either id or old_name for each category update")
        }
    })
}

alter_cats_get_order_ids <- function(x, category_order) {
    if (is.null(category_order) | is.numeric(category_order)) {
        category_order
    } else if (is.character(category_order)) {
        if (!is.variable(x)) {
            halt("Must use category ids when reordering categories of an expression")
        }

        matches <- match(category_order, names(categories(x)))
        if (any(is.na(matches))) {
            halt(
                "Categories ",
                paste0("'", category_order[is.na(matches)], "'", collapse = ","),
                " not found in data,"
            )
        }
        ids(categories(x))[matches]
    } else {
        halt("Expected category_order to be numeric vector of ids or character vector of names")
    }
}

alter_cats_get_subvar_ids <- function(x, subvariables) {
    if (is.null(subvariables)) return(subvariables)

    if (is.variable(x)) {
        old_aliases <- setNames(as.list(ids(subvariables(x))), aliases(subvariables(x)))
        old_names <- setNames(as.list(ids(subvariables(x))), names(subvariables(x)))
    } else {
        old_aliases <- NULL
        old_names <- NULL
    }

    lapply(subvariables, function(sv) {
        if (!"name" %in% names(sv)) halt("All subvariables must have a name to update")
        if ("id" %in% names(sv)) {
            list(id = as.character(sv$id), name = sv$name)
        } else if (is.null(old_aliases)) {
            halt("Must use subvariable ids when modifying subvariable names of an expression")
        } else if ("alias" %in% names(sv)) {
            id <- getElement(old_aliases, sv$alias)
            if (is.null(id)) {
                halt("Could not find subvariable with alias '", sv$alias, "'")
            }
            list(id = id, name = sv$name)
        } else if ("old_name" %in% names(sv)) {
            id <- getElement(old_names, sv$old_name)
            if (is.null(id)) {
                halt("Could not find subvariable with old name '", sv$old_name, "'")
            }
            list(id = id, name = sv$name)
        }
    })
}

#' @rdname expressions-internal
#' @export
alterArrayExpr <- function(
    x,
    add = NULL,
    order = NULL,
    order_id = c("alias", "name", "id"),
    remove = NULL,
    remove_id = c("alias", "name", "id"),
    subreferences = NULL,
    subreferences_id = c("alias", "name", "id")
) {
    isVarButNotType(x, "Array", "alterArrayExpr")
    remove_id <- match.arg(remove_id)
    order_id <- match.arg(order_id)
    subreferences_id <- match.arg(subreferences_id)

    if (!is.null(add)) {
        if (is.variable(add) || is.VarDef(add)) add <- list(add)
        add_ids <- if (is.null(names(add))) new_array_ids(x, length(add)) else names(add)
        add <- setNames(lapply(add, zcl), add_ids)

        if (is.null(order)) {
            if (!is.variable(x)) {
                halt("Must set order when adding subvariables to an expression")
            }
            order <- c(ids(subvariables(x)), add_ids)
            order_id <- "id"
        }
    }

    if (!is.null(order)) {
        order <- match_subvar_to_id(x, order, order_id, add)
    }

    remove <- match_subvar_to_id(x, remove, remove_id, add)
    if (!is.null(subreferences)) {
        names(subreferences) <- match_subvar_to_id(x, names(subreferences), subreferences_id, add)
    }

    args <- list(fun = "alter_array", x = x)

    if (!is.null(add)) args$add <- list(map = I(add))
    if (!is.null(order)) args$order <- list(value = I(order))
    if (!is.null(remove)) args$remove <- list(value = I(remove))
    if (!is.null(subreferences)) args$subreferences <- list(value = I(subreferences))

    do.call(zfuncExpr, args)
}

new_array_ids <- function(array, num) {
    if (is.variable(array)) {
        existing_ids <- ids(subvariables(array))
    } else {
        existing_ids <- try({
            arrayflat <- unlist(array)
            unlist(arrayflat)[grepl("\\.map\\.", names(arrayflat))]
        }, silent = TRUE)
        if (inherits(existing_ids, "try-error")) existing_ids <- c()
    }

    max_numeric <- suppressWarnings(max(c(as.numeric(existing_ids), 0), na.rm = TRUE))
    as.character(max_numeric + seq_len(num))
}

#' @rdname expressions-internal
#' @export
arraySubsetExpr <- function(x, subvars, subvar_id = c("alias", "name", "id")) {
    isVarButNotType(x, "Array", "arraySubsetExpr")
    subvar_id <- match.arg(subvar_id)
    subvars <- match_subvar_to_id(x, subvars, subvar_id)

    zfuncExpr("array_subset", x, list(value = I(subvars)))
}

match_subvar_to_id <- function(x, subvars, id_type = c("alias", "name", "id"), add = NULL) {
    if (is.null(subvars)) return(subvars)
    id_type <- match.arg(id_type)

    if (!is.null(add)) {
        add_refs <- list(
            ids = names(add),
            aliases = vapply(add, function(x) x$references$alias %||% "", ""),
            names = vapply(add, function(x) x$references$name %||% "", "")
        )
    } else {
        add_refs <- list(ids = NULL, aliases = NULL, names = NULL)
    }

    if (id_type == "id") {
        if (!is.variable(x)) return(subvars) # no validation possible
        matches <- match(subvars, c(ids(subvariables(x)), add_refs$ids))
    } else if (!is.variable(x)) {
        halt("Must provide subvariable ids when x is an expression")
    } else if (id_type == "alias") {
        matches <- match(subvars, c(aliases(subvariables(x)), add_refs$aliases))
    } else if (id_type == "name") {
        matches <- match(subvars, c(names(subvariables(x)), add_refs$names))
    }

    if (any(is.na(matches))) {
        halt(
            "Could not find subvariables with ", id_type, " ",
            paste0("'", subvars[is.na(matches)], "'", collapse = ",")
        )
    }
    c(ids(subvariables(x)), add_refs$ids)[matches]
}

#' @rdname crunch-is
#' @export
is.CrunchExpr <- function(x) inherits(x, "CrunchExpr")

#' @rdname crunch-is
#' @export
is.Expr <- is.CrunchExpr

isVarButNotType <- function(x, types, caller) {
    if (is.null(x)) halt(
        "Expected a crunch variable or expression but got `NULL`, did you misspell a variable?"
    )
    type_check <- lapply(types, function(type) {
        switch(
            type,
            "Numeric" = is.Numeric,
            "Text" = is.Text,
            "Datetime" = is.Datetime,
            "Categorical" = is.Categorical,
            "Array" = is.Array,
            "Categorical Array" = is.CategoricalArray,
            "Multiple Response" = is.MultipleResponse,
            halt("bad type check ", type)
        )
    })
    if (is.variable(x) && !any(vapply(type_check, function(func) func(x), logical(1)))) {
        type_str <- paste0("'", types, "'", collapse = ", ")
        halt("variable must be of type ", type_str, " for ", caller, "().")
    }
}
