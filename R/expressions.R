#' Crunch expressions internal
#'
#' See [`expressions`] for more details.
#'
#' @param x,e1,e2 inputs
#' @param table,na.rm,selections,upper,lower,inclusive,regex,ignore_case,selections,collapse,tiers,
#' cases,data,resolution,year,month,day,hours,minutes,seconds,min,max,categories,category_order,
#' subvariables
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
    isVarButNotType(hour, "Numeric", "datetimeFromCols")
    isVarButNotType(minute, "Numeric", "datetimeFromCols")
    isVarButNotType(second, "Numeric", "datetimeFromCols")

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
#'  - `is.selected(x)`, `is.notSelected(x)` return `CrunchLogicalExpr` whether a value is (or is not) in
#'  a selected category (missing data is considered not selected, unlike `!` which preserves missing)
#' - `any(x)` and `all(x)` work row-wise on `MultipleResponse` Variables (and expressions), though `na.rm`
#'   is not implemented for `all(x)`.
#'   `%ornm%` is similar to `|`, but where "not selected" beats "missing" (so `FALSE %ornm% NA` is
#'   `FALSE` instead of `NA` as it would be with `FALSE | NA`)
#' - `isNoneAbove(x)` is equivalent to `!any(x)`
#'
#' Comparisons
#' - Comparison operators `<`, `<=`, `>`, `>=` work the same way as their base R counterparts.
#' - `between(x, lower, upper, inclusive)` to provide lower and upper bounds in a single
#'   expression.
#' - `textContains(x, regex, ignore_case)` looks for the regular expression pattern `regex` in a `TextVariable` (or
#'   expression) `x` and returns a `CrunchLogicalExpr` indicating if it was found.
#'
#' Missing data expressions
#' - `is.na(x)`, `is.valid(x)` return `CrunchLogicalExpr` whether a single variable (or expression that creates one)
#'   is missing (or not missing).
#' - `anyNA(x)`, `allNA(x)`, `allValid(x)` return `CrunchLogicalExpr` whether all/any values in an array
#'   variable (or expression that creates one) are missing (or not).
#' - `completeCases(x)` returns an expression that is "selected" if all cases are non-missing,
#'    "missing" if they are all missing, and "other" otherwise.
#'
#' Selection expressions
#'  - `selectCategories(x, selections, collapse = TRUE)` takes a categorical variable (or array)
#'     and marks categories as selected. `selections` should be a list of category names or values.
#'     If `collapse` is `TRUE`, (the default), it collapses the categories to "selected", "other"
#'     and "missing", but it is `FALSE`, then the old categories are preserved.
#'  - `asSelected(x)` returns an expression that condenses a categorical into 3 categories ("selected", "other"
#'     or "missing")
#'  - `selectedDepth(x)` returns an expression that creates a numeric variable that counts the number of selections
#'    across rows of an array variable (or expression that creates one)
#'  - `arraySelections(x)` returns an expression that takes an array and creates an array with each variable
#'    condensed to "selected", "other" or "missing" and an extra subvariable "__any__" that indicates whether
#'    any is selected.
#'
#' Array expressions
#'  - `makeFrame(x)` an expression that creates an array from existing variables or expressions, see
#'    `deriveArray()` for more details
#'  - `tiered(x, tiers)` collapses a categorical array to the first value of tiers that is found
#'    (`tiers` use the category ids only, so is for advanced use only, [`tieredVar()`] is a nicer
#'    interface, but does not provide an expression, nor work on expressions).
#'  - `alterCategoriesExpr(x, categories = NULL, category_order = NULL, subvariables = NULL)`
#'     Change the category names, order, or subvariable names of categorical or Array variables
#'      (can only modify existing ones, not add or remove categories or subvariables). `categories`
#'     is a `Categories` object or a list of lists, each with a `name` indicating the new name, as
#'     well as an `id` or `old_name` to identify which category to modify.
#'     `category_order` is either a numeric vector indicating category ids or a character vector
#'     indicating the names of the categories in the order they should be displayed
#'     (note that all categories must be specified). `subvariables` is  a list of lists, each with
#'     a `name` to rename the subvariable and an `alias`, `old_nam` or `id` to identify the subvariable.
#'     When `x` is an expression, all categories and subvariables must be identified by `id`.
#'  - `arraySubsetExpr(x, subvars, subvar_id = c("alias", "name", "id"))` Take a subset of an existing
#'    array variable, identifying the subvars by alias, name, or id (if `x` is an expression,
#'    you must use id).
#'
#' Miscellaneous expressions
#'  - `caseExpr(..., cases)` Create a categorical variable from
#'    a set of logical expressions that when met are assigned to a category. See
#'    [`makeCaseVariable()`] for more details.
#'  - `fillExpr(..., fills)` Create a categorical variable by assigning existing categories
#'     to be filled in by values from another categorical variable See [`makeFillVariable()`]
#'     for more details.
#'  - `bin(x)` returns a column's values binned into equidistant bins.
#'  - `charLength(x)` returns a numeric value indicating the length of a string (or missing reason)
#'     in a `TextVariable` (or expression that creates one)
#'  - `unmissing(x)` for a `NumericVariable` (or expression that creates one) return the values of
#'    the data, ignoring the ones set to missing.
#'  - `normalize(x)` for a `NumericVariable` (or expression that creates one) return values normalized
#'    so that the sum of all values is equal to the number of observations.
#'  - `trim(x, min, max)` for a `NumericVariable` (or expression that creates one) return values that
#'    where all values less than `min` have been replaced with `min` and all values greater than
#'    `max` have been
#'  - `crunchDifftime(e1, e2, resolution)` Gets the difference between two datetimes as a number with
#'    specified resolution units (one of `c("Y", "Q", "M", "W", "D", "h", "m", "s", "ms")`).
#'  - `datetimeFromCols(year, month, day, hours, minutes, seconds)` create a `Datetime` variable from numeric variables
#'    or expressions (`year`, `month`, and `day` are required, but `hours`, `minutes`, and `seconds` are
#'    optional)
#'  - `rollup(x, resolution)` sets the resolution of a datetime variable or expression, see `rollup()`
#'
#' @name expressions
#' @param fun The name of the crunch database function to call
#' @param x An input, a crunch variable, expression or R object
#' @param ... Other arguments passed to the database function
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

#' @rdname expressions-internal
#' @export
tiered <- function(x, tiers) {
    isVarButNotType(x, "Array", "tiered")
    zfuncExpr("tiered", x, list(value = I(tiers)))
}

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

#' @rdname expressions
#' @export
makeFrame <- function(x) {
    ## Get subvariable URLs
    if (is.dataset(x)) {
        ## as in, if the list of variables is a [ extraction from a Dataset
        x <- allVariables(x)
    }

    # if it's a list, it could contain variable definitions:
    if (is.list(x)) {
        x <- x[lengths(x) > 0] # remove NULLs (from eg slider)
        x <- lapply(x, function(sv) {
            if (is.VarDef(sv)) {
                out <- sv$derivation
                out$references <- sv[names(sv) != "derivation"]
                out
            } else {
                list(variable = urls(sv))
            }
        })
    } else { # but ShojiCatalogs don't give their urls when lapplying, so treat differently
        x <- lapply(urls(x), function(sv) list(variable = sv))
    }

    subvarids <- as.character(seq_along(x))
    expression <- zfunc("array", zfunc(
        "make_frame",
        list(map = structure(x, .Names = subvarids)),
        list(value = I(subvarids))
    ))
    # TODO: filters are not preserved in makeFrame expressions because
    # they aren't preserved in `VarDefs` which expressions are wrapped in
    # when forming variables... I believe this will only affect someone trying to
    # `as.vector()` an array, which also doesn't currently work so leave it for now.
    CrunchExpr(expression = expression)
}

#' @rdname expressions-internal
#' @export
selectCategories <- function(x, selections, collapse = TRUE) {
    out <- zfuncExpr("select_categories", x, list(value = I(selections)))
    if (collapse) out <- asSelected(out)
    out
}

#' @rdname expressions-internal
#' @export
between <- function(x, lower, upper, inclusive = c(TRUE, FALSE)) {
    isVarButNotType(x, "Numeric", "between")
    zfuncExpr("between", x, lower, upper, list(value = I(inclusive)))
}

#' @rdname expressions-internal
#' @export
setMethod("all", c("CrunchVarOrExpr", "ANY"), function(x, ..., na.rm = FALSE) {
    isVarButNotType(x, "Array", "all")
    if (length(list(...)) > 0) halt("crunch::all() only works on arrays so can only take a single argument")
    if (!isFALSE(na.rm)) warning("na.rm ignored by crunch::all()")
    zfuncExpr("all", x, ...)
})

#' @rdname expressions-internal
#' @export
setMethod("any", c("CrunchVarOrExpr", "ANY"), function(x, ..., na.rm = FALSE) {
    isVarButNotType(x, "Array", "any")
    if (length(list(...)) > 0) halt("crunch::any() only works on arrays so can only take a single argument")
    func_name <- if (na.rm) "anynm" else "any"
    zfuncExpr(func_name, x)
})

#' @rdname expressions-internal
#' @export
isNoneAbove <- function(x) {
    isVarButNotType(x, "Array", "isNoneAbove")
    zfuncExpr("is_none_of_the_above", x)
}

#' @rdname expressions-internal
#' @export
textContains <- function(x, regex, ignore_case = FALSE) {
    isVarButNotType(x, "Text", "textContains")
    func_name <- if (ignore_case) "icontains" else "contains"
    zfuncExpr(func_name, x, regex)
}

#' @rdname expressions-internal
#' @export
setMethod("anyNA", "CrunchVarOrExpr", function(x, recursive = FALSE) {
    isVarButNotType(x, "Array", "anyNA")
    if (!isFALSE(recursive)) warning("recursive ignored by crunch::anyNA()")
    zfuncExpr("any_missing", x)
})

#' @rdname expressions-internal
#' @export
allNA <- function(x) {
    isVarButNotType(x, "Array", "all_missing")
    zfuncExpr("all_missing", x)
}

#' @rdname expressions-internal
#' @export
allValid <- function(x) {
    isVarButNotType(x, "Array", "allValid")
    zfuncExpr("all_valid", x)
}

#' @rdname expressions-internal
#' @export
completeCases <- function(x) {
    isVarButNotType(x, "Array", "completeCases")
    zfuncExpr("complete_cases", x)
}

#' @rdname expressions-internal
#' @export
setMethod("is.selected", "CrunchVarOrExpr", function(x) {
    isVarButNotType(x, "Categorical", "is.selected")
    zfuncExpr("selected", x)
})

#' @rdname expressions-internal
#' @export
is.notSelected <- function(x) {
    isVarButNotType(x, "Categorical", "is.notSelected")
    zfuncExpr("not_selected", x)
}

#' @rdname expressions-internal
#' @export
asSelected <- function(x) {
    isVarButNotType(x, c("Categorical", "Categorical Array", "Multiple Response"), "asSelected")
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
charLength <- function(x) {
    isVarButNotType(x, "Text", "charLength")
    zfuncExpr("char_length", x)
}

#' @rdname expressions-internal
#' @export
unmissing <- function(x) {
    isVarButNotType(x, "Numeric", "unmissing")
    zfuncExpr("unmissing", x)
}

#' @rdname expressions-internal
#' @export
normalize <- function(x) {
    isVarButNotType(x, "Numeric", "normalize")
    zfuncExpr("normalize", x)
}

#' @rdname expressions-internal
#' @export
trim <- function(x, min, max) {
    isVarButNotType(x, "Numeric", "trim")
    zfuncExpr("trim", x, min, max)
}

#' @rdname expressions-internal
#' @export
alterCategoriesExpr <- function(x, categories = NULL, category_order = NULL, subvariables = NULL) {
    isVarButNotType(x, "Array", "alterCategoriesExpr")

    if (is.categories(categories)) {
        categories <- lapply(categories, unclass)
    }
    if (!is.null(categories)) {
        old_names <- if (is.variable(x)) setNames(as.list(ids(categories(x))), names(categories(x))) else NULL

        categories <- lapply(categories, function(cat) {
            if ("id" %in% names(cat)) {
                cat[!names(cat) %in% "selected"]
            } else if ("old_name" %in% names(cat)) {
                if (is.null(old_names)) halt("Must use category ids when modifying categories of an expression")
                id <- getElement(old_names, cat$old_name)
                if (is.null(id)) halt("Could not find category with old name '", cat$old_name, "'")
                c(list(id = id), cat[!names(cat) %in% c("selected", "old_name")])
            } else {
                halt("Must specify either id or old_name for each category update")
            }
        })
    }

    if (is.character(category_order)) {
        if (!is.variable(x)) halt("Must use category ids when reordering categories of an expression")

        matches <- match(category_order, names(categories(x)))
        if (any(is.na(matches))) {
            halt(
                "Categories ",
                paste0("'", category_order[is.na(matches)], "'", collapse = ","),
                " not found in data,"
            )
        }
        category_order <- ids(categories(x))[matches]
    }

    if (!is.null(subvariables)) {
        old_aliases <- if (is.variable(x)) setNames(as.list(ids(subvariables(x))), aliases(subvariables(x))) else NULL
        old_names <- if (is.variable(x)) setNames(as.list(ids(subvariables(x))), names(subvariables(x))) else NULL
        subvariables <- lapply(subvariables, function(sv) {
            if (!"name" %in% names(sv)) halt("All subvariables must have a name to update")
            if ("id" %in% names(sv)) {
                list(id = as.character(sv$id), name = sv$name)
            } else if (is.null(old_aliases)) {
                halt("Must use subvariable ids when modifying subvariable names of an expression")
            } else if ("alias" %in% names(sv)) {
                id <- getElement(old_aliases, sv$alias)
                if (is.null(id)) halt("Could not find subvariable with alias '", sv$alias, "'")
                list(id = id, name = sv$name)
            } else if ("old_name" %in% names(sv)) {
                id <- getElement(old_names, sv$old_name)
                if (is.null(id)) halt("Could not find subvariable with old name '", sv$old_name, "'")
                list(id = id, name = sv$name)
            }
        })
        args <- list()
        if (!is.null(categories)) args$categories <- categories
        if (!is.null(order)) args$order <- category_order
        if (!is.null(subvariables)) args$subvariables <- subvariables

        zfuncExpr("alter_categories", x, list(value = I(args)))
    }
}

#' @rdname expressions-internal
#' @export
arraySubsetExpr <- function(x, subvars, subvar_id = c("alias", "name", "id")) {
    isVarButNotType(x, "Array", "arraySubsetExpr")
    subvar_id <- match.arg(subvar_id)
    if (subvar_id != "id") {
        if (!is.variable(x)) halt("Must subset by id when subsetting an expression")

        if (subvar_id == "alias") {
            matches <- match(subvars, aliases(subvariables(x)))
        } else if (subvar_id == "name") {
            matches <- match(subvars, names(subvariables(x)))
        }

        if (any(is.na(matches))) {
            halt(
                "Could not find subvariables with ", subvar_id, " ",
                paste0("'", subvars[is.na(matches)], "'", collapse = ",")
            )
        }
        subvars <- ids(subvariables(x))[matches]
    }

    zfuncExpr("array_subset", x, list(value = I(subvars)))
}

#' @rdname crunch-is
#' @export
is.CrunchExpr <- function(x) inherits(x, "CrunchExpr")

#' @rdname crunch-is
#' @export
is.Expr <- is.CrunchExpr

isVarButNotType <- function(x, types, caller) {
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
        type_str <- paste0("'", types, "'", collapse = ",")
        halt("variable must be of type '", type_str, "' for ", caller, "().")
    }
}
