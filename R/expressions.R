#' Construct Crunch Expressions
#'
#' @description
#' Crunch Expressions, i.e. `CrunchExpr` and `CrunchLogicalExpr`,
#' encapsulate derivations of Crunch variables, which are only evaluated when
#' passed to a function like `VarDef()` `as.vector()`. They allow you to compose
#' functional expressions of variables and evaluate them against the server
#' only when appropriate. Crunch uses `selection` roughly equivalent to base R's
#' logical, where `selected` in Crunch is `TRUE` in R, `other` is `FALSE`, and
#' `No data` is `NA`, the rcrunch documentation may switch between these conventions.
#'
#' Logical expressions
#' - These logical operators `==`, `!=`, `&`, `|`, `!`,`%in%`  work the same way as their
#'   base R counterparts
#'  - `is.selected(x)`, `is.notSelected(x)` return `CrunchLogicalExpr` whether a value is (or is not) in
#'  a selected category (missing data is considered not selected, unlike `!` which preserves missing)
#' - `any(x)` and `all(x)` work rowwise on `MultipleResponse` Variables (and expressions), though `na.rm`
#'   is not implemented for `all(x)`.
#'   `%ornm%` is similar tos `|`, but where "not selected" beats "missing" (so `FALSE %ornm% NA` is
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
#'   is misssing (or not missing).
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
#'  - `arraySelections(x)` returns an expression that takes an array and creates an array with each vaiable
#'    condensed to "selected", "other" or "missing" and an extra subvariable "__any__" that indicates whether
#'    any is selected.
#'
#' Array expressions
#'  - `makeFrame(x)` an expression that creates an array from existing variables or expressions, see
#'    `deriveArray()` for more details
#'  - `tiered(x, tiers)` collapses a categorical array to the first value of tiers that is found
#'    (`tiers` use the category ids only, so is for advanced use only).
#'
#' Miscellaneous expressions
#'  - `makeCaseExpr(..., cases, data = NULL)` Create a categorical variable from
#'    a set of logical expressions (cases). See `makeCaseVariable()` for more
#'    details.
#'  - `bin(x)` returns a column's values binned into equidistant bins.
#'  - `charLength(x)` returns a numeric value indicating the length of a string (or missing reason)
#'     in a `TextVariable` (or expression that creates one)
#'  - `unmissing(x)` for a `NumericVariable` (or expression that creates one) return the values of
#'    the data, ignoring the ones set to missing.
#'  - `normalize(x)` for a `NumericVariable` (or rexpression that creates one) return values normalized
#'    so that the sum of all values is equal to the number of observations.
#'  - `crunchDifftime(e1, e2, resolution)` Gets the difference between two datetimes as a number with
#'    specified resolution units (one of `c("Y", "Q", "M", "W", "D", "h", "m", "s", "ms")`).
#'  - `datetimeFromCols(year, month, day, hours, minutes, seconds)` create a `Datetime` variable from numeric variables
#'    or expressions (`year`, `month`, and `day` are required, but `hours`, `minutes`, and `seconds` are
#'    optional)
#'  - `rollup(x, resolution)` sets the resolution of a datetime variable or expression, see `rollup()`
#'
#' @param x an input
#' @param e1 an input
#' @param e2 an input
#' @param table For \code{\%in\%}. See [base::match()]
#' @return Most functions return a CrunchExpr or CrunchLogicalExpr.
#' `as.vector` returns an R vector.
#' @aliases expressions %in% == != !
#' @name expressions
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

#' @rdname expressions
#' @export
crunchDifftime <- function(e1, e2, resolution = NULL) {
    isVarButNotType(e1, "Datetime", "crunchDifftime")
    isVarButNotType(e2, "Datetime", "crunchDifftime")
    crunch.ops("difftime", resolution)
}

#' @rdname expressions
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

#' @rdname expressions
#' @export
`%ornm%` <- function(e1, e2) {
    crunch.ops("ornm")
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

#' @rdname expressions
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

#' @rdname expressions
#' @export
setMethod(
    "%in%", c("CategoricalVariable", "character"),
    function(x, table) .inCrunch(x, n2i(table, categories(x), strict = FALSE))
)
#' @rdname expressions
#' @export
setMethod(
    "%in%", c("CategoricalVariable", "factor"),
    function(x, table) x %in% as.character(table)
)

## Iterated version of below:
for (i in seq_along(.sigs)) {
    setMethod("%in%", .sigs[[i]], .inCrunch)
}

#' @rdname expressions
#' @export
setMethod("%in%", c("TextVariable", "character"), .inCrunch)
#' @rdname expressions
#' @export
setMethod("%in%", c("NumericVariable", "numeric"), .inCrunch)
#' @rdname expressions
#' @export
setMethod("%in%", c("DatetimeVariable", "Date"), .inCrunch)
#' @rdname expressions
#' @export
setMethod("%in%", c("DatetimeVariable", "POSIXt"), .inCrunch)
#' @rdname expressions
#' @export
setMethod("%in%", c("DatetimeVariable", "character"), .inCrunch)
#' @rdname expressions
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

#' @rdname expressions
#' @export
setMethod("==", c("CategoricalVariable", "numeric"), function(e1, e2) {
    if (length(e2) == 0) {
        ## The specified category doesn't exist. But `== BAD` breaks server
        ## However, "in []" is fine
        return(math.exp(e1, e2, "in"))
    }
    return(math.exp(e1, e2, "=="))
})

#' @rdname expressions
#' @export
setMethod("==", c("CategoricalVariable", "character"), function(e1, e2) {
    e2 <- n2i(e2, categories(e1), strict = FALSE)
    return(e1 == e2)
})
#' @rdname expressions
#' @export
setMethod(
    "==", c("CategoricalVariable", "factor"),
    function(e1, e2) e1 == as.character(e2)
)

#' @rdname expressions
#' @export
setMethod("!=", c("CategoricalVariable", "numeric"), function(e1, e2) {
    if (length(e2) == 0) {
        ## The specified category was doesn't exist. But `== BAD` breaks server
        ## However, "in []" is fine. So do `not (in [])`
        return(!math.exp(e1, e2, "in"))
    }
    return(math.exp(e1, e2, "!="))
})
#' @rdname expressions
#' @export
setMethod("!=", c("CategoricalVariable", "character"), function(e1, e2) {
    e2 <- n2i(e2, categories(e1), strict = FALSE)
    return(e1 != e2)
})
#' @rdname expressions
#' @export
setMethod(
    "!=", c("CategoricalVariable", "factor"),
    function(e1, e2) e1 != as.character(e2)
)

#' @rdname expressions
#' @export
setMethod("is.na", "CrunchVarOrExpr", function(x) zfuncExpr("is_missing", x))

#' @rdname expressions
#' @export
is.valid <- function(x) zfuncExpr("is_valid", x)

#' @rdname expressions
#' @export
bin <- function(x) zfuncExpr("bin", x)

#' @rdname expressions
#' @export
tiered <- function(x, tiers) zfuncExpr("tiered", x, list(value = I(tiers)))

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

#' @rdname makeArray
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
    # TODO: compare subvar filters and send if equal (and error otherwise)
    CrunchExpr(expression = expression)
}

#' @rdname expressions
#' @export
selectCategories <- function(x, selections, collapse = TRUE) {
    out <- zfuncExpr("select_categories", x, list(value = I(selections)))
    if (collapse) out <- asSelected(out)
    out
}

#' @rdname expressions
#' @export
between <- function(x, lower, upper, inclusive = c(TRUE, FALSE)) {
    isVarButNotType(x, "Numeric", "between")
    zfuncExpr("between", x, lower, upper, list(value = I(inclusive)))
}

#' @rdname expressions
#' @export
setMethod("all", c("CrunchVarOrExpr", "ANY"), function(x, ..., na.rm = FALSE) {
    isVarButNotType(x, "Array", "all")
    if (length(list(...)) > 0) halt("crunch::all() only works on arrays so can only take a single argument")
    if (!isFALSE(na.rm)) warning("na.rm ignored by crunch::all()")
    zfuncExpr("all", x, ...)
})

#' @rdname expressions
#' @export
setMethod("any", c("CrunchVarOrExpr", "ANY"), function(x, ..., na.rm = FALSE) {
    isVarButNotType(x, "Array", "any")
    if (length(list(...)) > 0) halt("crunch::any() only works on arrays so can only take a single argument")
    func_name <- if (na.rm) "anynm" else "any"
    zfuncExpr(func_name, x)
})

#' @rdname expressions
#' @export
isNoneAbove <- function(x) {
    isVarButNotType(x, "Array", "isNoneAbove")
    zfuncExpr("is_none_of_the_above", x)
}

#' @rdname expressions
#' @export
textContains <- function(x, regex, ignore_case = FALSE) {
    isVarButNotType(x, "Text", "textContains")
    func_name <- if (ignore_case) "icontains" else "contains"
    zfuncExpr(func_name, x, regex)
}

#' @rdname expressions
#' @export
setMethod("anyNA", "CrunchVarOrExpr", function(x, recursive = FALSE) {
    isVarButNotType(x, "Array", "anyNA")
    if (!isFALSE(recursive)) warning("recursive ignored by crunch::anyNA()")
    zfuncExpr("any_missing", x)
})

#' @rdname expressions
#' @export
allNA <- function(x) {
    isVarButNotType(x, "Array", "all_missing")
    zfuncExpr("all_missing", x)
}

#' @rdname expressions
#' @export
allValid <- function(x) {
    isVarButNotType(x, "Array", "allValid")
    zfuncExpr("all_valid", x)
}

#' @rdname expressions
#' @export
completeCases <- function(x) {
    isVarButNotType(x, "Array", "completeCases")
    zfuncExpr("completeCases", x)
}

#' @rdname expressions
#' @export
setMethod("is.selected", "CrunchVarOrExpr", function(x) {
    isVarButNotType(x, "Categorical", "is.selected")
    zfuncExpr("selected", x)
})

#' @rdname expressions
#' @export
is.notSelected <- function(x) {
    isVarButNotType(x, "Categorical", "is.notSelected")
    zfuncExpr("not_selected", x)
}

#' @rdname expressions
#' @export
asSelected <- function(x) {
    isVarButNotType(x, "Multiple Response", "asSelected")
    zfuncExpr("as_selected", x)
}

#' @rdname expressions
#' @export
selectedDepth <- function(x) {
    isVarButNotType(x, "Multiple Response", "selectedDepth")
    zfuncExpr("selected_depth", x)
}

#' @rdname expressions
#' @export
arraySelections <- function(x) {
    isVarButNotType(x, "Multiple Response", "arraySelections")
    zfuncExpr("selections", x)
}

#' @rdname expressions
#' @export
charLength <- function(x) {
    isVarButNotType(x, "Text", "charLength")
    zfuncExpr("char_length", x)
}

#' @rdname expressions
#' @export
unmissing <- function(x) {
    isVarButNotType(x, "Numeric", "unmissing")
    zfuncExpr("unmissing", x)
}

#' @rdname expressions
#' @export
normalize <- function(x) {
    isVarButNotType(x, "Numeric", "normalize")
    zfuncExpr("normalize", x)
}


#' @rdname crunch-is
#' @export
is.CrunchExpr <- function(x) inherits(x, "CrunchExpr")

#' @rdname crunch-is
#' @export
is.Expr <- is.CrunchExpr

isVarButNotType <- function(x, type, caller) {
    type_check <- switch(
        type,
        "Numeric" = is.Numeric,
        "Text" = is.Text,
        "Datetime" = is.Datetime,
        "Categorical" = is.Categorical,
        "Array" = is.Array,
        "Categorical Array" = is.CategoricalArray,
        "Multiple Response" = is.MultipleResponse
    )
    if (is.variable(x) & !type_check(x)) {
        halt("variable must be of type '", type, "' for ", caller, "().")
    }
}
