#' Construct Crunch Expressions
#'
#' Crunch Expressions, i.e. `CrunchExpr` and `CrunchLogicalExpr`,
#' encapsulate derivations of Crunch variables, which are only evaluated when
#' passed to a function like `as.vector`. They allow you to compose
#' functional expressions of variables and evaluate them against the server
#' only when appropriate.
#' @param x an input
#' @param e1 an input
#' @param e2 an input
#' @param table For \code{\%in\%}. See [`base::match`]
#' @param resolution For `rollup`. Either `NULL` or a character in
#' c("Y", "Q", "M", "W", "D", "h", "m", "s", "ms") indicating the unit of
#' time at which a Datetime variable should be aggregated. If `NULL`,
#' the server will determine an appropriate resolution based on the range of
#' the data.
#' @return Most functions return a CrunchExpr or CrunchLogicalExpr.
#' `as.vector` returns an R vector.
#' @aliases expressions
#' @name expressions
NULL

#' @rdname variable-to-R
#' @export
setMethod("as.vector", "CrunchExpr", function (x, mode) {
    payload <- list(query=toJSON(list(out=zcl(x))))
    if (length(x@filter)) {
        payload[["filter"]] <- toJSON(x@filter)
    } else {
        payload$filter <- "{}"
    }
    out <- paginatedGET(paste0(x@dataset_url, "table/"),
        query=payload, table=TRUE, limit=.crunchPageSize(x))
    ## pass in the variable metadata to the column parser
    variable <- VariableEntity(structure(list(body=out$metadata$out),
        class="shoji"))
    return(columnParser(out$metadata$out$type)(out$data$out, variable, mode))
})

#' @rdname toVariable
#' @export
setMethod("toVariable", "CrunchExpr", function (x, ...) {
    structure(list(derivation=zcl(x), ...), class="VariableDefinition")
})

## "Ops" for Crunch Variables
##
## Most of the indirection here is to programatically create the Ops methods
## for the right combinations of multiple-dispatch signatures

math.exp <- function (e1, e2, operator) {
    ## Generic function that creates CrunchExpr of `e1 %operator% e2`
    if (identical(e1, logical(0)) || identical(e2, logical(0))) {
        ## If you reference a variable in a dataset that doesn't exist, you
        ## get NULL, and e.g. NULL == something becomes logical(0).
        ## That does awful things if you try to send to the server. So don't.
        ##
        ## Because of how this function is invoked, get the offending expression
        ## from the call before this one
        halt("Invalid expression (probably a reference to a variable that doesn't exist): ",
             deparseAndFlatten(tail(sys.calls(), 2)[[1]]))
    }
    ex <- zfunc(operator, e1, e2)
    ds.url <- unique(unlist(lapply(list(e1, e2), datasetReference))) %||% ""

    ## If either e1 or e2 are Crunch objects with filters, pass those along,
    ## and if both do, make sure that they're the same
    f1 <- try(activeFilter(e1), silent=TRUE)
    f2 <- try(activeFilter(e2), silent=TRUE)
    if (is.error(f1)) {
        if (is.error(f2)) {
            ## Neither object is a Crunch object? We shouldn't be here.
            filt <- NULL
        } else {
            filt <- f2
        }
    } else if (is.error(f2)) {
        filt <- f1
    } else {
        ## Ok: Both are Crunch objects. Reject if filters aren't identical.
        if (!identical(f1, f2)) {
            halt("Cannot combine expressions with different filters")
        }
        filt <- f1
    }
    out <- ExprConstructor(operator)(expression=ex, dataset_url=ds.url)
    activeFilter(out) <- filt
    return(out)
}

ExprConstructor <- function (operator) {
    ## Based on the operator function, make either CrunchExpr or CrunchLogicalExpr
    logics <- c("in", "<", ">", ">=", "<=", "==", "!=", "and", "or", "not", "is_missing", "duplicates")
    if (operator %in% logics) {
        Constructor <- CrunchLogicalExpr
    } else {
        Constructor <- CrunchExpr
    }
    return(Constructor)
}

crunch.ops <- function (i) {
    ## Create math.exp of Variable x R.object, R.object x Variable, or V x V
    force(i)
    return(function (e1, e2) math.exp(e1, e2, i))
}

.sigs <- list(
    c("TextVariable", "character"),
    c("NumericVariable", "numeric"),
    c("DatetimeVariable", "Date"),
    c("DatetimeVariable", "POSIXt"),
    c("DatetimeVariable", "character"), ## TODO: validate that the "character" is valid 8601?
    c("CategoricalVariable", "numeric") ## TODO: add cast(x, "numeric") around var for this?
)

.rtypes <- unique(vapply(.sigs, function (a) a[[2]], character(1)))
.nomath <- which(!vapply(.sigs,
    function (a) a[[1]] %in% c("TextVariable", "CategoricalVariable"),
    logical(1)))

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

setMethod("&", c("CrunchExpr", "CrunchExpr"), crunch.ops("and"))
setMethod("&", c("logical", "CrunchExpr"), crunch.ops("and"))
setMethod("&", c("CrunchExpr", "logical"), crunch.ops("and"))
setMethod("|", c("CrunchExpr", "CrunchExpr"), crunch.ops("or"))
setMethod("|", c("logical", "CrunchExpr"), crunch.ops("or"))
setMethod("|", c("CrunchExpr", "logical"), crunch.ops("or"))


zfuncExpr <- function (fun, x, ...) {
    ## Wrap zfunc(fun, x) in a way that preserves x's active filter
    ## Returns CrunchExpr instead of zcl/list
    ## Currently only implemented for one arg with a filter (x)
    out <- ExprConstructor(fun)(expression=zfunc(fun, x, ...),
        dataset_url=datasetReference(x) %||% "")
    activeFilter(out) <- activeFilter(x)
    return(out)
}

#' @rdname expressions
#' @export
setMethod("!", "CrunchExpr", function (x) zfuncExpr("not", x))

#' @importFrom utils head tail
.seqCrunch <- function (x, table) {
    ## Given x %in% table, if table is numeric, see if we can/should collapse
    ## it into a range query rather than sending lots of distinct values

    if (is.numeric(table) &&
        length(table) > 2 &&
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

        return(zfunc("between",
            x,
            beg,
            end + 1))
            ## Add 1 because "between" by default doesn't include the upper
            ## bound and explicitly overriding that is failing. See #112089103.
            ## When that is fixed, we can do the following:
            #rep(TRUE, 2L))) ## Inclusive on both sides
    } else {
        return(zfunc(ifelse(length(table) == 1L, "==", "in"), x, table))
    }
}

.inCrunch <- function (x, table) {
    ## TODO: bring in .seqCrunch here, once it is working
    math.exp(x, table, ifelse(length(table) == 1L, "==", "in"))
}

#' @rdname expressions
#' @export
setMethod("%in%", c("CategoricalVariable", "character"),
    function (x, table) .inCrunch(x, n2i(table, categories(x), strict=FALSE)))
#' @rdname expressions
#' @export
setMethod("%in%", c("CategoricalVariable", "factor"),
    function (x, table) x %in% as.character(table))

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

## Use %in% because it handles the possibility that e2 is not a valid category

#' @rdname expressions
#' @export
setMethod("==", c("CategoricalVariable", "character"),
    function (e1, e2) e1 %in% e2)
#' @rdname expressions
#' @export
setMethod("==", c("CategoricalVariable", "factor"),
    function (e1, e2) e1 %in% e2)
#' @rdname expressions
#' @export
setMethod("!=", c("CategoricalVariable", "character"), function (e1, e2) {
    e2 <- n2i(e2, categories(e1), strict=FALSE)
    neq <- length(e2) == 1
    out <- math.exp(e1, e2, ifelse(neq, "!=", "in"))
    if (!neq) {
        ## We did "in" above, so make that "not in"
        out <- !out
    }
    return(out)
})
#' @rdname expressions
#' @export
setMethod("!=", c("CategoricalVariable", "factor"),
    function (e1, e2) e1 != as.character(e2))

setMethod("datasetReference", "CrunchExpr", function (x) x@dataset_url)

#' @rdname expressions
#' @export
setMethod("is.na", "CrunchVariable", function (x) zfuncExpr("is_missing", x))

#' @rdname expressions
#' @export
bin <- function (x) zfuncExpr("bin", x)


#' @rdname expressions
#' @export
rollup <- function (x, resolution=rollupResolution(x)) {
    valid_res <- c("Y", "Q", "M", "W", "D", "h", "m", "s", "ms")
    force(resolution)
    if (!is.null(resolution) && !(resolution %in% valid_res)) {
        halt(dQuote("resolution"), " is invalid. Valid values are NULL, ",
            serialPaste(valid_res))
    }
    if (is.variable(x) && !is.Datetime(x)) {
        halt("Cannot rollup a variable of type ", dQuote(type(x)))
    }
    return(zfuncExpr("rollup", x, list(value=resolution)))
}

rollupResolution <- function (x) {
    if (is.Datetime(x)) {
        return(tuple(x)$rollup_resolution)
    } else {
        return(NULL)
    }
}

#' @rdname variable-extract
#' @export
setMethod("[", c("CrunchExpr", "CrunchLogicalExpr"), .updateActiveFilter)

.updateActiveFilterLogical <- function (x, i, ...) {
    if (length(i)) {
        i <- CrunchLogicalExpr(dataset_url=datasetReference(x),
            expression=.dispatchFilter(i))
        return(x[i])
    } else {
        ## If you reference a variable in a dataset that doesn't exist, you
        ## get NULL, and e.g. NULL == something becomes logical(0).
        ## That does awful things if you try to send to the server. So don't.
        halt("Invalid expression: ", deparseAndFlatten(match.call()$i))
    }
}

#' @rdname variable-extract
#' @export
setMethod("[", c("CrunchExpr", "logical"), .updateActiveFilterLogical)

#' @rdname variable-extract
#' @export
setMethod("[", c("CrunchExpr", "numeric"), function (x, i, ...) {
    i <- CrunchLogicalExpr(dataset_url=datasetReference(x),
        expression=.dispatchFilter(i))
    return(x[i])
})

#' "which" method for CrunchLogicalExpr
#'
#' NOTE: this isn't correct. Don't use it yet.
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
setMethod("which", "CrunchLogicalExpr", function (x, arr.ind, useNames) {
    as.integer(as.vector(CrunchExpr(expression=zfunc("row"),
        dataset_url=datasetReference(x) %||% "")[x])) + 1L
})

#' "duplicated" method for Crunch objects
#'
#' @param x CrunchVariable or CrunchExpr
#' @param incomparables Ignored
#' @param ... Ignored
#' @return A CrunchLogicalExpr that evaluates `TRUE` for all repeated
#' entries after the first occurrence of a value.
#' @name duplicated
#' @seealso [`base::duplicated`]
#' @aliases duplicated
#' @export
NULL

#' @rdname duplicated
#' @export
setMethod("duplicated", "CrunchVariable", function (x, incomparables=FALSE, ...) {
    zfuncExpr("duplicates", x)
})

#' @rdname duplicated
#' @export
setMethod("duplicated", "CrunchExpr", function (x, incomparables=FALSE, ...) {
    zfuncExpr("duplicates", x)
})

#' @rdname crunch-is
#' @export
is.CrunchExpr <- function (x) inherits(x, "CrunchExpr")

#' @rdname crunch-is
#' @export
is.Expr <- is.CrunchExpr
