## Functions to make ZZ9 Command Language query objects

r2zcl <- function(x) {
    ## Convert an R vector to a value/column to be sent in a ZCL request.
    ## Called inside the zcl() method.
    v <- toVariable(x)
    attributes(v$values) <- NULL

    ## "column" if we're sending an array. If scalar, it is "value"
    if (length(x) != 1) {
        out <- as.zcl(column = v$values)
    } else if (inherits(x, "AsIs")) {
        out <- as.zcl(column = I(v$values))
    } else {
        out <- as.zcl(value = v$values)
    }
    return(out)
}

as.zcl <- function(...) structure(list(...), class = "zcl")

## Methods to convert various objects to ZCL
setMethod("zcl", "CrunchExpr", function(x) x@expression)
setMethod("zcl", "CrunchVariable", function(x) list(variable = self(x)))
setMethod("zcl", "VariableTuple", function(x) list(variable = self(x)))
setMethod("zcl", "numeric", r2zcl)
setMethod("zcl", "character", r2zcl)
setMethod("zcl", "Date", r2zcl)
setMethod("zcl", "POSIXt", r2zcl)
setMethod("zcl", "logical", function(x) {
    if (length(x)) {
        ## 3VL categorical
        out <- r2zcl(x)
        out$type <- list(class = "categorical", categories = .selected.cats)
        return(out)
    } else {
        ## If you reference a variable in a dataset that doesn't exist, you
        ## get NULL, and e.g. NULL == something becomes logical(0).
        ## That does awful things if you try to send to the server. So don't.
        halt("Invalid expression. Probably a reference to a variable that doesn't exist.")
    }
})
setMethod("zcl", "NULL", function(x) NULL)
setOldClass("zcl")
setMethod("zcl", "zcl", function(x) x)
setMethod("zcl", "list", function(x) x) ## is this a good idea?
setMethod("zcl", "CrunchFilter", function(x) x@body$expression)

zfunc <- function(func, ...) {
    ## Wrapper that creates ZCL function syntax
    dots <- lapply(list(...), zcl)
    if (is.null(names(dots))) {
        unnamed_dots <- dots
        named_dots <- NULL
    } else {
        unnamed_dots <- dots[names(dots) == ""]
        named_dots <- dots[names(dots) != ""]
    }

    out <- list(`function` = func)
    if (!is.null(unnamed_dots)) out["args"] <- list(unnamed_dots)
    if (!is.null(named_dots)) out["kwargs"] <- list(named_dots)

    return(out)
}

# check if a template or query has a particular ZCL function somewhere in it recursively.
has.function <- function(query, funcs) {
    query <- unlist(query, recursive = TRUE)

    func_names <- grepl("function$", names(query))
    func_names <- names(query)[func_names]

    if (any(query[func_names] %in% funcs)) {
        return(TRUE)
    }

    return(FALSE)
}
