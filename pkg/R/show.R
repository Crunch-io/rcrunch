showCrunchVariable <- function (x) {
    out <- c(getNameAndType(x), "")
    desc <- x@body$description
    if (!is.null(desc)) out <- c(out, desc, "")
    out <- c(out, "")
    return(doc(out))
}

getNameAndType <- function (x) {
    varname <- name(x)
    vartype <- paste0("(", type(x), ")")
    return(c(varname, vartype))
}

##' @export
setMethod("show", "CrunchVariable", function (object) {
    out <- showCrunchVariable(object)
    cat(out)
    print(summary(object))
    invisible(out)
})

describeDatasetVariables <- function (dataset) {
    nk <- namekey(dataset)
    return(vapply(variables(dataset), function (v) {
        header <- paste0("$", v[[nk]], ":")
        paste(c(header, getNameAndType(v), "\n"), collapse=" ")
    }, character(1)))
}

showCrunchDataset <- function (x) {
    n <- sQuote(name(x))
    out <- c("Dataset", n, "")
    if (!is.null(x@body$description)) {
        out <- c(out, x@body$description, "")
    }
    
    out <- c(out, 
            "", 
            "Contains", nrow(x), "rows of", ncol(x), "variables:", "",
            "")
    vars <- describeDatasetVariables(x)
    out <- c(out, vars)
    
    return(doc(out))
}

##' @export
setMethod("show", "CrunchDataset", function (object) {
    out <- showCrunchDataset(object)
    cat(out)
    invisible(out)
})
