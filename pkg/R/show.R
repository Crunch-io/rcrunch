showCrunchVariable <- function (x) {
    out <- getNameAndType(x)
    desc <- x@body$description
    if (!is.null(desc)) out <- c(out, "", desc)
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
    cat(out, "\n")
    try(print(summary(object)), silent=TRUE)
    invisible(out)
})

setMethod("show", "CategoricalArrayVariable", function (object) {
    out <- c(showCrunchVariable(object), showSubvariables(subvariables(object)))
    cat(out)
    invisible(out)
})

showCategories <- function (x) {
    vapply(x, showCategory, character(1))
}

##' @importFrom methods show
##' @export
setMethod("show", "Categories", function (object) {
    out <- showCategories(object)
    cat(out, sep="\n")
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

showSubvariables <- function (object) {
    out <- c("Subvariables:", "", vapply(object@index, function (x) {
        paste0("  $`", x$name, "`\n")
    }, character(1)))
    return(doc(out))
}

##' @export
setMethod("show", "Subvariables", function (object) {
    out <- showSubvariables(object)
    cat(out)
    invisible(out)
})