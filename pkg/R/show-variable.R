showCrunchVariable <- function (x) {
    out <- c(getNameAndType(x), "")
    desc <- x@body$description
    if (!is.null(desc)) out <- c(out, desc, "")
    out <- c(out, "")
    return(doc(out))
}

getNameAndType <- function (x) {
    varname <- x@body$name
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