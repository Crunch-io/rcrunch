##' Show methods for Crunch objects
##'
##' @param object the object
##' @return invisibly
##' @seealso \code{\link[methods]{show}}
##' @importFrom methods show
##' @name show-crunch
NULL


# Boilerplate

.showIt <- function (object) {
    out <- getShowContent(object)
    cat(out, sep="\n")
    invisible(out)
}

##' @rdname show-crunch
##' @export
setMethod("show", "ShojiObject", .showIt)

setMethod("show", "ShojiCatalog", function (object) {
    ## Catalog show content is a data.frame unless otherwise indicated.
    ## Print it, but capture the output so we can return the character output.
    out <- capture.output(print(getShowContent(object)))
    cat(out, sep="\n")
    invisible(out)
})

##' @rdname show-crunch
##' @export
setMethod("show", "CrunchVariable", .showIt)

##' @rdname show-crunch
##' @export
setMethod("show", "Category", .showIt)

##' @rdname show-crunch
##' @export
setMethod("show", "Categories", .showIt)


# Actual show methods

showCategory <- function (x) paste0("[ ", value(x), " ]  ", name(x))

showCategories <- function (x) vapply(x, showCategory, character(1))

showCrunchVariableTitle <- function (x) {
    out <- paste(getNameAndType(x), collapse=" ")
    desc <- description(x)
    if (!is.null(desc) && nchar(desc)) out <- c(out, desc)
    return(out)
}

getNameAndType <- function (x) {
    varname <- name(x)
    vartype <- paste0("(", type(x), ")")
    return(c(varname, vartype))
}

##' @importFrom utils capture.output
showCrunchVariable <- function (x) {
    out <- showCrunchVariableTitle(x)
    try(out <- c(out, "", capture.output(print(summary(x)))))
    invisible(out)
}

showCrunchDataset <- function (x) {
    out <- paste("Dataset", dQuote(name(x)))
    d <- description(x)
    if (!is.null(d) && nchar(d)) {
        out <- c(out, d)
    }

    out <- c(out,
            "",
            paste("Contains", nrow(x), "rows of", ncol(x), "variables:"),
            describeDatasetVariables(x))
    return(out)
}

describeDatasetVariables <- function (dataset) {
    nk <- namekey(dataset)
    return(vapply(variables(dataset), function (v) {
        paste0("$", v[[nk]], ": ", paste(getNameAndType(v), collapse=" "))
    }, character(1)))
}

showCategoricalArrayVariable <- function (x) {
    c(showCrunchVariableTitle(x), showSubvariables(subvariables(x)))
}

showSubvariables <- function (x) {
    out <- c("Subvariables:", vapply(index(x), function (s) {
        paste0("  $`", s$name, "`")
    }, character(1), USE.NAMES=FALSE))
    return(out)
}

showVariableOrder <- function (x, vars=x@vars) {
    return(unlist(lapply(x, showVariableGroup, index=vars)))
}

showVariableGroup <- function (x, index) {
    if (inherits(x, "VariableGroup")) {
        ents <- entities(x)
        if (length(ents)) {
            group <- unlist(lapply(ents, showVariableGroup, index=index))
        } else {
            group <- "(Empty group)"
        }
        out <- c(paste0("[+] ", name(x)), paste0("    ", group))
    } else {
        tup <- index[[x]] %||% list()
        out <- tup[["name"]] %||% "(Hidden variable)"
    }
    return(out)
}

formatVersionCatalog <- function (x, from=Sys.time()) {
    ts <- timestamps(x)
    if (!is.null(from)) {
        ts <- vapply(seq_along(ts), function (a) {
            ## Grab dates by sequence because POSIXt is a list internally
            ## (i.e. lapply does the wrong thing)
            this <- from - ts[a]
            num <- as.integer(this)
            un <- attr(this, "units")
            if (num == 1) {
                ## Make singular
                un <- sub("s$", "", un)
            }
            out <- paste(num, un, "ago")
            return(out)
        }, character(1))
    }
    return(data.frame(Name=names(x), Timestamp=ts, stringsAsFactors=FALSE))
}


# More boilerplate

setMethod("getShowContent", "Category", showCategory)
setMethod("getShowContent", "Categories", showCategories)
setMethod("getShowContent", "CrunchVariable", showCrunchVariable)
setMethod("getShowContent", "CategoricalArrayVariable",
    showCategoricalArrayVariable)
setMethod("getShowContent", "CrunchDataset", showCrunchDataset)
setMethod("getShowContent", "Subvariables", showSubvariables)
setMethod("getShowContent", "VariableOrder", showVariableOrder)
setMethod("getShowContent", "ShojiCatalog",
    function (x) catalogToDataFrame(x, TRUE))
setMethod("getShowContent", "BatchCatalog",
    function (x) catalogToDataFrame(x, c("id", "status"), rownames=NULL))
setMethod("getShowContent", "VariableCatalog",
    function (x) catalogToDataFrame(x, c("alias", "name", "type"), rownames=NULL))
setMethod("getShowContent", "VersionCatalog", formatVersionCatalog)
setMethod("getShowContent", "ShojiObject",
    function (x) capture.output(print(x@body)))

##' @rdname show-crunch
##' @export
setMethod("show", "CrunchCube", function (object) show(cubeToArray(object)))
