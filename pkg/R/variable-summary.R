getSummary <- function (x) {
    url <- shojiURL(x, "views", "summary")
    if (is.null(url)) {
        halt("No summary available")
    }
    out <- crGET(url)
    ## Summaries don't return as shoji entities
    # if (!is.shoji(url)) {
    #     halt("Error in retrieving summary")
    # }
    if (is.Datetime(x)) {
        toR <- columnParser("datetime")
        for (i in c("min", "max")) out[[i]] <- toR(out[[i]])
    }
    return(out)
}

getFrequencies <- function (x) {
    url <- shojiURL(x, "views", "frequencies")
    if (is.null(url)) {
        halt("No frequencies available")
    }
    url <- crGET(url)
    return(url)
}

makeCategoricalTable <- function (frequencies, add.na=FALSE) {
    freqs <- .na.omit.categories(frequencies)
    counts <- selectFrom("count", freqs)
    n <- selectFrom("name", freqs)
    if (add.na) {
        n <- c(n, "<NA>")
        missings <- selectFromWhere(isTRUE(missing), frequencies, key="count")
        counts <- c(counts, sum(missings))
    }
    return(structure(as.integer(counts), 
        .Dim = length(counts),
        .Dimnames = structure(list(n), .Names = ""), 
        class = "table"))
}

CategoricalVariable.table <- function (..., 
                    exclude = if (useNA == "no") c(NA, NaN), 
                    useNA = c("no", "ifany", "always"), dnn = list.names(...),
                    deparse.level = 1) {
    var <- ..1
    summ <- getSummary(var)
    useNA <- match.arg(useNA)
    add.na <- useNA=="always" || (useNA=="ifany" && summ$missing_count>0)
    return(makeCategoricalTable(summ$categories, add.na))
}

##' Table function for Crunch objects
##'
##' @param ... things to tabulate
##' @param exclude see \code{link{base:table}}
##' @param useNA see \code{link{base:table}}
##' @param dnn see \code{link{base:table}}
##' @param deparse.level see \code{link{base:table}}
##' @return a table object
##' @seealso \code{\link[base]{table}}
##' @export 
table <- function (..., exclude, useNA, dnn, deparse.level) {
    m <- match.call()
    
    dots <- list(...)
    are.vars <- vapply(dots, is.variable, logical(1))
    if (length(are.vars) && all(are.vars)) {
        if (length(are.vars)>1) {
            halt("Cannot currently tabulate more than one Crunch variable")
        }
        if (!is.Categorical(dots[[1]])) {
            halt("Only CategoricalVariables currently supported for table()")
        }
        m[[1]] <- get("CategoricalVariable.table", asNamespace("rcrunch"))
        where <- parent.frame()
        return(eval(m, envir=where, enclos=asNamespace("rcrunch")))
    } else if (any(are.vars)) {
        halt("Cannot currently tabulate Crunch variables with ", 
            "non-Crunch vectors")
    } else {
        m[[1]] <- quote(base::table)
        return(eval.parent(m))
    }
}

#setGeneric("table", signature="...")
# ## @export 
#setMethod("table", "CategoricalVariable", CategoricalVariable.table)

##' Summary methods for Crunch Variables
##' 
##' @param object A Variable
##' @param ... additional arguments, ignored (they're in the summary.default)
##' signature
##' @return a summary response. Categorical variable summaries should work like
##' summary.factor; Numeric variables should be like summary.numeric. Other
##' Variable types are not yet supported.
##' @rdname crunch-summary
##' @export
summary.CategoricalVariable <- function (object, ...) {
    tab <- table(object)
    tab <- tab[order(tab, decreasing=TRUE)]
    class(tab) <- c("CategoricalVariableSummary", class(tab))
    attr(tab, "varname") <- getNameAndType(object)
    return(tab)
}

##' @export
print.CategoricalVariableSummary <- function (x, ...) {
    # class(x) <- class(x)[-1] ## uh, call next method
    # attr(x, "varname") <- NULL
    print(data.frame(Count=x))
}

##' @rdname crunch-summary
##' @export
summary.NumericVariable <- function (object, ...) {
    summ <- getSummary(object)
    fivenum <- sapply(summ$fivenum, function (x) x[[2]])
    out <- c(fivenum[1:3], summ$mean, fivenum[4:5])
    if (summ$missing_count) {
        out <- c(out, summ$missing_count)
    }
    names(out) <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.", "NA's")[1:length(out)]
    class(out) <- c("NumericVariableSummary", "summaryDefault", "table")
    attr(out, "varname") <- getNameAndType(object)
    return(out)
}

