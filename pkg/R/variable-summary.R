getSummary <- function (x) {
    url <- x@urls$summary_url
    if (is.null(url)) {
        stop("No summary available", call.=FALSE)
    }
    out <- GET(url)
    ## Summaries don't return as shoji entities
    # if (!is.shoji(url)) {
    #     stop("Error in retrieving summary", call.=FALSE)
    # }
    if (is.Datetime(x)) {
        toR <- columnParser("datetime")
        for (i in c("min", "max")) out[[i]] <- toR(out[[i]])
    }
    return(out)
}

getFrequencies <- function (x) {
    url <- x@urls$frequencies_url
    if (is.null(url)) {
        stop("No frequencies available", call.=FALSE)
    }
    url <- GET(url)
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

##' @export 
table <- function (..., exclude, useNA, dnn, deparse.level) {
    m <- match.call()
    
    dots <- list(...)
    are.vars <- vapply(dots, is.variable, logical(1))
    if (length(are.vars) && all(are.vars)) {
        if (length(are.vars)>1) {
            stop("Cannot currently tabulate more than one Crunch variable", 
                call.=FALSE)
        }
        FUN <- quote(rcrunch:::CategoricalVariable.table)
    } else if (any(are.vars)) {
        stop("Cannot currently tabulate Crunch variables with ", 
            "non-Crunch vectors", call.=FALSE)
    } else {
        FUN <- quote(base::table)
    }
    m[[1]] <- FUN
    eval(m, parent.frame())
}

#setGeneric("table", signature="...")
# ##' @export 
#setMethod("table", "CategoricalVariable", CategoricalVariable.table)

##' @S3method summary CategoricalVariable
summary.CategoricalVariable <- function (object, ...) {
    tab <- table(object)
    tab <- tab[order(tab, decreasing=TRUE)]
    class(tab) <- c("CategoricalVariableSummary", class(tab))
    attr(tab, "varname") <- getNameAndType(object)
    return(tab)
}

##' @S3method print CategoricalVariableSummary
print.CategoricalVariableSummary <- function (x, ...) {
    # class(x) <- class(x)[-1] ## uh, call next method
    # attr(x, "varname") <- NULL
    print(data.frame(Count=x))
}

##' @S3method summary NumericVariable
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

