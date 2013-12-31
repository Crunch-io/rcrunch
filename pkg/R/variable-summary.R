getSummary <- function (x) {
    url <- x@urls$summary_url
    if (is.null(url)) {
        stop("No summary available", call.=FALSE)
    }
    url <- GET(url)
    ## Summaries don't return as shoji entities
    # if (!is.shoji(url)) {
    #     stop("Error in retrieving summary", call.=FALSE)
    # }
    return(url)
}

getFrequencies <- function (x) {
    url <- x@urls$frequencies_url
    if (is.null(url)) {
        stop("No frequencies available", call.=FALSE)
    }
    url <- GET(url)
    return(url)
}

makeCategoricalTable <- function (frequencies) {
    frequencies <- .na.omit.categories(frequencies)
    values <- structure(selectFrom("count", frequencies),
        .Names=selectFrom("name", frequencies),
        class="table")
    return(values)
}

CategoricalVariable.table <- function (..., 
                    exclude = if (useNA == "no") c(NA, NaN), 
                    useNA = c("no", "ifany", "always"), dnn = list.names(...),
                    deparse.level = 1) {
    var <- ..1
    summ <- getSummary(var)
    out <- makeCategoricalTable(summ$categories)
    useNA <- match.arg(useNA)
    add.na <- useNA=="always" || (useNA=="ifany" && summ$missing_count>0)
    if (add.na) {
        cl <- class(out)
        out <- c(out, `<NA>`=summ$missing_count)
        class(out) <- cl
    }
    ## Figure this out:
    # n <- names(out)
    # names(out) <- NULL
    # out <- array(as.integer(out), length(out), dimnames=list(n))
    # class(out) <- "table"
    return(out)
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

##' s3method summary CategoricalVariable
summary.CategoricalVariable <- function (object, ...) {
    tab <- table(object)
    tab <- tab[order(tab, decreasing=TRUE)]
    class(tab) <- c("CategoricalVariableSummary", class(tab))
    attr(tab, "varname") <- getNameAndType(object)
    return(tab)
}

##' s3method print CategoricalVariableSummary
print.CategoricalVariableSummary <- function (x, ...) {
    # class(x) <- class(x)[-1] ## uh, call next method
    # attr(x, "varname") <- NULL
    print(data.frame(Count=x))
}

##' s3method summary NumericVariable
summary.NumericVariable <- function (object, ...) {
    summ <- getSummary(object)
    fivenum <- sapply(summ$fivenum, function (x) x[[2]])
    out <- c(fivenum[1:3], summ$mean, fivenum[4:5])
    if (summ$missing_count) {
        out <- c(out, summ$missing_count)
    }
    names(out) <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.", "NA's")[1:length(out)]
    class(out) <- c("NumericVariableSummary", "table")
    attr(out, "varname") <- getNameAndType(object)
    return(out)
}

##' s3method print NumericVariableSummary
print.NumericVariableSummary <- function (x, ...) {
    
    class(x) <- class(x)[-1] ## uh, call next method
    attr(x, "varname") <- NULL
    print(str(x))
    print(as.data.frame(x))
}

