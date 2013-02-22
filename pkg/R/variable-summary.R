getSummary <- function (x) {
    url <- x@urls$summary_url
    if (is.null(url)) {
        stop("No summary available", call.=FALSE)
    } else if (is.character(url)) {
        ## Flexibilized so that object can be injected for tests
        url <- GET(url)
    }
    if (!is.shoji(url)) {
        stop("Error in retrieving summary", call.=FALSE)
    }
    return(url)
}

makeCategoricalTable <- function (categories, summaries) {
    id_key <- "_id"
    category_ids <- selectFrom(id_key, categories)
    summary_ids <- selectFrom(id_key, summaries)
    values <- selectFrom("count", summaries)[match(summary_ids, category_ids)]
    names(values) <- selectFrom("name", categories)
    class(values) <- "table"
    return(values)
}

CategoricalVariable.table <- function (..., 
                    exclude = if (useNA == "no") c(NA, NaN), 
                    useNA = c("no", "ifany", "always"), dnn = list.names(...),
                    deparse.level = 1) {
    var <- ..1
    summ <- getSummary(var)$body
    out <- makeCategoricalTable(categories(var), summ$categories)
    add.na <- useNA=="always" || (useNA=="ifany" && summ$missing_count>0)
    if (add.na) {
        cl <- class(out)
        out <- c(out, `<NA>`=summ$missing_count)
        class(out) <- cl
    }
    return(out)
}

basetable <- base::table

##' @export 
table <- function (..., exclude, useNA, dnn, deparse.level) {
	m <- match.call()
	
	if (is.variable(..1)) {
        FUN <- "CategoricalVariable.table"
    } else {
        FUN <- "basetable"
    }
    m[[1]] <- as.name(FUN)
	eval(m, parent.frame())
}

CategoricalVariable.summary <- function (object, ...) {
    summ <- getSummary(object)
    tab <- makeCategoricalTable(categories(object), summ$body$categories)
    tab <- tab[order(tab, decreasing=TRUE)]
    class(tab) <- c("CategoricalVariableSummary", class(tab))
    attr(tab, "varname") <- getNameAndType(object)
    return(tab)
}