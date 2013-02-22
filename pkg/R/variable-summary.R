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

CategoricalVariable.table <- function (...) {
    var <- ..1
    summary <- getSummary(var)
    return(makeCategoricalTable(categories(var), summary$body$categories))
}

##' @export
setGeneric("table", function (...) standardGeneric("table"))
##' @export
setMethod("table", "CategoricalVariable", CategoricalVariable.table)