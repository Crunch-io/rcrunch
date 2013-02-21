getSummary <- function (x) {
    url <- x@urls$summary_url
    if (is.null(url)) stop("No summary available", call.=FALSE)
    return(GET(url))
}

makeCategoricalTable <- function (categories, summaries) {
    id_key <- "_id"
    category_ids <- selectFrom(id_key, categories)
    summary_ids <- selectFrom(id_key, summaries)
    values <- selectFrom("count", summaries)[match(summary_ids, category_ids)]
    names(values) <- selectFrom("name", categories)
    class(values) <- "table"
    return(values)
    # return(structure(values, 
    #     #.Dim = length(values), 
    #     .Dimnames = structure(list(selectFrom("name", categories)), 
    #         .Names = ""), 
    #     class = "table"))
}