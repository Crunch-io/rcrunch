setMethod("refresh", "IndexTuple", function (x) {
    index.list <- GET(x@index_url)$index
    x@body <- index.list[[x@entity_url]]
    return(x)
})