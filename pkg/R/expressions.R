math.exp <- function (e1, e2, operator) {
    ex <- structure(list(operator, list(zcl(e1), zcl(e2))), .Names=c("function", "args"))
    ds.url <- unique(unlist(lapply(list(e1, e2), datasetReference))) %||% ""
    CrunchExpression(expression=ex, dataset_url=ds.url)
}

mec <- function (e1, e2, i) {
    force(i)
    return(function (e1, e2) math.exp(e1, e2, i))
}

for (i in c("+", "-", "*", "/", "<", ">", "==")) {
    setMethod(i, c("CrunchVariable", "numeric"), mec(e1, e2, i))
    setMethod(i, c("numeric", "CrunchVariable"), mec(e1, e2, i))
}

setMethod("zcl", "CrunchExpression", function (x) x@expression)
setMethod("zcl", "CrunchVariable", function (x) list(variable=tuple(x)$id))
setMethod("zcl", "numeric", function (x) {
    if (length(x) == 1) {
        return(list(value=x))
    } else {
        return(list(column=x))
    }
})

##' @export 
##' @S3method as.vector CrunchExpression
as.vector.CrunchExpression <- function (x, mode) {
    payload <- toJSON(list(command="select", variables=list(out=zcl(x))))
    cat(payload)
    out <- POST(paste0(x@dataset_url, "table/"), body=payload)
    return(columnParser(out$metadata$out$type)(out$data$out))
}