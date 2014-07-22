math.exp <- function (e1, e2, operator) {
    ex <- zfunc(operator, e1, e2)
    ds.url <- unique(unlist(lapply(list(e1, e2), datasetReference))) %||% ""
    CrunchExpression(expression=ex, dataset_url=ds.url)
}

vxr <- function (i) {
    force(i)
    return(function (e1, e2) math.exp(e1, typeof(e2, e1), i))
}

rxv <- function (i) {
    force(i)
    return(function (e1, e2) math.exp(typeof(e1, e2), e2, i))
}

vxv <- function (i) {
    force(i)
    return(function (e1, e2) math.exp(e1, e2, i))
}

for (i in c("+", "-", "*", "/")) {
    setMethod(i, c("NumericVariable", "numeric"), vxr(i))
    setMethod(i, c("numeric", "NumericVariable"), rxv(i))
    setMethod(i, c("CrunchExpression", "numeric"), vxv(i)) # no typeof?
    setMethod(i, c("numeric", "CrunchExpression"), vxv(i)) # no typeof?
    setMethod(i, c("CrunchVariable", "CrunchVariable"), vxv(i))
    setMethod(i, c("CrunchExpression", "CrunchVariable"), vxv(i))
    setMethod(i, c("CrunchVariable", "CrunchExpression"), vxv(i))
}

for (i in c("<", ">", ">=", "<=")) {
    setMethod(i, c("NumericVariable", "numeric"), vxr(i))
    setMethod(i, c("numeric", "NumericVariable"), rxv(i))
    setMethod(i, c("CrunchExpression", "numeric"), vxv(i)) # no typeof?
    setMethod(i, c("numeric", "CrunchExpression"), vxv(i)) # no typeof?
    setMethod(i, c("CrunchVariable", "CrunchVariable"), vxv(i))
    setMethod(i, c("CrunchExpression", "CrunchVariable"), vxv(i))
    setMethod(i, c("CrunchVariable", "CrunchExpression"), vxv(i))
}

for (i in c("==", "!=")) {
    setMethod(i, c("CrunchVariable", "numeric"), vxr(i))
    setMethod(i, c("numeric", "CrunchVariable"), rxv(i))
    setMethod(i, c("CrunchExpression", "numeric"), vxv(i)) # no typeof?
    setMethod(i, c("numeric", "CrunchExpression"), vxv(i)) # no typeof?
    setMethod(i, c("CrunchVariable", "CrunchVariable"), vxv(i))
    setMethod(i, c("CrunchExpression", "CrunchVariable"), vxv(i))
    setMethod(i, c("CrunchVariable", "CrunchExpression"), vxv(i))
}

.inCrunch <- function (x, table) math.exp(x, typeof(table, x), "contains")
setMethod("%in%", c("TextVariable", "character"), .inCrunch)
setMethod("%in%", c("NumericVariable", "numeric"), .inCrunch)


setMethod("datasetReference", "CrunchExpression", function (x) x@dataset_url)

##' @export 
##' @S3method as.vector CrunchExpression
as.vector.CrunchExpression <- function (x, mode) {
    payload <- list(command="select", variables=list(out=zcl(x)))
    if (length(x@filter)) {
        payload[["filter"]] <- x@filter
    }
    out <- POST(paste0(x@dataset_url, "table/"), body=toJSON(payload))
    return(columnParser(out$metadata$out$type)(out$data$out))
}

setMethod("is.na", "CrunchVariable", function (x) {
    CrunchExpression(expression=zfunc("is_missing", x),
        dataset_url=datasetReference(x))
})
