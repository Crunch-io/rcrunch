setMethod("toVariable", "character", function (x) {
    return(list(values=x, type="text"))
})
setMethod("toVariable", "numeric", function (x) {
    return(list(values=x, type="numeric"))
})
setMethod("toVariable", "factor", function (x) {
    return(list(values=as.integer(x), type="categorical",
        categories=categoriesFromLevels(levels(x))))
})

categoriesFromLevels <- function (x) {
    return(lapply(seq_along(x), function (i) {
        list(id=i, name=x[i], numeric_value=i, missing=FALSE)
    }))
}