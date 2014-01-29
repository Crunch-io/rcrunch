setMethod("toVariable", "character", function (x, ...) {
    return(list(values=x, type="text"))
})
setMethod("toVariable", "numeric", function (x, ...) {
    return(list(values=x, type="numeric"))
})
setMethod("toVariable", "factor", function (x, ...) {
    return(list(values=as.integer(x), type="categorical",
        categories=categoriesFromLevels(levels(x))))
})
setMethod("toVariable", "Date", function (x, ...) {
    return(list(values=as.character(x), type="datetime"))
})
# setMethod("toVariable", "POSIXt", function (x) {
#     return(list(values=as.character(x), type="datetime"))
# })
setMethod("toVariable", "logical", function (x, ...) {
    ## Make it categorical
    return(list(values=2L-as.integer(x), type="categorical", 
        categories=categoriesFromLevels(c("True", "False"))))
})

categoriesFromLevels <- function (x) {
    return(lapply(seq_along(x), function (i) {
        list(id=i, name=x[i], numeric_value=i, missing=FALSE)
    }))
}