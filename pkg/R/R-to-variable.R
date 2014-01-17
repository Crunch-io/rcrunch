setMethod("toVariable", "character", function (x) {
    return(list(values=x, type="text"))
})
setMethod("toVariable", "numeric", function (x) {
    return(list(values=x, type="numeric"))
})