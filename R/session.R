setMethod("[[", "Session", function (x, i, ..., drop=FALSE) {
    if (i %in% c("datasets", "projects")) {
        return(do.call(i, list()))
    } else {
        halt("Unknown session attribute: ", i)
    }
})

setMethod("$", "Session", function (x, name) x[[name]])

setMethod("[[<-", "Session", function (x, i, value) {
    if (i %in% c("datasets", "projects")) {
        return(x)
    } else {
        halt("Unknown session attribute: ", i)
    }
})

setMethod("$<-", "Session", function (x, name, value) session())
