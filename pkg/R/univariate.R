##' @export
setMethod("mean", "CrunchVariable", function (x, ...) {
    stop(sQuote('mean'), " is not defined for ", class(x), call.=FALSE)
})
##' @export
setMethod("mean", "NumericVariable", function (x, ...) {
    kwargs <- list(...)
    
    summ <- getSummary(x)
    m <- summ[['mean']]
    na.rm <- isTRUE(kwargs[['na.rm']])
    if (!na.rm && summ[['missing_count']] > 0) {
        m <- NA_real_
    }
    return(m)
})

##' @export
setMethod("sd", "CrunchVariable", function (x, na.rm) {
    stop(sQuote('sd'), " is not defined for ", class(x), call.=FALSE)
})
##' @export
setMethod("sd", "NumericVariable", function (x, na.rm=FALSE) {    
    summ <- getSummary(x)
    m <- summ[['stddev']]
    if (!na.rm && summ[['missing_count']] > 0) {
        m <- NA_real_
    }
    return(m)
})

##' @export
setMethod("median", "CrunchVariable", function (x, na.rm) {
    stop(sQuote('median'), " is not defined for ", class(x), call.=FALSE)
})
##' @export
setMethod("median", "NumericVariable", function (x, na.rm=FALSE) {    
    summ <- getSummary(x)
    m <- summ[['median']]
    if (!na.rm && summ[['missing_count']] > 0) {
        m <- NA_real_
    }
    return(m)
})