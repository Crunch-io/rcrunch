.summary.stat <- function (x, stat, na.rm=FALSE, ...) {
    summ <- getSummary(x)
    m <- summ[[stat]]
    if (!na.rm && summ[['missing_count']] > 0) {
        m <- NA_real_
    }
    return(m)
}

## Backstop methods

##' @export
setMethod("mean", "CrunchVariable", function (x, ...) {
    stop(sQuote("mean"), " is not defined for ", class(x), call.=FALSE)
})
##' @export
setMethod("sd", "CrunchVariable", function (x, na.rm) {
    stop(sQuote('sd'), " is not defined for ", class(x), call.=FALSE)
})
##' @export
setMethod("median", "CrunchVariable", function (x, na.rm) {
    stop(sQuote('median'), " is not defined for ", class(x), call.=FALSE)
})
##' @export
setMethod("min", "CrunchVariable", function (x, na.rm) {
    stop(sQuote('min'), " is not defined for ", class(x), call.=FALSE)
})
##' @export
setMethod("max", "CrunchVariable", function (x, na.rm) {
    stop(sQuote('max'), " is not defined for ", class(x), call.=FALSE)
})


## Actual methods

setMethod("mean", "NumericVariable", 
    function (x, ...) .summary.stat(x, "mean", ...))

for (i in c("median", "sd", "min", "max")) {
    m <- ifelse(i == "sd", "stddev", i)
    setMethod(i, "NumericVariable", 
        function (x, na.rm=FALSE) .summary.stat(x, m, na.rm=na.rm))
    if (i %in% c("min", "max")) {
        setMethod(i, "DatetimeVariable", 
            function (x, na.rm=FALSE) .summary.stat(x, m, na.rm=na.rm))
    }
}
