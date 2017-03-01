#' Univariate statistics on Crunch objects
#'
#' @param x a NumericVariable, or for \code{min} and \code{max}, possibly a
#' DatetimeVariable
#' @param ... additional arguments to \code{mean}
#' @param na.rm logical: exclude missings?
#' @seealso \code{\link[base]{mean}} \code{\link[stats]{sd}} \code{\link[stats]{median}} \code{\link[base]{min}} \code{\link[base]{max}}
#' @name crunch-uni
#' @aliases mean sd median min max
NULL

.summary.stat <- function (x, stat, na.rm=FALSE, ...) {
    ## Get a single stat from the summary object
    # summ <- getSummary(x)
    # m <- summ[[stat]]
    # if (!na.rm && summ[['missing_count']] > 0) {
    #     m <- NA_real_
    # }
    # return(m)

    query <- list(
        query=toJSON(list(dimensions=list(),
            measures=list(q=registerCubeFunctions()[[stat]](x)))),
        filter=toJSON(zcl(activeFilter(x)))
    )
    cube <- CrunchCube(crGET(cubeURL(x), query=query))
    if (!na.rm && cube$result$measures$q[['n_missing']] > 0) {
        return(NA_real_)
    } else {
        return(as.array(cube))
    }
}


#' @rdname crunch-uni
#' @export
setMethod("mean", "CrunchVariable", function (x, ...) {
    halt(dQuote("mean"), " is not defined for ", class(x))
})
#' @rdname crunch-uni
#' @export
setMethod("mean", "NumericVariable",
    function (x, ...) .summary.stat(x, "mean", ...))


#' @rdname crunch-uni
#' @export
setMethod("sd", "CrunchVariable", function (x, na.rm) {
    halt(dQuote('sd'), " is not defined for ", class(x))
})
#' @rdname crunch-uni
#' @export
setMethod("sd", "NumericVariable",
    function (x, na.rm=FALSE) .summary.stat(x, "sd", na.rm=na.rm))


## Future-proofing for change to median signature in R >= 3.4
is.R.3.4 <- "..." %in% names(formals(median))

#' @rdname crunch-uni
#' @export
setMethod("median", "CrunchVariable", ifelse(is.R.3.4,
    function (x, na.rm, ...) {
        halt(dQuote('median'), " is not defined for ", class(x))
    }, function (x, na.rm) {
        halt(dQuote('median'), " is not defined for ", class(x))
    }))

#' @rdname crunch-uni
#' @export
setMethod("median", "NumericVariable", ifelse(is.R.3.4,
    function (x, na.rm=FALSE, ...) .summary.stat(x, "median", na.rm=na.rm),
    function (x, na.rm=FALSE) .summary.stat(x, "median", na.rm=na.rm)))

## Can't do datetime apparently:
# (400) Bad Request: The 'cube_quantile' function requires argument 0 be of
# type {'class': 'numeric'}. (for variable q)
# setMethod("median", "DatetimeVariable",
#     function (x, na.rm=FALSE) .summary.stat(x, "median", na.rm=na.rm))

#' @rdname crunch-uni
#' @export
setMethod("min", "CrunchVariable", function (x, na.rm) {
    halt(dQuote('min'), " is not defined for ", class(x))
})
#' @rdname crunch-uni
#' @export
setMethod("min", "NumericVariable",
    function (x, na.rm=FALSE) .summary.stat(x, "min", na.rm=na.rm))
#' @rdname crunch-uni
#' @export
setMethod("min", "DatetimeVariable",
    function (x, na.rm=FALSE) from8601(.summary.stat(x, "min", na.rm=na.rm)))

#' @rdname crunch-uni
#' @export
setMethod("max", "CrunchVariable", function (x, na.rm) {
    halt(dQuote('max'), " is not defined for ", class(x))
})
#' @rdname crunch-uni
#' @export
setMethod("max", "NumericVariable",
    function (x, na.rm=FALSE) .summary.stat(x, "max", na.rm=na.rm))
#' @rdname crunch-uni
#' @export
setMethod("max", "DatetimeVariable",
    function (x, na.rm=FALSE) from8601(.summary.stat(x, "max", na.rm=na.rm)))
