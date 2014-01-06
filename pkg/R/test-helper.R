## For tests only
setGeneric("mockValues", function (x, n, ...) standardGeneric("mockValues"))
setMethod("mockValues", "NumericVariable", function (x, n, ...) rnorm(n, ...))
setMethod("mockValues", "TextVariable", function (x, n, ...) {
    sample(letters, n, replace=TRUE, ...)
})
setMethod("mockValues", "CategoricalVariable", function (x, n, ...) {
    sample(names(categories(x)), n, replace=TRUE, ...)
})
setMethod("mockValues", "DatetimeVariable", function (x, n, ...) {
    as.character(sample(as.Date(1:100, origin="1955-11-05"), n, replace=TRUE, ...))
})
setMethod("mockValues", "CrunchVariable", function (x, n, ...) {
    sample(letters, n, replace=TRUE, ...)
})