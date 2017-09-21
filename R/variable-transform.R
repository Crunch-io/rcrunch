#' @rdname describe
#' @export
setMethod("transforms", "CrunchVariable", function (x) {
    var_entity <- entity(x)
    return(var_entity@body$view$transforms)
})
#' @rdname describe
#' @export
setMethod("transforms<-", "NumericVariable", function (x, value) {
    if (!is.numeric(value) || !is.whole(value)) {
        halt("digit specifications should be an integer")
    }
    if (value < 0 | value > 16) {
        halt("digit specifications should be between 0 and 16")
    }

    frmt <- wrapEntity("format" = list("data" = list("digits" = value)))
    crPATCH(self(x), body=toJSON(frmt))
    invisible(x)
})

# transforms can be:
# * elements (metadata about subreferences in MR/arrays)
# * categories (changing specific attributes about categories) (should just take a categories?)
# * insertions (can be any function? `"function": { "combine": []}`)