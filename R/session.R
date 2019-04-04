#' Extract catalogs from a Session object
#'
#' @param x a Session object
#' @param i which catalog to load. Supported values are "datasets" and "projects"
#' @param name for `$`, the same as `i` for `[`
#' @param drop Invalid
#' @param ... additional arguments, ignored.
#' @param value For updating, an object of the appropriate class and size to
#' insert. In practice `value` is ignored; Session objects hold no state
#' and it is assumed that any state modification on the server happens in other
#' methods.
#' @return The requested catalog when extracting; a Session object if assigning.
#' @seealso `session`
#' @keywords internal
#' @name session-extract
NULL

#' @rdname session-extract
#' @export
setMethod("[[", "Session", function(x, i, ..., drop = FALSE) {
    if (i %in% c("datasets", "projects")) {
        return(do.call(i, list()))
    } else {
        halt("Unknown session attribute: ", i)
    }
})

#' @rdname session-extract
#' @export
setMethod("$", "Session", function(x, name) x[[name]])

#' @rdname session-extract
#' @export
setMethod("[[<-", "Session", function(x, i, value) {
    if (i %in% c("datasets", "projects")) {
        return(x)
    } else {
        halt("Unknown session attribute: ", i)
    }
})

#' @rdname session-extract
#' @export
setMethod("$<-", "Session", function(x, name, value) session())
