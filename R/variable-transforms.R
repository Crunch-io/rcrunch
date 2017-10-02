#' @rdname Transforms
#' @export
setMethod("transforms", "CrunchVariable", function (x) {
    var_entity <- entity(x)
    trans <- var_entity@body$view$transform

    if (is.null(trans)) {
        return(NULL)
    }

    trans_out <- Transforms(insertions = Insertions(data=trans$insertions),
                            categories = NULL,
                            elements = NULL)
    return(trans_out)
})

#' @rdname Transforms
#' @export
setMethod("transforms<-", "CrunchVariable", function (x, value) {
    frmt <- wrapEntity("view" = list("transform" = value))
    crPATCH(self(x), body=toJSON(frmt))
    invisible(x)
})

setValidity("Transforms", function (object) {
    one_of_names <- c("insertions", "categories", "elements") %in% names(object)
    if (!any(one_of_names)) {
        val <- paste("Transforms must have at least one of", serialPaste(dQuote(c("insertions", "categories", "elements")), "or"))
    } else {
        val <- TRUE
    }

    if (!is.null(object[["insertions"]]) && !is.insertions(object[["insertions"]])) {
        val <- "Transforms insertions element must be of class Insertions"
    }
    if (!is.null(object[["categories"]])) {
        val <- "Transforms categories element must be NULL"
    }
    if (!is.null(object[["elements"]])) {
        val <- "Transforms elements element must be NULL"
    }

    return(val)
})

# transforms can be:
# * categories (changing specific attributes about categories) (should just take a categories?)
# * insertions (can be any function? `"function": { "combine": []}`)
# * elements (metadata about subreferences in MR/arrays)
