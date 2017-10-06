#' Show the variable transformations on a CrunchCube
#'
#' @param x a CrunchCube
#'
#' @return a CrunchCube, with any transformations applied.
#' @aliases showTransforms
#'
#' @export
setMethod("showTransforms", "CrunchCube", function (x) {
    browser()
    #measure by measure?
})

showTransform <- function(x) {
    trans <- transforms(x)
    combos <- trans$insertions
    var_cats <- categories(entity(variables(x)[[1]]))

    adds <- vapply(combos, function (cmb) {
        which.cats <- names(var_cats[ids(var_cats) %in% unlist(combinations(cmb))])
        return(sum(x@arrays$count[which.cats]))
    }, double(1), USE.NAMES = TRUE)
    names(adds) <- names(combos)

    # shuffle names around
    dn <- dimnames(x@arrays$count)
    dn[[1]] <- c(names(x@arrays$count), names(adds))
    x@arrays$count <- array(c(x@arrays$count, adds), dimnames = dn)
    return(x)
}

getTransforms <- function (x) {
    var_entity <- entity(x)
    trans <- var_entity@body$view$transform

    if (is.null(trans)) {
        return(NULL)
    }

    trans_out <- Transforms(insertions = Insertions(data=trans$insertions),
                            categories = NULL,
                            elements = NULL)
    return(trans_out)
}

#' @rdname Transforms
#' @export
setMethod("transforms", "CrunchVariable", getTransforms)
#' @rdname Transforms
#' @export
setMethod("transforms", "VariableTuple", getTransforms)
#' @rdname Transforms
#' @export
setMethod("transforms", "CrunchCube", function (x) {
    ## hack since variables(CrunchCube) is not subsettable
    ref_vars <- variables(x)@index

    trans <- vapply(ref_vars, function (v) v$view$transform)

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
