#' Given an array and transforms, calculate the transformations.
#'
#' @param ary an array with all dimensions
#' @param trans a Transforms object to pull transformations from
#' @param var_cats the Categories object of the transform
#'
#' @return the array given in `ary`, with any transformations specified in
#' `trans` calculated
#'
#' @export
calcTransform <- function(ary, trans, var_cats) {
    if (length(dim(ary)) > 1) {
        halt("Calculating varaible transforms is not implemented for dimensions ",
             "greater than 1.")
    }

    combos <- trans$insertions
    adds <- vapply(combos, function (cmb) {
        which.cats <- names(var_cats[ids(var_cats) %in% unlist(combinations(cmb))])
        return(sum(ary[which.cats]))
    }, double(1), USE.NAMES = TRUE)
    names(adds) <- names(combos)

    # shuffle names around
    dn <- dimnames(ary)
    dn[[1]] <- c(names(ary), names(adds))
    ary <- array(c(ary, adds), dimnames = dn)
    return(ary)
}

#' Show the variable transformations on a Categorical variable
#'
#' @param x a Categorical variable
#'
#' @return summary of the variable, with transforms applied
#'
#' @export
setMethod("showTransforms", "CategoricalVariable", function (x) {
    tab <- calcTransform(table(x), transforms(x), categories(x))
    # tab <- tab[order(tab, decreasing=TRUE)]
    class(tab) <- c("CategoricalVariableSummary", class(tab))
    attr(tab, "varname") <- getNameAndType(x)

    return(tab)
})

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
