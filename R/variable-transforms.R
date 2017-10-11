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

#' Show the variable transformations on a Categorical variable
#'
#' @param x a Categorical variable
#'
#' @return summary of the variable, with transforms applied
#'
#' @aliases showTransforms
#' @export
setMethod("showTransforms", "CategoricalVariable", function (x) {
    tab <- calcTransform(table(x), transforms(x), categories(x))
    # tab <- tab[order(tab, decreasing=TRUE)]
    attr(tab, "varname") <- getNameAndType(x)

    return(tab)
})

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
calcTransform <- function (ary, trans, var_cats) {
    if (length(dim(ary)) > 1) {
        halt("Calculating varaible transforms is not implemented for dimensions ",
             "greater than 1.")
    }

    inserts <- trans$insertions
    adds <- vapply(inserts, function (insert) {
        # if there is no function, return NA
        if (is.null(insert[["function"]])) {
            return(NA)
        }

        # check if there are other functions
        not_subtotal <- insert[["function"]] != "subtotal"
        if (any(not_subtotal)) {
            warning("Transform functions other than subtotal are not supported.",
                    " Applying only subtotals and ignoring ", insert[["function"]])
            return(NA)
        }

        combos <- unlist(subtotals(insert))
        which.cats <- names(var_cats[ids(var_cats) %in% combos])
        return(sum(ary[which.cats]))
    }, double(1), USE.NAMES = TRUE)
    names(adds) <- names(inserts)

    # shuffle names around
    dn <- dimnames(ary)
    dn[[1]] <- c(names(ary), names(adds))
    ary <- array(c(ary, adds), dimnames = dn)

    # reorder respecting anchors and missingness
    cats_collated <- collateCats(trans, var_cats)
    ary <- ary[names(cats_collated[!is.na(cats_collated)])]

    return(ary)
}

collateCats <- function (trans, var_cats) {
    # setup abstract categories to collate into
    cats_out <- AbsCats(data=lapply(var_cats, as, "AbsCat"))
    inserts <- trans$insertions
    for (insert in inserts) {
        pos <- findInsertPosition(insert, cats_out)
        cats_out <- AbsCats(data = append(cats_out, list(as(insert, "AbsCat")), pos))
    }
    return(cats_out)
}

findInsertPosition <- function (insert, cats) {
    anchr <- anchor(insert)
    if (anchr == 0) {
        return(0)
    }
    # if the anchor is the id of a non-missing category place after
    if (anchr %in% ids(cats)) {
        which_cat <- which(anchr == ids(cats))
        if (!is.na(cats[[which_cat]])) {
            return(which_cat)
        }
    }

    # all other situations, put at the end
    return(Inf)
}

