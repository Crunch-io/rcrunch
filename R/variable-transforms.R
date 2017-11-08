getTransforms <- function (x) {
    var_entity <- entity(x)
    trans <- var_entity@body$view$transform

    if (is.null(trans) || length(trans) == 0) {
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
setMethod("transforms<-", c("CrunchVariable", "ANY"), function (x, value) {
    frmt <- wrapEntity("view" = list("transform" = value))
    crPATCH(self(x), body=toJSON(frmt))
    dropCache(cubeURL(x))
    invisible(x)
})

#' @rdname Transforms
#' @export
setMethod("transforms<-", c("CrunchVariable", "NULL"), function (x, value) {
    frmt <- wrapEntity("view" = list("transform" = emptyObject()))
    crPATCH(self(x), body=toJSON(frmt))
    dropCache(cubeURL(x))
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
#'
#' @examples
#' \dontrun{
#' showTransforms(ds$variable)
#' }
#'
#' @export
setMethod("showTransforms", "CategoricalVariable", function (x) {
    tab <- table(x)
    tab <- as.array(calcTransforms(tab, transforms(x), categories(x)))

    remove_these <- names(categories(x)[is.na(categories(x))])
    tab <- tab[!(names(tab) %in% remove_these)]
    styles <- transformStyles(transforms(x), categories(x)[!is.na(categories(x))])

    out <- prettyPrint2d(tab, row_styles = styles)
    cat(unlist(out), sep="\n")
    return(invisible(tab))
})


#' Test if an abstract category object is of a conceptual type
#'
#' Convenience functions to test if an abstract category (AbsCat) object is a
#' specific type. These types are defined by the properties of the abstract
#' category object.
#'
#' `is.abscat.subtotal` is `x` a subtotal insertion?
#' `is.abscat.heading` is `x` a heading insertion?
#' `is.abscat.category` is `x` a category?
#'
#' @param x an AbsCat object
#'
#' @return logical if the AbsCat object is
#'
#' @name AbsCat-type-tests
#' @keywords internal
NULL

#' @rdname AbsCat-type-tests
#' @export
is.abscat.subtotal <- function (x) {
    if (is.abstract.categories(x)) {
        return(!(is.na(funcs(x))) & funcs(x) == 'subtotal')
    }

    return(!(is.na(func(x))) & func(x) == 'subtotal')
}

#' @rdname AbsCat-type-tests
#' @export
is.abscat.heading <- function (x) {
    if (is.abstract.categories(x)) {
        return(is.na(funcs(x)) & !is.na(anchors(x)))
    }

    return(is.na(func(x)) & !is.na(anchor(x)))
}

#' @rdname AbsCat-type-tests
#' @export
is.abscat.category <- function (x) {
    # if the class has already been specified, use that.
    if (!is.null(x$class)) {
        return(x$class == "Category")
    }

    # Otherwise, check if the properties of x look like a category, since
    # value is sometimes legitamately NA, just check id, missing, and name
    if (is.abstract.categories(x)) {
        all <- !is.na(ids(x)) & !is.na(is.na(x)) & !is.na(names(x))
    }  else {
        all <- !is.na(id(x)) & !is.na(is.na(x)) & !is.na(name(x))
    }

    return(all)
}


# make styles based on transforms and categories
transformStyles <- function (trans, cats) {
    all_labs <- collateCats(trans$insertions, cats)
    styles <- lapply(all_labs, function (lab) {
        if (is.abscat.subtotal(lab)) {
            return(subtotalStyle)
        } else if (is.abscat.heading(lab)) {
            return(headingStyle)
        } else {
            return(NULL)
        }
    })
    return(styles)
}

#' @importFrom crayon make_style italic underline
headingStyle <- c(nonas, make_style("#546499"), underline) # blue with underline
subtotalStyle <- c(italic, make_style("#005e46"))

#' Given values from an array and transforms, calculate the insertions
#'
#' @param vec values to transform (typically a single dimension of an array)
#' @param inserst an `Insertions` object to pull transformations from
#' @param var_cats the `Categories` object of the transform
#'
#' @return the values given in `vec`, with any insertions specified in
#' `trans` calculated
#' @keywords internal
calcInsertions <- function (vec, inserts, var_cats) {
    if (length(dim(vec)) > 1) {
        halt("Calculating varaible transforms is not implemented for dimensions ",
             "greater than 1.")
    }

    vec_out <- vapply(inserts, function (insert) {
        # if insert is a category, return value
        if (is.abscat.category(insert)) {
            return(vec[name(insert)])
        }

        # if insert is a heading return NA
        if (is.abscat.heading(insert)) {
            return(NA)
        }

        # if insert is a subtotal, sum the things
        if (is.abscat.subtotal(insert)) {
            # grab category combinations, and then sum those categories.
            combos <- unlist(args(insert))
            which.cats <- names(var_cats[ids(var_cats) %in% combos])
            return(sum(vec[which.cats]))
        }

        # finally, check if there are other functions, warn and return NA
        not_subtotal <- insert[["function"]] != "subtotal"
        if (not_subtotal) {
            warning("Transform functions other than subtotal are not supported.",
                    " Applying only subtotals and ignoring ", insert[["function"]])
        }
        return(NA)
    }, double(1), USE.NAMES = TRUE)
    names(vec_out) <- names(inserts)

    return(vec_out)
}

calcTransforms <- function (ary, trans, var_cats,
                            include = c("subtotals", "headings", "cube_cells", "other_insertions")) {
    # TODO: other possible Transforms

    ### Insertions
    # collate insertions with categories for rearranging and calculation purposes
    cat_insert_map <- mapInsertions(trans$insertions, var_cats, include = include)

    # calculate the insertions based on cat_insert_map
    ary <- calcInsertions(ary, cat_insert_map, var_cats)

    return(ary)
}

# make a map of insertions and categories to be calculated
mapInsertions <- function (inserts, var_cats, include) {
    new_inserts <- list()
    # add subtotals if they are found in include
    if ("subtotals" %in% include) {
        subtots <- inserts[is.abscat.subtotal(inserts)]
        new_inserts <- c(new_inserts, subtots)
    }

    # add subtotals if they are in include
    if ("headings" %in% include) {
        new_inserts <- c(new_inserts, inserts[is.abscat.heading(inserts)])
    }

    # add non-subtotal insertions we must check that inserts are not subtotals
    # or headings
    if ("other_insertions" %in% include) {
        nonsubtots <- inserts[!is.abscat.subtotal(inserts) &
                                  !is.abscat.heading(inserts)]
        new_inserts <- c(new_inserts, nonsubtots)
    }

    # make an insertions object
    new_inserts <- Insertions(data=new_inserts)

    # collate with variable cats to get the order right (even if the cube_cells
    # aren't being requested)
    cats_collated <- collateCats(new_inserts, var_cats)

    # remove cube_cells if not in include
    if (!("cube_cells" %in% include)) {
        cats_collated <- cats_collated[!is.abscat.category(cats_collated)]
    }

    return(cats_collated)
}

# collate insertions and categories together
collateCats <- function (inserts, var_cats) {
    # setup abstract categories to collate into
    cats_out <- AbsCats(data=lapply(var_cats, function(x) {
        new_abscat <- as(x, "AbsCat")
        new_abscat$class <- class(x)
        return(new_abscat)
    }))

    # for each insert, find the position for its anchor, and add the insertion
    # at that position we use a for loop, because as we insert, the positions of
    # categories (which may serve as anchors) will change.
    for (insert in inserts) {
        pos <- findInsertPosition(insert, cats_out)
        new_abscat <- as(insert, "AbsCat")
        new_abscat$class <- class(insert)
        cats_out <- AbsCats(data = append(cats_out, list(new_abscat), pos))
    }
    return(cats_out)
}

# for a single Insertion, and a set of categories (or merged categories and
# insertions) find the position to insert to
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

