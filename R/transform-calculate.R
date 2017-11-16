# calculcate transforms for an array.
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

    # remove cube_cells if not in include. We need to add them and remove them
    # again even if include doesn't have "cube_cells" in order to ensure that
    # the insertions above are returned in a similar order as if there were
    # cube_cells returned.
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
        # save the original class for easy of iding former categories later.
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

#' Given values from an array and transforms, calculate the insertions
#'
#' @param vec values to transform (a single dimension of an array)
#' @param elements AbsCats of both categories and insertions to calculate.
#' Generally derived from `mapInsertions()`
#' @param var_cats the `Categories` object of the transform
#'
#' @return the values given in `vec`, with any insertions specified in
#' `trans` calculated
#' @keywords internal
calcInsertions <- function (vec, elements, var_cats) {
    # we always calculate insertions at the lowest dimension
    if (length(dim(vec)) > 1) {
        halt("Calculating varaible transforms is not implemented for dimensions ",
             "greater than 1.")
    }

    vec_out <- vapply(elements, function (element) {
        # if element is a category, return value
        if (is.abscat.category(element)) {
            return(vec[name(element)])
        }

        # if element is a heading return NA
        if (is.abscat.heading(element)) {
            return(NA)
        }

        # if element is a subtotal, sum the things
        if (is.abscat.subtotal(element)) {
            # grab category combinations, and then sum those categories.
            combos <- unlist(args(element))
            which.cats <- names(var_cats[ids(var_cats) %in% combos])
            return(sum(vec[which.cats]))
        }

        # finally, check if there are other functions, if there are warn, and
        # return NA
        not_subtotal <- element[["function"]] != "subtotal"
        if (not_subtotal) {
            warning("Transform functions other than subtotal are not supported.",
                    " Applying only subtotals and ignoring ", element[["function"]])
        }
        return(NA)
    }, double(1), USE.NAMES = TRUE)

    # make sure that the vector is named appropriately
    names(vec_out) <- names(elements)

    return(vec_out)
}
