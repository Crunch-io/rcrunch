#' Calculate transforms for an array
#'
#' Given an array and transforms calculate values based on the transforms and
#' then return the values calculated that are specified.
#'
#' @param array the array to use (this is most likely from a `CrunchCube`` object).
#' The cells from this array are the cube_cells referred to in `include`
#' @param trans a `Transform` object to get the transformations to calculate
#' @param var_cats a `Categories` object that are the categories for the
#' variable the transforms are being calculated for
#' @param include what values should be included in the output? (default: "subtotals", "headings", "cube_cells", "other_insertions")
#' * cube_cells -- the values from the array given in `array`
#' * subtotals -- insertions that have the function subtotal
#' * headings -- insertions that have no function specified
#' * other_insertions -- any other insertion that is present in the transforms
#'
#' @return an array with transforms calculated and added or applied
#' @keywords internal
calcTransforms <- function (array, trans, var_cats,
                            include = c("subtotals", "headings", "cube_cells", "other_insertions")) {
    # TODO: other possible Transforms

    ### Insertions
    # collate insertions with categories for rearranging and calculation purposes
    # we need the categories to know what order the the cube cells should be in
    # and where to insert insertions (as they are `anchor`ed) to a category id.
    cat_insert_map <- mapInsertions(trans$insertions, var_cats, include = include)

    # calculate the insertions based on cat_insert_map
    array <- calcInsertions(array, cat_insert_map, var_cats)

    return(array)
}

# make a map of insertions and categories to be calculated
# this map is a collation of the categories with the insertions that are
# specified the output is an abstract category object with both `Category`s and
# `Insertion`s in it, in the order we want.
mapInsertions <- function (inserts, var_cats, include) {
    # make an empty list to store the insertions in
    new_inserts <- list()

    # add subtotals if they are found in include
    if ("subtotals" %in% include) {
        subtots <- inserts[are.Subtotals(inserts)]
        new_inserts <- c(new_inserts, subtots)
    }

    # add headings if they are in include
    if ("headings" %in% include) {
        new_inserts <- c(new_inserts, inserts[are.Headings(inserts)])
    }

    # add non-subtotal insertions we must check that inserts are not subtotals
    # or headings
    if ("other_insertions" %in% include) {
        nonsubtots <- inserts[!are.Subtotals(inserts) &
                                  !are.Headings(inserts)]
        new_inserts <- c(new_inserts, nonsubtots)
    }

    # make an insertions object that includes only the insertions that were
    # requested in include
    new_inserts <- Insertions(data=new_inserts)

    # collate with variable categories to get the order right. This is necessary
    # even if the cube_cells aren't being requested because the insertion order
    # is dependent (partially) on where they are anchored. This ensures that
    # the insertions are returned in a similar order as if there cube_cells was
    # in include.
    cats_collated <- collateCats(new_inserts, var_cats)

    # remove cube_cells if not in include
    if (!("cube_cells" %in% include)) {
        cat_inds <- unlist(vapply(cats_collated, is.category, logical(1)))
        cats_collated <- cats_collated[!cat_inds]
    }

    return(cats_collated)
}

# collate insertions and categories together
# given a set of insertions and categories, collate together into a single set
# of AbstractCategories which includes both `Category`s and `Insertion`s
collateCats <- function (inserts, var_cats) {
    # setup an empty AbstractCategories object to collate into
    cats_out <- AbstractCategories()
    cats_out@.Data <- var_cats
    
    if (length(var_cats) < 1) {
        halt("Can't collateCats with no categories.")
    }
    
    last_category <- tail(ids(var_cats), 1)

    # for each insert, find the position for its anchor, and add the insertion
    # at that position we use a for loop, because as we insert, the positions of
    # categories (which may serve as anchors) will change. We also reverse the
    # list because for insertions that have the same anchor, we want to maintain
    # the order as it is stored in the API. To do this we insert the last one 
    # first so that when we insert ones before that they are higher up (closer 
    # to the category anchor)
    # Categoies: A, B, C 
    # Insertions: [{name = first, anchor = A}, {name = second, anchor = A}]
    # Desired result: A, first, second, B, C
    # Result if not `rev(inserts)`: A, second, first, B, C
    for (insert in rev(inserts)) {
        pos <- findInsertPosition(insert, cats_out, last_category)
        cats_out@.Data <- append(cats_out, list(insert), pos)
    }
    return(cats_out)
}

# for a single Insertion, and a set of categories (or collated categories and
# insertions) find the position to insert to
findInsertPosition <- function (insert, cats, last_category) {
    anchr <- anchor(insert)
    # if the anchor is top, put at the beginning
    if (anchr == "top") {
        return(0)
    }

    # if the anchor is the id of a non-missing category put it after that cat
    if (anchr %in% ids(cats)) {
        which_cat <- which(anchr == ids(cats))
        if (!is.na(cats[[which_cat]])) {
            return(which_cat)
        }
    }

    # all other situations, put after the last category
    return(which(last_category == ids(cats)))
}

#' Given a vector of values and elements, calculate the insertions
#'
#' @param vec values to transform (a single dimension of an array)
#' @param elements AbstractCategories of both `Category`s and `Insertion`s to
#' calculate. Generally derived from `mapInsertions()`
#' @param var_cats the `Categories` object tat corresponds to the vector in
#' `vec` of the transform
#'
#' @return the values given in `vec`, with any insertions specified in
#' `trans` calculated and inserted
#' @keywords internal
calcInsertions <- function (vec, elements, var_cats) {
    # we always calculate insertions at the lowest dimension so warn if there is
    # more than one dimension.
    if (length(dim(vec)) > 1) {
        halt("Calculating varaible transforms is not implemented for dimensions ",
             "greater than 1.")
    }

    # make the actual calculations and insertions
    vec_out <- vapply(elements, function (element) {
        # if element is a category, simply return the value
        if (is.category(element)) {
            return(vec[name(element)])
        }

        # if element is a heading return NA (since there is no value to be
        # calculated but we need a placeholder non-number)
        if (is.Heading(element)) {
            return(NA)
        }

        # if element is a subtotal, sum the things it corresponds to which are
        # found with arguments()
        if (is.Subtotal(element)) {
            # grab category combinations, and then sum those categories.
            combos <- unlist(arguments(element, var_cats))
            which.cats <- names(var_cats[ids(var_cats) %in% combos])
            return(sum(vec[which.cats]))
        }

        # if element is a summaryStat, grab the function from summaryStatInsertions
        # to use.
        if (is.SummaryStat(element)) {
            statFunc <- summaryStatInsertions[[func(element)]]
            return(statFunc(element, var_cats, vec))
        }

        # finally, check if there are other functions, if there are warn, and
        # then return NA
        unknown_funcs <- !(element[["function"]] %in% c("subtotal", names(summaryStatInsertions)))
        if (unknown_funcs) {
            warning("Transform functions other than subtotal are not supported.",
                    " Applying only subtotals and ignoring ", element[["function"]])
        }
        return(NA)
    }, double(1), USE.NAMES = TRUE)

    # make sure that the vector is named appropriately
    names(vec_out) <- names(elements)

    return(vec_out)
}
