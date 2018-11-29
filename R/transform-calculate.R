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
calcTransforms <- function(array, insert_funcs, dim_names = names(insert_funcs)) {
    # TODO: other possible Transforms

    # calculate the insertions based on cat_insert_map
    array <- calcInsertions(array, insert_funcs, dim_names)

    return(array)
}

# make a map of insertions and categories to be calculated
# this map is a collation of the categories with the insertions that are
# specified the output is an abstract category object with both `Category`s and
# `Insertion`s in it, in the order we want.
mapInsertions <- function(inserts, var_cats, include) {
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
    new_inserts <- Insertions(data = new_inserts)

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
collateCats <- function(inserts, var_cats) {
    # setup an empty AbstractCategories object to collate into
    cats_out <- AbstractCategories()
    cats_out@.Data <- var_cats

    if (length(var_cats) < 1) {
        halt("Can't collateCats with no categories.")
    }

    # add a fake botom category to attach things that are bottom anchored below
    # the last category
    cats_out@.Data <- append(
        cats_out@.Data,
        list(AbstractCategory(name = "__fake__bottom__category__"))
    )

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
        pos <- findInsertPosition(insert, cats_out)
        cats_out@.Data <- append(cats_out, list(insert), pos)
    }

    # remove the fake bottom category
    not_fake_bottom <- which(names(cats_out) !=  "__fake__bottom__category__")
    cats_out@.Data <- cats_out@.Data[not_fake_bottom]

    return(cats_out)
}

# for a single Insertion, and a set of categories (or collated categories and
# insertions) find the position to insert to
findInsertPosition <- function(insert, cats) {
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

    # all other situations, put after the bottom category
    return(which("__fake__bottom__category__" == names(cats)))
}

#' Given a vector of values and elements, calculate the insertions
#'
#' @param vec values to transform (a single dimension of an array)
#' @param trans_funcs a (named) list of functions that return the correct vector
#'  of the array (with desired insertions and transformations included)
#' @param dim_names the names of the dimensions (although this is calculable at
#'   call-time, it's much more efficient to provide this to the call)
#'
#' @return the values given in `vec`, with any insertions specified in
#' `trans` calculated and inserted
#' @keywords internal
calcInsertions <- function(vec, insert_funcs, dim_names = names(insert_funcs)) {
    # we always calculate insertions at the lowest dimension so warn if there is
    # more than one dimension.
    if (length(dim(vec)) > 1) {
        halt(
            "Calculating varaible transforms is not implemented for dimensions ",
            "greater than 1."
        )
    }

    # make the actual calculations and insertions
    vec_out <- vapply(seq_along(insert_funcs), function(ind) {
        return(insert_funcs[[ind]](vec))
    }, double(1))

    # make sure that the vector is named appropriately
    names(vec_out) <- dim_names

    return(vec_out)
}
