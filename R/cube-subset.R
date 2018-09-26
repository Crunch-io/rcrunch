#' @rdname cube-methods
#' @export
setMethod("[", "CrunchCube", function(x, i, j, ..., drop = TRUE) {
    # Missing arguments to a subset method means "select all the items along this
    # dimension". In order to do this we need to capture the unevaluated arguments
    # and replace all the missing elements of that list with TRUE.
    #
    # The 3 - missing(drop) is to handle cases were drop is specified but only one
    # index is used.
    if (nargs() == (3 - missing(drop))) {
        index <- eval(substitute(alist(i)))
    } else {
        index <- eval(substitute(alist(i, j, ...)))
    }
    index <- replaceMissingWithTRUE(index)
    useNA_list <- evalUseNA(x@arrays$count, dims = x@dims, useNA = x@useNA)
    useNA_list <- useNA_list[!is.selectedDimension(x)]
    index <- mapply(
        replaceCharWithNumeric,
        cat_names = dimnames(x),
        idx = index,
        visible = useNA_list,
        SIMPLIFY = FALSE
    )

    dims <- dim(x)
    # Check if the user has supplied the right number of dimensions
    if (length(index) != length(dims)) {
        halt(
            "You must supply ",
            length(dims),
            " dimensions to subset a ",
            length(dims),
            " dimensional cube; you supplied ",
            length(index),
            "."
        )
    }

    # This block of code checks whether the user has supplied a valid index. For
    # instance it will error if they tried to select element 4 from a dimension
    # with only three elements.
    err_indices <- lapply(index, function(idx) {
        if (is.numeric(idx)) {
            return(max(idx))
        } else if (is.logical(idx) && !isTRUE(idx)) {
            return(length(idx))
        }
        # Assume other indices are valid and trust that the array subsetting
        # method will fail if they are invalid.
        return(TRUE)
    })

    isInValid <- function(idx, dim) {
        if (isTRUE(idx)) {
            return(FALSE)
        }
        return(idx > dim)
    }

    invalid_indices <- unlist(mapply(isInValid, idx = err_indices, dim = dims, SIMPLIFY = FALSE))
    if (any(invalid_indices)) {
        errs <- paste(
            "- At position", which(invalid_indices),
            "you tried to select element",
            err_indices[invalid_indices],
            "when the dimension has",
            dims[invalid_indices],
            "elements."
        )
        halt(
            "Invalid subset:\n",
            paste0(errs[1:min(sum(invalid_indices), 5)], collapse = "\n")
        )
    }

    # The index needs to be transformed to skip over missing categories.
    # See the "Crunch Internals" vignette for more on this.
    #
    # In order to ensure that reording subsets like `cube[c(2,1), ]` preseve
    # hidden categories we do the following:
    # 1) Change the cube to useNA = "always"
    # 2) Translate the provided index to one which accounts for hidden categories
    # 3) Subset the cube using the index
    # 4) Return the display setting to the original
    if (x@useNA != "always") {
        NA_setting <- x@useNA
        index <- skipMissingCategories(x, index, drop)
        x@useNA <- "always"
        on.exit(expr = {
            out@useNA <- NA_setting
        })
    }

    # We then translate the index which the user supplied of the user cube to
    # the dimensionality of the real cube. See documentation to
    # translateCubeIndex
    translated_index <- translateCubeIndex(x, index, drop)

    # Finally we can use that translated subset on the real cube.
    out <- x
    out@arrays[] <- lapply(out@arrays, function(arr) {
        # select_dims must be FALSE here because we do not want to treat them
        # differently
        subsetCubeArray(arr, translated_index, drop, selected_dims = FALSE)
    })

    # We also need to subset the @dims part of the object using the same index.
    # This is so that other cube methods, like as.array, don't get confused.
    out@dims[] <- mapply(subsetArrayDimension,
        dim = x@dims,
        dim_type = getDimTypes(x),
        idx = translated_index,
        SIMPLIFY = FALSE
    )
    # Some dimensions need to be dropped because they were dropped from the
    # cube@array slot.
    if (drop) {
        keep_args <- vapply(translated_index, function(a) {
            if (is.logical(a)) {
                return(sum(a) != 1 || isTRUE(a))
            } else {
                return(length(a) != 1)
            }
        }, FUN.VALUE = logical(1))
        out@dims <- out@dims[keep_args]
    }
    return(out)
})

#' Transform character vectors into indices
#'
#' When a user supplies character strings to subset a cube, we need to translate it
#' into a numeric index to subset the cube. This function does that as well as
#' some error checking. It is broken out for testing.
#'
#' @param cat_names The category names for a given dimension
#' @param idx The index
#' @param visible whether or not a category is visible to the user
#'
#' @keywords internal
replaceCharWithNumeric <- function(cat_names, idx, visible = TRUE) {
    if (!is.logical(idx) &&
        length(idx) != length(unique(idx))) {
        halt("Index is not unique. Cube subetting is only supported for unique indices.")
    }

    if (is.character(idx)) {
        if (length(cat_names) != length(unique(cat_names))) {
            halt("Duplicate categories detected, please use a numeric or logical subset.")
        }
        visible_categories <- cat_names[visible]
        not_categories <- !(idx %in% visible_categories)
        if (any(not_categories)) {
            halt("Invalid categories: ", serialPaste(idx[not_categories]))
        }
        return(match(idx, visible_categories))
    }
    return(idx)
}

#' Replace missing elements with TRUE
#'
#' When subsetting, missingness stands in for selecting all of the elements of that
#' dimension. This gets tricky when you are selecting n dimensions and so need to
#' capture the dimensions with `...`.  This function checks if an element of a list
#' is missing and replaces that element with `TRUE` in order to handle this case.
#'
#' @param l a list
#' @return a list
#' @keywords internal
replaceMissingWithTRUE <- function(l) {
    out <- lapply(l, function(x) {
        if (is.symbol(x)) {
            x <- tryCatch(eval(x), error = function(c) {
                msg <- conditionMessage(c)
                if (msg == "argument is missing, with no default") {
                    return(TRUE)
                } else {
                    stop(c)
                }
            })
        }
        return(eval(x))
    })
    return(out)
}

#' Translate user facing cube subset to programmatic cube subset
#'
#' Cubes that include multiple response variables create a special kind of
#' complexity. Multiple response variables are actually 2d arrays with the
#' responses along one dimension (`cat`, `dog`, `fish`) and the selection
#' status along the second dimension (`selected`, `not_selected`). When an MR
#' variable is crossed with a categorical variable it creates a 3d array with
#' the categorical variable's categories along one dimension and the MR
#' dimensions on the other two.
#'
#' The complexity is that while the real MR cube includes two dimensions per MR,
#' we only show the user one dimension per MR which represents only the
#' `selected` items. This means that every cube has two different
#' representations, the low dimensional user cube, and the higher dimensional
#' real cube. This function translates user cube subsets into the higher
#' dimensional subset. In the case above, the user would see a 2d cube and
#' subset it with `user_cube[1:2, 1:2]` in order to subset the programmatic cube
#' we need to translate this to `prog_cube[1:2, 1:2, ]` in order to select the
#' right variables of the high dimensional cube.
#' @param x  a Crunch Cube
#' @param subset a list of array extent indices (for the user-cube)
#' @param drop whether to drop unnecessary dimensions.
#' @keywords internal
#' @return a list of array extent indices (for the real-cube)
translateCubeIndex <- function(x, subset, drop) {
    is_selected <- is.selectedDimension(x@dims)
    if (length(is_selected) == length(dim(x))) {
        # no MR variables so no need to translate the subset
        return(subset)
    }

    # This is the main work of the subset translation, just taking the user
    # supplied subset and projecting them up to real cube dimension
    out <- subset[real2userMargin(seq_along(is_selected), cube = x, dedupe = FALSE)]

    # Dropping MR variables is a bit special. Whenever the user drops the MR
    # dimension the MR selection dimension is also dropped. This checks if it's
    # an MR dimension which is being dropped, and assigns an appropriate index
    # value.
    out <- lapply(seq_along(out), function(i) {
        if (!is_selected[i]) {
            # if we aren't a selected dimension, return without modification
            return(out[[i]])
        }

        # If we are dropping and MR response variable is a single number
        if (drop && length(out[[i - 1]]) == 1 && !isTRUE(out[[i - 1]])) {
            return(1)
        }
        # return TRUE in all other circumstances
        return(TRUE)
    })

    return(out)
}


#' Handle missing categories in CrunchCube
#'
#' Missing categories are not displayed when `cube@useNA` is set to `"no"` or `ifANy`.
#' The way we handle these cases is to change the `useNA` setting to `always`, subset
#' the cube, and then change it back to the original value. For this to work we
#' need to translate the indices that the user supplied to properly skip over hidden
#' categories.
#'
#' @param cube a CrunchCube
#' @param index the user supplied index
#' @param drop whether dimensions should be dropped
#' @return A list translated indexes
#' @keywords internal
skipMissingCategories <- function(cube, index, drop) {
    not_slected_dim <- !is.selectedDimension(cube)
    visible <- evalUseNA(cube@arrays$count, dims = cube@dims, useNA = cube@useNA)
    visible <- visible[not_slected_dim]
    missing_list <- lapply(cube@dims[not_slected_dim], function(x) x$missing)
    out <- mapply(
        function(missing, idx, vis) {
            if (isTRUE(idx)) {
                out <- rep(TRUE, length(missing))
            } else {
                out <- translateHidden(idx, missing, drop, vis)
            }
            return(out)
        },
        missing = missing_list, idx = index, vis = visible, SIMPLIFY = FALSE
    )
}

#' Translate provided index to an index which accounts for hidden categories.
#'
#' When the user subsets a cube in which "useNA" is either "no" or "ifAny" they
#' are not interacting with hidden categories. This function takes the index which
#' they provide and translates it to an index which includes hidden categories.
#'
#' For example, if `v` includes c("cat1", "hidden_cat", "cat2") and `useNA` is
#' "no". The user will see `v` as c("cat1", "cat2") and might subset it with
#' `v[c(2,1)]`. We need to skip over the hidden category and change the index
#' to account for any hidden categories which might appear in the vector. Here
#' the translated index would be `c(3,1`. This function does all of that.
#'
#' @param index The index to be translated
#' @param not_hidden Logical, `TRUE` indicates a category is not hidden.
#' @param drop Should dimensions with a single category be dropped
#' @param vis Logical, is a category visible or not. Because of the `useNA` behavior,
#' categories can be visible even if they are hidden.
#'
#' @return The translated index
#' @keywords internal
translateHidden <- function(index,
                            not_hidden,
                            drop = TRUE,
                            # vis default is to simplify tests
                            vis = not_hidden) {
    if (length(index) > sum(vis)) {
        halt("Incorrect number of dimensions")
    }

    # This data frame is not necessary, but does help keep track of how the
    # various indices and visability statuses are related.
    mapping <- data.frame(
        is_hidden = !not_hidden,
        visible = vis
    )
    mapping$true_index <- 1:nrow(mapping)
    mapping$new_order <- NA

    # The non-visible dimensions get their true index
    mapping$new_order[!vis] <- mapping$true_index[!vis]

    # This translate the provided index, to its true index accounting for hidden
    # categories. It also handles reordering indexes like cube[(2,1), ]
    mapping$new_order[vis][1:length(index)] <- mapping$true_index[vis][index]

    out <- mapping$new_order
    if (length(index) == 1 && !drop) {
        return(out[!is.na(out)])
    } else {
        return(out[vis & !is.na(out)])
    }
}


subsetArrayDimension <- function(dim, idx, dim_type) {
    dim$missing <- dim$missing[idx]
    dim$name <- dim$name[idx]

    # subset the category or item metadata
    if (dim_type %in% c("categorical", "ca_categories", "mr_selections")) {
        dim$references$categories <- dim$references$categories[idx]
    } else if (dim_type %in% c("mr_items", "ca_items")) {
        dim$references$subreferences <- dim$references$subreferences[idx]
    }

    return(dim)
}
