#' @rdname cube-methods
#' @export
setMethod("[", "CrunchCube", function(x, i, j, ..., drop = TRUE) {
    if (as.logical(Sys.getenv("debug"))) browser()
    # Missing arguments to a subset method means "select all the items along this
    # dimension". In order to do this we need to capture the unevaluated arguments
    # and replace all the missing elements of that list with TRUE.

    # This is to handle cases where one subset argument is set, but drop is also
    # specified.
    if (nargs() == (3 - missing(drop))) {
        index <- eval(substitute(alist(i)))
    } else {
        index <- eval(substitute(alist(i, j, ...)))
    }
    index <- replaceMissingWithTRUE(index)

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
    # 2) Translated the provided index to one which explictly includes
    # hidden categories
    # 3) Subset the cube using the index 4) Return the display
    # setting to the original
    if (x@useNA != "always") {
        NA_setting <- x@useNA
        index <- skipMissingCategories(x, index, drop)
        x@useNA <- "always"
        out <- do.call("[", c(list(x, drop = drop), unname(index)))
        out@useNA <- NA_setting
        return(out)
    }
    if (as.logical(Sys.getenv("debug"))) browser()
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
            if (x@useNA == "no") {
                # This is used by skipMissingCategories below
                return("mr_select_drop")
            } else {
                # assign index value to "Selected" along the mr_selection dimension
                return(1)
            }
        }
        # return TRUE in all other circumstances
        return(TRUE)
    })

    return(out)
}


#' Handle missing categories in CrunchCube
#'
#' By default we don't display missing categories. The result is that when the
#' user subsets a cube with a missing category, we need to translate that subset
#' to only refer to the non-missing categories. This occurs if `cube@useNA` is
#' set to `"no"`. It can also occur if `useNA = "ifAny"` and there are no non-0
#' missing categories This function handles this behavior by translating the
#' user supplied indices to logical vectors which accounts for the missing
#' values.
#'
#' @param cube a CrunchCube
#' @param index a index of the real cube which was generated by
#'   `translateCubeIndex()`
#' @return A list of logical vectors
#' @keywords internal
skipMissingCategories <- function(cube, index, drop) {
    not_slected_dim <- !is.selectedDimension(cube)
    visible <- evalUseNA(cube@arrays$count, dims = cube@dims, useNA = cube@useNA)
    visible <- visible[not_slected_dim]
    missing_list <- lapply(cube@dims[not_slected_dim], function(x) x$missing)
    out <- mapply(
        function(missing, idx, vis) {
            if (identical(idx, "mr_select_drop")) {
                # select the "Selected" element of the selection dimension.
                ## TODO: Don't assume "selected" is position 1; consider an is.selected attr/vector
                return(c(TRUE, FALSE, FALSE))
            }
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

translateHidden <- function(index,
                            is_hidden,
                            drop = TRUE,
                            # vis default is to simplify tests
                            vis = rep(TRUE, length(is_hidden))) {
    if (length(index) > sum(!is_hidden)) {
        halt("Incorrect number of dimensions")
    }
    mapping <- data.frame(
        is_hidden = is_hidden,
        visible = vis
    )
     if (as.logical(Sys.getenv("debug"))) browser()
    mapping$true_index <-  1:nrow(mapping)
    mapping$new_order <- NA
    mapping$new_order[is_hidden] <- mapping$true_index[is_hidden]
    mapping$new_order[!is_hidden][1:length(index)] <- mapping$true_index[!is_hidden][index]
    out <- mapping[mapping$visible, "new_order"]

    if (drop && length(index) == 1) {
        return(out[!mapping$is_hidden & !is.na(out)])
    } else {
        return(out[!is.na(out)][index])
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
