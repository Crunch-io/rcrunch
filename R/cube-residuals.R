# standardized residuals formed by the subvariable-wise margins
# the internal chisq tests use rowSums and colSums, but we need
# the more abstract 'margin' (specifically cubeMarginTable) to
# get the right numbers for multiple response.
standardizedMRResiduals <- function(cube, types){
    # grab the dims from as.array because sometimes cat x MR has one too many
    cube_array <- as.array(noTransforms(cube))
    cube_dims <- dim(as.array(cube))

    # get counts for table, this will end up being a table because there are MRs
    n <- broadcast(margin.table(cube), dims = cube_dims)
    # get row values, broadcast in case this is a non-MR dimension
    r <- broadcast(margin.table(cube, 1), dims = cube_dims)
    # get column values, broadcast in case this is a non-MR dimension
    c <- broadcast(margin.table(cube, 2), dims = cube_dims)

    # These formulae follow Agresti, 2007 (section 2.4.5), but use count instead
    # of proportion space (cf the source for `chisq.test`, as of R 3.4.2)
    # calculate expected values
    E <- r * c / n
    # calculate variance
    V <- r * c * (n - r) * (n - c) / n^3

    return((cube_array - E) / sqrt(V))
}

#' Calculate standardized residuals from a CrunchCube
#'
#' Standardized residuals, \code{(observed - expected) / sqrt(V)}, where \code{V}
#' is the residual cell variance (Agresti, 2007, section 2.4.5). Special care is taken
#' for multiple-response variables which are in effect a series of separate tables where
#' \sQuote{not selected} cells for each item are are hidden.
#'
#' @param model A CrunchCube representing a contingency table
#'
#' @return an array of standardized residuals or Z-scores from the hypothesis being tested.
#' The default method is that the joint distributions of (weighted) counts are equal to the
#' marginal distributions of the table.
#'
#' @seealso [stats::chisq.test]
#' @references
#' Agresti, A. (2007)
#' \emph{An Introduction to Categorical Data Analysis, 2nd ed.},
#' New York: John Wiley & Sons. Page 38.
#' @importFrom stats chisq.test
#' @name cube-residuals
#' @aliases rstandard,CrunchCube-method cube-residuals
#' @exportMethod rstandard
setMethod('rstandard', 'CrunchCube', function(model) {
    if(length(dimensions(model)) > 2){
        halt(
            "Cannot compute residuals with more than two dimensions. Pick a ",
            "slice to evaluate."
        )
    }

    if (any(vapply(dimensions(model), is.selectedArrayDim, logical(1)))) {
        # TODO: remove the reference to `as_selected` when that becomes default
        halt("rstandard is not implemented with CrunchCubes that use selected ",
             "arrays. Selected arrays have been deprecated, please recreate ",
             "your cube using `as_selected()` around multiple response variables.")
    }

    # determine which dimensinos are multiple response, and treat those special
    # TODO: make this specific to multiple response rather than both cat array and MR
    types <- vapply(dimensions(model), function(dim) dim$references$type, character(1))

    if(any(types == 'subvariable_items')){
        return(standardizedMRResiduals(model, types))
    } else {
        return(chisq.test(as.array(model))$stdres)
    }
})



# broadcast a vector of values to a matrix with dimensions dims. Similar to
# but not exactly the same as numpy's `broadcast_to` method.
# If values is a vector: you only need to supply either nrow or ncol to
# broadcast in that direction (ie the other direction will be = length(values)).
# If values is a matrix: if the dims match, then return the matrix (no
# broadcasting needed).
# Finally, if both dimensions are provided, check to see which dimension
# length(values) matches, and make a matrix
# Examples:
# broadcast(c(1, 2, 3), nrow = 3)
# [,1] [,2] [,3]
# [1,]    1    2    3
# [2,]    1    2    3
# [3,]    1    2    3
#
# broadcast(c(1, 2, 3), ncol = 2)
# [,1] [,2]
# [1,]    1    1
# [2,]    2    2
# [3,]    3    3
#
# broadcast(c(1, 2, 3), dims = c(3, 2))
#      [,1] [,2]
# [1,]    1    1
# [2,]    2    2
# [3,]    3    3
#
# broadcast(c(1, 2, 3), dims = c(2, 3))
# [,1] [,2] [,3]
# [1,]    1    2    3
# [2,]    1    2    3
broadcast <- function(values, dims = c(NULL, NULL), nrow = dims[1], ncol = dims[2]) {
    # if values is already the correct shape, then return values immediately
    if (!is.null(dim(values)) & identical(dim(values), as.integer(c(nrow, ncol)))) {
        return(values)
    }

    # if there's only one nrow/ncol, use that alone (matching length of values)
    if (is.null(nrow)) {
        byrow <- FALSE
        nrow <- length(values)
    } else if (is.null(ncol)) {
        byrow <- TRUE
        ncol <- length(values)
    }

    # if both dimensions are specified, then we need to check which dimension
    # length(values) matches, and fill by the opposite.
    if (length(values) == ncol) {
        byrow <- TRUE
    } else if (length(values) == nrow) {
        byrow <- FALSE
    }

    if (!exists("byrow")) {
        if (length(values) == 1) {
            # if the values is one, then set byrow, and move on for a matrix of
            # all of that one value.
            byrow <- FALSE
        } else {
            halt(
                "Something has gone wrong broadcasting the vector ",
                dQuote(substitute(values)), " to the dimensions c(",nrow, ", ",
                ncol, ")"
            )
        }
    }

    return(matrix(values, byrow = byrow, ncol = ncol, nrow = nrow))
}
