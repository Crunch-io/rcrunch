#' Calculate standardized residuals from a CrunchCube
#'
#' Standardized residuals, \code{(observed - expected) / sqrt(V)}, where
#' \code{V} is the residual cell variance (Agresti, 2007, section 2.4.5).
#' Special care is taken for multiple-response variables which are in effect a
#' series of separate tables where \sQuote{not selected} cells for each item are
#' are hidden.
#'
#' @param x A CrunchCube representing a contingency table
#' @param model A CrunchCube representing a contingency table (for `rstandard()`
#'   only)
#'
#' @return an array of standardized residuals or Z-scores from the hypothesis
#'   being tested. The default method is that the joint distributions of
#'   (weighted) counts are equal to the marginal distributions of the table.
#'
#' @seealso [stats::chisq.test]
#' @references
#' Agresti, A. (2007)
#' \emph{An Introduction to Categorical Data Analysis, 2nd ed.},
#' New York: John Wiley & Sons. Page 38.
#' @importFrom stats chisq.test
#' @name cube-residuals
#' @aliases rstandard,CrunchCube-method cube-residuals zScores
setGeneric("zScores", function(x) standardGeneric("zScores"))

#' @rdname cube-residuals
#' @export
setMethod("zScores", "CrunchCube", function(x) {
    if (length(dimensions(x)) > 2) {
        halt(
            "Cannot compute residuals with more than two dimensions. Pick a ",
            "slice to evaluate."
        )
    }

    if (any(startsWith(getDimTypes(x), "mr_"))) {
        return(standardizedMRResiduals(x))
    } else {
        array_to_test <- as.array(noTransforms(x))
        return(chisq.test(array_to_test)$stdres)
    }
})

# standardized residuals formed by the subvariable-wise margins
# the internal chisq tests use rowSums and colSums, but we need
# the more abstract 'margin' (specifically cubeMarginTable) to
# get the right numbers for multiple response.
standardizedMRResiduals <- function(cube) {
    cube_array <- as.array(noTransforms(cube))
    cube_dims <- dim(cube_array)

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

# for bacwards compatibility
#' @rdname cube-residuals
#' @export
rstandard <- function(model) zScores(x = model)

#' Column and row comparison
#'
#' Comparing a column or row with a baseline column or row. This calculates the
#' z-score for the cells when comparing `x` to the baseline columns
#'
#' @param cube a cube to calculate the comparison on
#' @param dim which dimension is being compared (`rows` or `cols`, only valid
#'   for `compareDims()`)
#' @param baseline a character, the column to use as a baseline to compare `x`
#'   against
#' @param x a character, the column to compare against the baseline
#' @param ... arguments passed from `compareRows()` or `compareCols()` to
#'   `compareDims()` (i.e. `baseline` and `x`)
#'
#' @return the z-score for the column or row given in `x`
#'
#' @name dimension-comparison
NULL

#' @rdname dimension-comparison
#' @export
compareCols <- function(cube, ...) compareDims(cube = cube, dim = "cols", ...)

#' @rdname dimension-comparison
#' @export
compareRows <- function(cube, ...) compareDims(cube = cube, dim = "rows", ...)

#' @rdname dimension-comparison
#' @export
compareDims <- function(cube, dim = c("cols", "rows"), baseline, x) {
    dim <- match.arg(dim)

    # grab the names of extents for each dimensions
    # TODO: we shouldn't need to do this if we can just pass baseline/x into the
    # subsetting function when we can subset by name _or_ index natively
    if (dim == "cols") {
        names <- colnames(as.array(cube))
    } else if (dim == "rows") {
        names <- rownames(as.array(cube))
    }

    # convert to numeric indices
    # TODO: remove when we can accept either characters or indices and pass them
    # to subsetting unmolested
    if (is.character(baseline)) {
        baseline_ind <- which(names == baseline)
    } else {
        stop("Currently, column comparison only accepts category names.")
    }
    if (is.character(x)) {
        x_ind <- which(names == x)
    } else {
        stop("Currently, column comparison only accepts category names.")
    }

    # ensure that the extents given are in the cube
    # TODO: remove when we can defer this check to the subsetting methods
    not_in_cube <- !(c(baseline, x) %in% names)
    if (any(not_in_cube)) {
        stop(c(baseline, x)[not_in_cube], " is not a column or row in the cube")
    }

    # ensure that there are not MRs on the comparison direction
    dim_types <- getDimTypes(cube)
    if ((dim == "cols" & startsWith(dim_types[2], "mr_")) |
        (dim == "rows" & startsWith(dim_types[1], "mr_"))) {
        stop(
            "Column or row z-scores are not implemented for multiple response ",
            "dimensions"
        )
    }

    # generate the 2xm or nx2 table tfor testing
    if (dim == "cols") {
        sub_cube <- cube[, c(x_ind, baseline_ind)]
    } else if (dim == "rows") {
        sub_cube <- cube[c(x_ind, baseline_ind), ]
    }

    # make the test
    out <- zScores(sub_cube)

    # only return the scores for x
    if (dim == "cols") {
        out <- out[, x, drop = FALSE]
    } else if (dim == "rows") {
        out <- out[x, , drop = FALSE]
    }

    return(out)
}


#' Pairwise column and row comparison
#'
#' Given a single baseline column compare each other row or column against this
#' baseline. Internally this function uses `compareDims()` iteratively.
#'
#' *Warning* since there is more than one comparison being made against each
#' baseline the z-scores, and especially the p-values derived from these
#' z-scores should be interpreted with caution. Using standard p-value cutoffs
#' will result in anti-conservative interpretations because of the multiple
#' comparisons problem. Adjustments to p-value cut offs (e.g. Bonferonni
#' correction) should be used when interpreting z-scores from the
#' `compare[Rows|Cols|Dims]Pairwise()` family of functions.
#'
#' @param cube a cube to calculate the comparison on
#' @param dim which dimension is being compared (`rows` or `cols`, only valid
#'   for `compareDims()`)
#' @param baseline a character, the column to use as a baseline to compare
#'   against all other columns
#' @param ... arguments passed from `compareRowsPairwise()` or
#'   `compareColsPairwise()` to `compareDimsPairwise()` (i.e. `baseline`)
#'
#' @return an array of z-score for the all the columns or rows compared to
#'   `baseline`. The `baseline` column is all 0s
#'
#' @name dimension-comparison-pairwise
NULL

#' @rdname dimension-comparison-pairwise
#' @export
compareColsPairwise <- function(cube, ...) {
    compareDimsPairwise(cube = cube, dim = "cols", ...)
}

#' @rdname dimension-comparison-pairwise
#' @export
compareRowsPairwise <- function(cube, ...) {
    compareDimsPairwise(cube = cube, dim = "rows", ...)
}

#' @rdname dimension-comparison-pairwise
#' @export
compareDimsPairwise <- function(cube, dim = c("cols", "rows"), baseline) {
    dim <- match.arg(dim)

    # grab the names of extents for each dimensions
    # TODO: we shouldn't need to do this if we can just pass baseline/x into the
    # subsetting function when we can subset by name _or_ id natively
    if (dim == "cols") {
        len_out <- dim(as.array(cube))[1]
        names <- colnames(as.array(cube))
    } else if (dim == "rows") {
        len_out <- dim(as.array(cube))[2]
        names <- rownames(as.array(cube))
    }

    to_compare <- names[!(names %in% baseline)]

    out <- vapply(
        names, function(one_extent) {
            if (one_extent == baseline) {
                return(rep(0, len_out)) # not the right shape
            }

            # generate the 2xm or nx2 table tfor testing
            return(compareDims(cube, baseline = baseline, x = one_extent, dim = dim))
        },
        numeric(len_out)
    )

    out <- simplify2array(out)

    # if the dimension is rows, the array that vapply returns needs to be
    # transposed
    if (dim == "rows") {
        out <- t(out)
    }

    dimnames(out) <- dimnames(as.array(cube))

    return(out)
}

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
                dQuote(substitute(values)), " to the dimensions c(", nrow, ", ",
                ncol, ")"
            )
        }
    }

    return(matrix(values, byrow = byrow, ncol = ncol, nrow = nrow))
}
