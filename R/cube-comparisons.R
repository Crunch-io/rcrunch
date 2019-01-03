#' Column and row comparison
#'
#' Comparing a column or row with a baseline column or row. This calculates the
#' test statistic \eqn{\Chi^2} of independence for each pair of columns/rows.
#'
#' @param cube a CrunchCube to calculate the comparison on
#' @param dim which dimension is being compared (`cols` or `rows`, only valid
#'   for `compareDims()`)
#' @param baseline a character, the column/row to use as a baseline to compare `x`
#'   against
#' @param x a character, the column/row to compare against the baseline
#' @param ... arguments passed from `compareRows()` or `compareCols()` to
#'   `compareDims()` (i.e. `baseline` and `x`)
#'
#' @return the value for the column or row given in `x`, default is Chi-squared
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
compareDims <- function(cube, dim = c("cols", "rows"), baseline, x, value=c("statistic", "p.value")) {
    dim <- match.arg(dim)
    value <- match.arg(value)
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
    if (is.character(baseline) && length(baseline) == 1) {
        baseline_ind <- which(names == baseline)
    } else {
        stop("Currently, column comparison only accepts at most one category name.")
    }
    if (is.character(x) && length(x) == 1) {
        x_ind <- which(names == x)
    } else {
        stop("Currently, column comparison only accepts at most one category name.")
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
    
    # return NA for the diagonal (comparison is self)
    # note: this is duplicated work in compareCols
    if (x_ind == baseline_ind) {
        return(NA)
    }
    
    # generate the 2xm or nx2 table tfor testing
    if (dim == "cols") {
        sub_cube <- cube[, c(x_ind, baseline_ind)]
    } else if (dim == "rows") {
        sub_cube <- cube[c(x_ind, baseline_ind), ]
    }
    
    # make the test
    # TODO: fix this output for the diagonal case
    out <- chisq.test(as.array(sub_cube))[[value]]
    
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
#' will result in anti-conservative interpretations because of the \href{https://en.wikipedia.org/wiki/Multiple_comparisons_problem}{multiple
#' comparisons problem}.
#' Adjustments to p-value cut offs (e.g. \href{https://en.wikipedia.org/wiki/Bonferroni_correction}{Bonferonni correction}) should be used when interpreting z-scores from the
#' `compare[Rows|Cols|Dims]Pairwise()` family of functions.
#'
#' @param cube a CrunchCube to calculate the comparison on
#' @param dim which dimension is being compared (`rows` or `cols`, only valid
#'   for `compareDims()`)
#' @param baseline a character, the name of a column or row to compare
#'   against each of the others in turn
#' @param ... arguments passed from `compareRowsPairwise()` or
#'   `compareColsPairwise()` to `compareDimsPairwise()` (e.g., `baseline`; `value`)
#'
#' @return a vector of representing the baseline column’s test statistic
#' with respect to each of the others.
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
compareDimsPairwise <- function(cube, dim = c("cols", "rows"), baseline, value=c("statistic", "p.value")) {
    dim <- match.arg(dim)
    
    # grab the names of extents for each dimensions
    # TODO: we shouldn't need to do this if we can just pass baseline/x into the
    # subsetting function when we can subset by name _or_ id natively
    if (dim == "cols") {
        names <- colnames(as.array(cube))
    } else if (dim == "rows") {
        names <- rownames(as.array(cube))
    }
    
    to_compare <- names[!(names %in% baseline)]
    
    out <- vapply(
        names, function(one_extent) {
            # generate the 2xm or nx2 table for testing
            return(compareDims(cube, baseline = baseline, x = one_extent, dim = dim, value=value))
        }
        , numeric(1)
    )
    
    out <- simplify2array(out)
    
    # if the dimension is rows, the array that vapply returns needs to be
    # transposed
    if (dim == "rows") {
        out <- t(out)
    }
    
    names(out) <- names
    
    return(out)
}

#' Matrix of Chi-Squared Statistics for all rows or columns
#' 
#' Generate a matrix of pairwise comparisons of rows or columns, each against 
#' the others.
#'
#' @param cube 
#' @param dim which dimension along which to compare (`cols` or `rows`)
#'
#' @return A symmetric square matrix of all column-comparison or row-comparison
#' \eqn{\Chi^2} statistics. Typical element \eqn{i,j} is the test statistic equivalent
#' to \code{chisq.test} subsetting to just columns i and j (for dim="cols").
#' @export
#'
#' @examples
#' \dontrun{
#' some_cube <- crunch_example_data(cat_by_cat)
#' 
#' # By default, return the 'statistic' of chisq.test, evaluated
#' # for each pair of columns pairwise:
#' pairwiseMatrix(some_cube)
#' 
#' # or each pair of rows:
#' pairwiseMatrix(some_cube, dim="rows")
#' 
#' # Or, specify another "value" from `chisq.test`:
#' pairwiseMatrix(some_cube, value="p.value")
#' }
pairwiseMatrix <- function(cube, dim=c("cols", "rows"), value=c('statistic', 'p.value')){
    dim <- match.arg(dim)
    value <- match.arg(value)
    a <- as.array(cube)
    if (dim == "cols") {
        names <- colnames(a)
    } else if (dim == "rows") {
        names <- rownames(a)
    }
    out <- vapply(names, function(cur) {
            compareDimsPairwise(cube, dim=dim, baseline=cur, value=value)
        },
        numeric(length(names))
    )
    return(simplify2array(out))
}

#' Matrix of Chi-Squared Statistics for all rows or columns
#' 
#' Use the alternative Wishart method of forming the matrix of column- or row-wise
#' comparison Chi-squared test statistics for a categorical-by-categorical 
#' contingency table.
#'
#' @param cube a CrunchCube of counts: a crosstab of two categorical variables
#' @param dim dimension along which to compare proportions (default `cols` or `rows`)
#'
#' @return A symmetric square matrix of Chi-squared statistics for a crosstab
#' @export
#'
#' @examples
#' \dontrun{
#' # TODO: include Hirotsu occupation-illness example cube and output
#' illness_occupation <- crunch_example_data(hirotsu)
#' # Chi-squared statistic for joint null hypothesis that each
#' # column is equal to each other column.
#' 
#' wishartMatrix(illness_occupation)
#' }
wishartMatrix <- function(cube, dim=c("cols","rows")){
    dim <- match.arg(dim)
    a <- as.array(cube)
    if(dim == "rows") {
        # This would be simpler if we could locally transpose the CrunchCube,
        # but instead these results will have transposed shapes for rows.
        a <- t(a)
        props <- t(prop.table(cube, 1))
        margins <- margin.table(cube, 1)
        weights <- t(prop.table(margin.table(cube, 2)))
    } else {
        props <- prop.table(cube, 2)
        margins <- margin.table(cube, 2)
        weights <- prop.table(margin.table(cube, 1))
    }
    names <- dimnames(a)[[2]]
    
    
    I <- ncol(a)
    X2 <- matrix(0.0, nrow = I, ncol = I)
    for (i in 2:I){
            for (j in 1:(i-1)) {
                X2[j,i] <- X2[i,j] <- sum( (props[,i] - props[,j])^2 / weights ) /
            ( 1 / margins[i] + 1 / margins[j] )
            }
    }
    if(dim == "rows") {
        X2 <- t(X2)
    }
    dimnames(X2) <- list(names, names)
    return(X2)
}


#' P-values for pairwise comparison on a dimension of a categorical crosstab
#'
#' @param cube a CrunchCube
#' @param dim dimension which jointly forms the null hypothesis that each
#' vector is equal to all of the others.
#'
#' @return A symmetric square matrix of the size of `dim`. Typical element
#' \eqn{i,j} is the P-value associated with the hypothesis that proportions in
#' row or column \eqn{i} are equal to those in row or column \eqn{j}.
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' # TODO: include Hirotsu occupation-illness example cube and output
#' illness_occupation <- crunch_example_data(hirotsu)
#' # P-value for joint null hypothesis that each
#' # column is equal to each other column.
#' 
#' wishartPvalues(illness_occupation)
#' }
wishartPvalues <- function(cube, dim=c("cols", "rows")){
    checkInstalledPackages("rootWishart")
    X2 <- wishartMatrix(cube, dim)
    a <- as.array(cube)
    I <- nrow(a)
    J <- ncol(a)
    p <- max(c(I,J)) - 1
    n <- min(c(I,J)) - 1
    upper.tail <- function(x) 1.0 - rootWishart::singleWishart(x, p, n, type = "double")
    apply(X2, c(1,2), upper.tail)    
}
