# z-score calculations from Doug's tables pdf. This will soon be deprecated and
# replaced using the cross product of the margins for expected values.
zScoresDep <- function (table, margin) {
    other_margin <- 3 - margin ## Assumes 2-D
    # dims <- dim(sample_prop)
    sample_prop <- prop.table(table, margin)
    # doug's C (for rows)
    off_margin <- prop.table(margin.table(table, other_margin))

    # adjust the denom only (for empty cells)
    adjusted_table <- bases(table, c(1, 2)) + 1
    n <- margin.table(adjusted_table)
    sample_prop_adj <- prop.table(adjusted_table, margin)

    # doug's R (for rows)
    direct_margin <- prop.table(margin.table(adjusted_table, margin))
    magic_d <- (1 - 2 * direct_margin) / direct_margin # d.r
    if (margin == 1) {
        # doug's ev.r (for rows)
        expected_value <- direct_margin %*% (sample_prop_adj * (1 - sample_prop_adj))
        # make arrays
        expected_value <- broadcast(expected_value, nrow = nrow(sample_prop_adj))
        magic_d <- broadcast(magic_d, ncol = ncol(sample_prop_adj))
    } else if (margin == 2) {
        # doug's ev.r (for cols)
        expected_value <- (sample_prop_adj * (1 - sample_prop_adj)) %*% direct_margin
        # make arrays
        expected_value <- broadcast(expected_value, ncol = ncol(sample_prop_adj))
        magic_d <- broadcast(magic_d, nrow = nrow(sample_prop_adj))
    }

    ### denominator prep
    # use unweighted counts
    std_err <- magic_d * sample_prop_adj * (1 - sample_prop_adj) + expected_value
    zed_denom <- sqrt(std_err / n)

    ### numerator prep
    if (margin == 1) {
        zed_num <- sample_prop - broadcast(off_margin, nrow = nrow(adjusted_table))
    } else if (margin == 2) {
        zed_num <- sample_prop - broadcast(off_margin, ncol = ncol(adjusted_table))
    }

    zed_score <- zed_num/zed_denom

    return(zed_score)
}

#' Calculate z-scores for a CrunchCube
#'
#' To get a z-score for the cells of a cube by comparing to the proportions on
#' the row or column margin. When testing against row marigns compare the cell
#' against the proprotion for all columns in that row. When testing against
#' column marigns compare the cell against the proprotion for all rows in that
#' column.
#'
#' @param table A CrunchCube to calculate z-scores for
#' @param margin which margin to test against (1 for rows, 2 for columns)
#'
#' @return an array of z-scores
#' @export
z.table <- zScoresDep

#' Calculate p-values for a CrunchCube
#'
#' @param table A CrunchCube to calculate z-scores for
#' @param margin which margin to test against (1 for rows, 2 for columns)
#'
#' @return an array of p-values
#' @export
#' @importFrom stats pnorm
p.values <- function (table, margin) {
    zeds <- z.table(table, margin)
    return(2*pnorm(abs(zeds), lower.tail = FALSE))
}

# broadcast a vector of values to a matrix with length(values) cols/rows and
# ncol/nrow cols/rows.
broadcast <- function (values, nrow, ncol) {
    if (missing(nrow)) {
        byrow <- FALSE
        nrow <- length(values)
    } else if (missing(ncol)) {
        byrow <- TRUE
        ncol <- length(values)
    } else {
        return(NULL)
    }
    return(matrix(values, byrow = byrow, ncol = ncol, nrow = nrow))
}
