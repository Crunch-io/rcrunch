#' Calculate z-scores for a CrunchCube
#'
#' To get a z-score for the cells of a cube by comparing to the proportions on
#' the row or column margins. When testing against row margins `z.table`
#' compares each cell against the proportion for all columns in that row. When
#' testing against column margins `z.table` compares each cell against the
#' proportion for all rows in that  column.
#'
#' @param table A CrunchCube to calculate z-scores for
#' @param margin which margin to test against (1 for rows, 2 for columns)
#'
#' @return an array of z-scores
#' @export
z.scores <- function (table, margin) {
    # z-score calculations from Doug's tables pdf. This will soon be deprecated
    # and replaced using the cross product of the margins for expected values.
    other_margin <- 3 - margin ## Assumes 2-D

    sample_prop <- prop.table(table, margin)
    # doug's C (for rows)
    off_margin <- prop.table(margin.table(table, other_margin))

    # adjusted (for empty cells), unweighted counts for the denom
    adjusted_table <- bases(table, c(1, 2)) + 1
    n <- margin.table(adjusted_table)
    sample_prop_adj <- prop.table(adjusted_table, margin)

    # doug's R (for rows)
    direct_margin <- prop.table(margin.table(adjusted_table, margin))
    if (margin == 1) {
        # doug's ev.r (for rows)
        expected_value <- direct_margin %*% (sample_prop_adj * (1 - sample_prop_adj))
    } else if (margin == 2) {
        # doug's ev.r (for cols)
        expected_value <- (sample_prop_adj * (1 - sample_prop_adj)) %*% direct_margin
    }

    ### denominator prep
    # use unweighted counts
    magic_d <- (1 - 2 * direct_margin) / direct_margin # d.r
    const <- sweep(sample_prop_adj, margin, magic_d, FUN = "*") * (1 - sample_prop_adj)
    std_err <-  sweep(const, other_margin, expected_value, FUN = "+")
    zed_denom <- sqrt(std_err / n)

    ### numerator prep
    # subtract off_margin from sample_prop
    zed_num <- sweep(sample_prop, other_margin, off_margin)

    zed_score <- zed_num/zed_denom

    return(zed_score)
}

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
