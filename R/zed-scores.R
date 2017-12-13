#' Calculate z-scores for a CrunchCube
#'
#' Calculate z-scores for each cell of a cube by comparing to the proportions on
#' the row or column margins. When testing against row margins `z.table`
#' compares each cell against the proportion for all columns in that row. When
#' testing against column margins `z.table` compares each cell against the
#' proportion for all rows in that  column.
#'
#' @param cube A CrunchCube to calculate z-scores for
#' @param margin which margin to test against (1 for rows, 2 for columns)
#'
#' @return an array of z-scores
#' @export
z.scores <- function (cube, margin) {
    # Multinomial covariance z-score calculations from Doug's tables pdf. This
    # will soon be deprecated and replaced using the cross product of the
    # margins for expected values.
    if (!inherits(cube, "CrunchCube")) {
        halt(dQuote(substitute(cube)), " must be a CrunchCube.")
    }
    all_margins <- seq_along(dim(cube))
    other_margins <- setdiff(all_margins, margin)

    sample_prop <- prop.table(cube, margin)
    # doug's C (for rows)
    off_margin <- prop.table(margin.table(cube, other_margins))

    # adjusted (for empty cells), unweighted counts for the denom
    adjusted_table <- bases(cube, all_margins) + 1
    n <- margin.table(adjusted_table)
    sample_prop_adj <- prop.table(adjusted_table, margin)

    # doug's R (for rows)
    direct_margin <- prop.table(margin.table(adjusted_table, margin))
    expected_value <- apply(sample_prop_adj, other_margins, function(samp_prop) {
        sum((samp_prop * (1 - samp_prop))*direct_margin)
    })

    # if (margin == 1) {
    #     # doug's ev.r (for rows)
    #     expected_value <- direct_margin %*% (sample_prop_adj * (1 - sample_prop_adj))
    # } else if (margin == 2) {
    #     # doug's ev.r (for cols)
    #     expected_value <- (sample_prop_adj * (1 - sample_prop_adj)) %*% direct_margin
    # }

    ### denominator prep
    # use unweighted counts
    magic_d <- (1 - 2 * direct_margin) / direct_margin # d.r
    const <- sweep(sample_prop_adj, margin, magic_d, FUN = "*") * (1 - sample_prop_adj)
    std_err <-  sweep(const, other_margins, expected_value, FUN = "+")
    zed_denom <- sqrt(std_err / n)

    ### numerator prep
    # subtract off_margin from sample_prop, don't check margin because MRs can
    # have an array and not just a vector for off_margin
    zed_num <- sweep(sample_prop, other_margins, off_margin, check.margin = FALSE)

    zed_score <- zed_num/zed_denom

    return(zed_score)
}

#' Calculate p-values for a CrunchCube
#'
#' @param cube A CrunchCube to calculate z-scores for
#' @param margin which margin to test against (1 for rows, 2 for columns)
#'
#' @return an array of p-values
#' @export
#' @importFrom stats pnorm
p.values <- function (cube, margin) {
    zeds <- z.scores(cube, margin)
    return(2*pnorm(abs(zeds), lower.tail = FALSE))
}
