#' Calculate z-scores for a CrunchCube
#'
#' @param table A CrunchCube to calculate z-scores for
#' @param margin which margin to test against (1 for rows, 2 for columns)
#'
#' @return an array of z-scores
#' @export
z.table <- function (table, margin) {
    other_margin <- 3 - margin ## Assumes 2-D
    # dims <- dim(sample_prop)
    sample_prop <- prop.table(table, margin)
    off_margin <- prop.table(margin.table(table, other_margin)) # doug's C (for rows)

    # adjust the denom only (for empty cells)
    adjusted_table <- bases(table, c(1, 2)) + 1
    n <- margin.table(adjusted_table)
    sample_prop_adj <- prop.table(adjusted_table, margin)

    direct_margin <- prop.table(margin.table(adjusted_table, margin)) # doug's R (for rows)
    if (margin == 1) {
        expected_value <- direct_margin %*% (sample_prop_adj * (1 - sample_prop_adj)) # doug's ev.r (for rows)
    } else if (margin == 2) {
        expected_value <- (sample_prop_adj * (1 - sample_prop_adj)) %*% direct_margin # doug's ev.r (for rows)
    }
    magic_d <- (1 - 2 * direct_margin) / direct_margin # d.r

    ### denominator prep
    # use unweighted counts
    # TODO: make less for-y
    std_err <- matrix(nrow = nrow(adjusted_table), ncol = ncol(adjusted_table))
    for(i in 1: nrow(adjusted_table)) {
        for(j in 1: ncol(adjusted_table)) {
            # swap expected value indicator
            ev_ind <- ifelse(margin == 1, j, i)
            md_ind <- ifelse(margin == 1, i, j)

            std_err[i, j] <- magic_d[md_ind] * sample_prop_adj[i, j] * (1 - sample_prop_adj[i, j]) + expected_value[ev_ind]
        }
    }
    zed_denom <- sqrt(std_err / n)

    ### numerator prep
    if (margin == 1) {
        zed_num <- (sample_prop - matrix(rep(off_margin, nrow(adjusted_table)),
                                         byrow = TRUE, nrow = nrow(adjusted_table)))
    } else if (margin == 2) {
        zed_num <- (sample_prop - matrix(rep(off_margin, ncol(adjusted_table)),
                                         byrow = FALSE, nrow = nrow(adjusted_table)))
    }

    zed_score <- zed_num/zed_denom

    return(zed_score)
}