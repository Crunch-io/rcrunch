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
    if (margin == 1) {
        # doug's ev.r (for rows)
        expected_value <- direct_margin %*% (sample_prop_adj * (1 - sample_prop_adj))
    } else if (margin == 2) {
        # doug's ev.r (for cols)
        expected_value <- (sample_prop_adj * (1 - sample_prop_adj)) %*% direct_margin
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

#' Calculate z-scores for a CrunchCube
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

