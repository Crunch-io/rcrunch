#' Calculate an index table for a CrunchCube
#'
#' Index tables are percentages of percentages. They take the percentage from
#' `prop.table()` and divide that by the proportions of the other margin.
#'
#' `index.table()` is only implemented for 2 dimensional cubes.
#'
#' @param x A CrunchCube to calculate index table for
#' @param margin which margin to index against (1 for rows, 2 for columns)
#'
#' @return an array of percentages indexed to the margin provided
#' @export
index.table <- function (x, margin) {
    if (length(dim(x)) != 2){
        halt("Index tables can only be calculated for 2 dimensional cubes.")
    }
    other_margin <- 3 - margin ## Assumes 2-D

    tab <- prop.table(x, margin)
    marg <- sweep(margin.table(x, other_margin), 1, margin.table(x), "/")

    if (identical(dim(tab), dim(marg))) {
        # if the dimensions are exactly the same, use diret division
        return(tab/marg)
    } else {
        return(sweep(tab, other_margin, marg, "/"))
    }
}