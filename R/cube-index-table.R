#' Calculate an index table for a CrunchCube
#'
#' Index tables are percentages of percentages. They take the percentage from
#' `prop.table()` and divide that by the proportions of the other margin.
#'
#' `index.table()` is only implemented for 2 dimensional cubes. If you need to
#' calculate indexes for a higher dimension Cube, please slice the cube first.
#'
#' @param x A CrunchCube to calculate index table for
#' @param margin which margin to index against (1 for rows, 2 for columns)
#'
#' @return an array of percentages indexed to the margin provided
#'
#' @examples
#' \dontrun{
#'  cube_object
#'  #    v7
#'  # v4  C E
#'  #   B 5 2
#'  #   C 5 3
#'  index.table(cube_object, 1)
#'  #    v7
#'  # v4         C         E
#'  #   B 1.071429 0.8571429
#'  #   C 0.937500 1.1250000
#'  index.table(cube_object, 2)
#'  #    v7
#'  # v4         C         E
#'  #   B 1.071429 0.8571429
#'  #   C 0.937500 1.1250000
#'  #
#' }
#'
#' @export
index.table <- function(x, margin) {
    if (length(dim(x)) != 2) {
        halt("Index tables can only be calculated for 2 dimensional cubes.")
    }
    other_margin <- 3 - margin ## Assumes 2-D

    tab <- prop.table(x, margin)
    marg <- sweep(margin.table(x, other_margin), 1, margin.table(x), "/")

    if (identical(dim(tab), dim(marg))) {
        # if the dimensions are the same, use direct division, needed for MRs
        return(tab / marg * 100)
    } else {
        return(sweep(tab, other_margin, marg, "/") * 100)
    }
}
