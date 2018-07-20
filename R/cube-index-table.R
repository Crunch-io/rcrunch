#' Calculate an index table for a CrunchCube
#'
#' Index tables are percentages of percentages. They take the percentage from
#' `prop.table(cube, margin)` and, by default, divide that by the proportions of the other margin. The `baseline` argument can be used to provide baseline proportions to compare against.
#'
#' `index.table()` is only implemented for 2 dimensional cubes. If you need to
#' calculate indexes for a higher dimension Cube, please slice the cube first.
#'
#' @param x A CrunchCube to calculate index table for
#' @param margin which margin to index against (1 for rows, 2 for columns)
#' @param baseline an arbitrary set of proportions to compare the table given in `x` to. Useful for comparing two separate cubes. `baseline` must have the same length as the extent of the dimension given in `margin`.
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
#'  #   B 107.1429  85.71429
#'  #   C  93.7500 112.50000
#'  index.table(cube_object, 2)
#'  #    v7
#'  # v4    C   E
#'  #   B 100  80
#'  #   C 100 120
#'  index.table(cube_object, 2, c(0.6, 0.4))
#'  #    v7
#'  # v4          C         E
#'  #   B  83.33333  66.66667
#'  #   C 125.00000 150.00000
#' }
#'
#' @export
index.table <- function (x, margin, baseline) {
    if (length(dim(x)) != 2){
        halt("Index tables can only be calculated for 2 dimensional cubes.")
    }
    other_margin <- 3 - margin ## Assumes 2-D

    # the numerators are the proportions by the margin axis
    tab <- prop.table(x, margin)
    
    # the denominators are the proportions of the uni-variate cube based on the
    # other margin. Because dimSums collapses the dimensions given
    # in margin, we get a uni(variate )cube for the opposite margin. Only do
    # this if an explicit baseline wasn't specified.
    if (missing(baseline)) {
        unicube <- dimSums(x, margin)
        baseline <- prop.table(unicube)
    }

    if (identical(dim(tab), dim(baseline))) {
        # if the dimensions are the same, use direct division, needed for MRs
        return(tab/baseline * 100)
    } else {
        return(sweep(tab, other_margin, baseline, "/") * 100)
    }
}