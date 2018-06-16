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
#' }
#'
#' @export
index.table <- function (x, margin, baseline) {
    if (length(dim(x)) != 2){
        halt("Index tables can only be calculated for 2 dimensional cubes.")
    }

    if (missing(baseline)) {
        unicube <- collapse.dimensions(x, margin)
        baseline <- prop.table(unicube)
    }

    tab <- prop.table(x, margin)

    return(tab/broadcast(baseline, dim(tab)) * 100)
}