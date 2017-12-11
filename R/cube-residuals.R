# standardized residuals formed by the subvariable-wise margins
# the internal chisq tests use rowSums and colSums, but we need
# the more abstract 'margin' (specifically cubeMarginTable) to
# get the right numbers for multiple response.
standardizedMRResiduals <- function(cube, types){
    if(all(types == "multiple_response")){
        n <- margin.table(cube)
        r <- margin.table(cube, 1)
        c <- margin.table(cube, 2)
        E <- r * c / n
        V <- r * c * (n-r) * (n-c) / n^3
    } else {
        MRdim <- which(types=="multiple_response")
        nonMRdim <- 3 - MRdim
        margins <- marginComplement <- vector("list", 2)
        n <- apply(margin.table(cube, nonMRdim), MRdim, sum)
        margins[[MRdim]] <- margin.table(cube, nonMRdim)
        margins[[nonMRdim]] <- margin.table(cube, MRdim)
        marginComplement[[MRdim]] <- sweep(margins[[MRdim]], MRdim, n, '-')
        marginComplement[[nonMRdim]] <- margins[[nonMRdim]] - n
        E <- V <- sweep(margins[[MRdim]], MRdim, margins[[nonMRdim]], '*')
        E <- sweep(E, MRdim, n, '/')
        # Cell variance:  (c * r * (n - r) * (n - c)) / n^3
        V <- V * marginComplement[[MRdim]]
        V <- sweep(V, MRdim, marginComplement[[nonMRdim]], '*')
        V <- sweep(V, MRdim, n^3, '/')
    }
    return((as.array(cube) - E) / sqrt(V))
}

#' Calculate standardized residuals from a CrunchCube
#'
#' To get a z-score for the cells of a cube by comparing to the proportions on
#' the row or column margins. When testing against row margins `z.table`
#' compares each cell against the proportion for all columns in that row. When
#' testing against column margins `z.table` compares each cell against the
#' proportion for all rows in that  column.
#'
#' @param table A CrunchCube to calculate z-scores for
#'
#' @return an array of z-scores for each cell
#' @importFrom stats chisq.test
#' @export
setMethod('rstandard', 'CrunchCube', function(model){
    types <- vapply(dimensions(model), function(dim) dim$references$type, character(1))
    if(any(types == 'multiple_response')){
        if(length(types) > 2){
            stop("Cannot compute residuals with more than two MR dims. Pick a slice to evaluate.")
        }
        return(standardizedMRResiduals(model, types))
    } else {
        return(chisq.test(as.array(model))$stdres)
    }
})