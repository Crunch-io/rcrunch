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
#' Standardized residuals, \code{(observed - expected) / sqrt(V)}, where \code{V} 
#' is the residual cell variance (Agresti, 2007, section 2.4.5). Special care is taken
#' for multiple-response variables which are in effect a series of separate tables where
#' \sQuote{not selected} cells for each item are are hidden.
#' 
#' @param model A CrunchCube representing a contingency table
#'
#' @return an array of standardized residuals or Z-scores from the hypothesis being tested.
#' The default method is that the joint distributions of (weighted) counts are equal to the
#' marginal distributions of the table.
#' 
#' @seealso [stats::chisq.test]
#' @references
#' Agresti, A. (2007)
#' \emph{An Introduction to Categorical Data Analysis, 2nd ed.},
#' New York: John Wiley & Sons. Page 38.
#' @importFrom stats chisq.test
#' @name cube-residuals
#' @aliases rstandard,CrunchCube-method cube-residuals
#' @exportMethod rstandard
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