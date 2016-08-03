## Common functions used throughout the comparison methods

findMismatches <- function (compdf, matched.on, alternate) {
    ## Check for matches on "alternate" that aren't matched by "matched.on"
    alt.A <- paste0(alternate, ".A")
    alt.B <- paste0(alternate, ".B")

    common <- na.omit(intersect(compdf[[alt.A]], compdf[[alt.B]]))
    in.A <- match(compdf[[alt.A]], common)
    in.A[is.na(in.A)] <- -1 ## So that the NAs work out in the != below
    in.B <- match(compdf[[alt.B]], common)
    in.B[is.na(in.B)] <- -1
    mismatched <- as.character(na.omit(compdf[[matched.on]][in.A != in.B]))
    return(mismatched)
}
