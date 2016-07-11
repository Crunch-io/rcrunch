compareSubvariables <- function (A, B) {
    ## Match variable catalogs on alias
    aa <- aliases(A)
    ab <- aliases(B)
    allaliases <- c(aa, setdiff(ab, aa))
    match.a <- match(allaliases, aa)
    match.b <- match(allaliases, ab)
    return(data.frame(
        name.A=names(A)[match.a],
        parent.A=vapply(A[match.a], vget("parent"), character(1), USE.NAMES=FALSE),
        alias=allaliases,
        parent.B=vapply(B[match.b], vget("parent"), character(1), USE.NAMES=FALSE),
        name.B=names(B)[match.b],
        stringsAsFactors=FALSE))
}

summarizeCompareSubariables <- function (compdf) {
    ## Check for name matches that aren't matched by alias
    ## Check for multiple parents too
    return(list(
        mismatched.name=findMismatches(compdf, "alias", "name"),
        parents=list(
            A=unique(na.omit(compdf$parent.A)),
            B=unique(na.omit(compdf$parent.B))
        )
    ))
}
