compareSubvariables <- function (A, B) {
    ## Match variable catalogs on alias
    aa <- aliases(A)
    ab <- aliases(B)
    allaliases <- c(aa, setdiff(ab, aa))
    match.a <- match(allaliases, aa)
    match.b <- match(allaliases, ab)
    return(structure(data.frame(
        name.A=names(A)[match.a],
        parent.A=getIndexSlot(A, "parent")[match.a],
        alias=allaliases,
        parent.B=getIndexSlot(B, "parent")[match.b],
        name.B=names(B)[match.b],
        stringsAsFactors=FALSE),
        class=c("compareSubvariables", "data.frame")))
}

summarizeCompareSubvariables <- function (compdf) {
    ## Check for name matches that aren't matched by alias
    ## Check for multiple parents too
    return(structure(list(
        subvariables=compdf,
        problems=list(
            mismatched.name=findMismatches(compdf, "alias", "name"),
            parents=list(
                A=unique(na.omit(compdf$parent.A)),
                B=unique(na.omit(compdf$parent.B))
            )
        )
    ), class="compareSubvariablesSummary"))
}

##' @export
summary.compareSubvariables <- function (object, ...) summarizeCompareSubvariables(object)

##' @export
print.compareSubvariablesSummary <- function (x, ...) {
    cat("Total subvariables:", nrow(x$subvariables), "\n")
    mismatched.name <- x$problems$mismatched.name
    if (length(mismatched.name)) {
        cat("\n")
        cat("Mismatched names:", length(mismatched.name), "\n")
        bad <- x$subvariables$alias %in% mismatched.name
        print(x$subvariables[bad, c("name.A", "alias", "name.B"), drop=FALSE],
            row.names=FALSE)
    } else {
        cat("All good :)\n")
    }
    ## TODO: also report on >1 parents
    invisible(x)
}
