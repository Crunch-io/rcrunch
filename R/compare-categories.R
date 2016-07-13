compareCategories <- function (A, B) {
    ## Assumes matching on names (because that's what append does)
    na <- names(A)
    nb <- names(B)
    allnames <- c(na, setdiff(nb, na))
    match.a <- match(allnames, na)
    match.b <- match(allnames, nb)
    return(structure(data.frame(
        # numeric_value.A=values(A)[match.a],
        id.A=ids(A)[match.a],
        name=allnames,
        id.B=ids(B)[match.b],
        # numeric_value.B=values(B)[match.b],
        stringsAsFactors=FALSE),
        class=c("compareCategories", "data.frame")))
}

summarizeCompareCategories <- function (compdf) {
    ## Check for id matches that aren't matched by name
    return(structure(list(
        categories=compdf,
        problems=list(
            mismatched.ids=findMismatches(compdf, "name", "id")
        )),
        class="compareCategoriesSummary"))
}

##' @export
summary.compareCategories <- function (object, ...) summarizeCompareCategories(object)

##' @export
print.compareCategoriesSummary <- function (object, ...) {
    cat("Total categories:", nrow(object$categories), "\n")
    mismatched.ids <- object$problems$mismatched.ids
    if (length(mismatched.ids)) {
        cat("\n")
        cat("Mismatched ids:", length(mismatched.ids), "\n")
        print(object$categories[object$categories$name %in% mismatched.ids,,drop=FALSE],
            row.names=FALSE)
    } else {
        cat("All good :)\n")
    }
    invisible(object)
}
