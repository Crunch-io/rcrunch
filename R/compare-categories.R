compareCategories <- function (A, B) {
    ## Assumes matching on names (because that's what append does)
    na <- names(A)
    nb <- names(B)
    allnames <- c(na, setdiff(nb, na))
    match.a <- match(allnames, na)
    match.b <- match(allnames, nb)
    return(structure(data.frame(
        numeric_value.A=values(A)[match.a],
        id.A=ids(A)[match.a],
        name=allnames,
        id.B=ids(B)[match.b],
        numeric_value.B=values(B)[match.b],
        stringsAsFactors=FALSE),
        class=c("compareCategories", "data.frame")))
}

summarizeCompareCategories <- function (catdf) {
    ## Check for mismatch of ids where names match
    mismatched.names <- as.character(na.omit(catdf$name[catdf$id.A != catdf$id.B]))
    unmatched.names <- with(catdf, {
        ## Check for unmatched id from one that matches in other
        unmatched.A <- intersect(id.A[is.na(id.B)], na.omit(id.B))
        unmatched.B <- intersect(id.B[is.na(id.A)], na.omit(id.A))
        name[id.A %in% unmatched.A | id.B %in% unmatched.B]
    })

    return(structure(list(
        categories=catdf,
        problems=list(
            mismatched.ids=mismatched.names,
            unmatched.ids=unmatched.names
        )),
        class="compareCategoriesSummary"))
}

summary.compareCategories <- function (object, ...) summarizeCompareCategories(object)

print.compareCategoriesSummary <- function (object, ...) {
    cat("Total categories:", nrow(object$categories), "\n")
    mismatched.ids <- object$problems$mismatched.ids
    if (length(mismatched.ids)) {
        cat("\n")
        cat("Mismatched ids:", length(mismatched.ids), "\n")
        print(object$categories[object$categories$name %in% mismatched.ids,,drop=FALSE],
            row.names=FALSE)
    }
    unmatched.ids <- object$problems$unmatched.ids
    if (length(unmatched.ids)) {
        cat("\n")
        cat("Unmatched ids:", length(unmatched.ids), "\n")
        print(object$categories[object$categories$name %in% unmatched.ids,,drop=FALSE],
            row.names=FALSE)
    }
    if (length(mismatched.ids) + length(unmatched.ids) == 0) {
        cat("All good :)\n")
    }
    invisible(object)
}
