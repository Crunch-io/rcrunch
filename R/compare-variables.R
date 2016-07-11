compareVariables <- function (A, B) {
    ## Match variable catalogs on alias
    aa <- aliases(A)
    ab <- aliases(B)
    allaliases <- c(aa, setdiff(ab, aa))
    match.a <- match(allaliases, aa)
    match.b <- match(allaliases, ab)

    out <- structure(data.frame(
        name.A=names(A)[match.a],
        type.A=types(A)[match.a],
        alias=allaliases,
        type.B=types(B)[match.b],
        name.B=names(B)[match.b],
        stringsAsFactors=FALSE),
        class=c("compareVariables", "data.frame"))
    return(out)
}

summarizeCompareVariables <- function (compdf) {
    ## Check for mismatch of types where aliases match
    mismatched.type <- as.character(na.omit(compdf$alias[compdf$type.A != compdf$type.B]))
    ## Check for name matches that aren't matched by alias
    common.names <- na.omit(intersect(compdf$name.A, compdf$name.B))
    in.A <- match(compdf$name.A, common.names)
    in.A[is.na(in.A)] <- -1
    in.B <- match(compdf$name.B, common.names)
    in.B[is.na(in.B)] <- -1
    mismatched.name <- as.character(na.omit(compdf$alias[in.A != in.B]))

    return(structure(list(
        variables=compdf,
        problems=list(
            mismatched.type=mismatched.type,
            mismatched.name=mismatched.name
        )),
        class="compareVariablesSummary"))
}

summary.compareVariables <- function (object, ...) summarizeCompareVariables(object)

print.compareVariablesSummary <- function (object, ...) {
    cat("Total variables:", nrow(object$variables), "\n")
    mismatched.type <- object$problems$mismatched.type
    if (length(mismatched.type)) {
        cat("\n")
        cat("Type mismatch:", length(mismatched.type), "\n")
        print(object$variables[object$variables$alias %in% mismatched.type,,drop=FALSE],
            row.names=FALSE)
    }
    mismatched.name <- object$problems$mismatched.name
    if (length(mismatched.name)) {
        cat("\n")
        cat("Name mismatch:", length(mismatched.name), "\n")
        print(object$variables[object$variables$alias %in% mismatched.name,,drop=FALSE],
            row.names=FALSE)
    }
    if (length(mismatched.type) + length(mismatched.name) == 0) {
        cat("All good :)\n")
    }
    invisible(object)
}
