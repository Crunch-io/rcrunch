compareSubvariables <- function (A, B) {
    ## Match variable catalogs on alias
    aa <- aliases(A)
    ab <- aliases(B)
    allaliases <- c(aa, setdiff(ab, aa))
    match.a <- match(allaliases, aa)
    match.b <- match(allaliases, ab)
    return(structure(data.frame(
        name.A=names(A)[match.a],
        parent.A=getIndexSlot(A, "parent_alias")[match.a],
        alias=allaliases,
        parent.B=getIndexSlot(B, "parent_alias")[match.b],
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

#' @export
summary.compareSubvariables <- function (object, ...) summarizeCompareSubvariables(object)

#' @export
print.compareSubvariablesSummary <- function (x, ...) {
    cat("Total subvariables:", nrow(x$subvariables), "\n")
    allgood <- TRUE
    mismatched.name <- x$problems$mismatched.name
    if (length(mismatched.name)) {
        allgood <- FALSE
        cat("\n")
        cat("Mismatched names:", length(mismatched.name), "\n")
        bad <- x$subvariables$alias %in% mismatched.name
        print(x$subvariables[bad, c("name.A", "alias", "name.B"), drop=FALSE],
            row.names=FALSE)
    }
    parA <- x$problems$parents$A
    parB <- x$problems$parents$B
    if (length(parA) > 1) {
        allgood <- FALSE
        cat("\nSubvariables in A have multiple parents:",
            serialPaste(parA), "\n")
    }
    if (length(parB) > 1) {
        allgood <- FALSE
        cat("\nSubvariables in B have multiple parents:",
            serialPaste(parB), "\n")
    }
    if (!is.null(x$problems$overlap)) {
        ## Comes from compareDatasets injection
        allgood <- FALSE
        cat("\nContains subvariables found in other arrays after matching:",
            serialPaste(x$problems$overlap), "\n")
    }
    if (allgood) {
        cat("All good :)\n")
    }
    invisible(x)
}

checkSubvariableParents <- function (subvar.comps) {
    ## Starting from the list of compareSubvariables output in compareDatasets,
    ## check whether any matched array references subvariables found in any
    ## other array.
    ## Returns list, keyed by alias, of any array variable that has subvariables
    ## in any other array; list elements contain the aliases of those array
    ## variables. If all are good, the list will be length 0.
    narrays <- length(subvar.comps)
    if (narrays > 1) {
        out <- lapply(seq_len(narrays - 1), function (i) {
            current <- subvar.comps[[i]]$alias
            overlap <- vapply(subvar.comps[(i+1):narrays], function (x) {
                length(intersect(x$alias, current)) > 0
            }, logical(1), USE.NAMES=TRUE)
            return(names(overlap)[overlap])
        })
        names(out) <- names(subvar.comps)[-narrays]
        return(Filter(function (x) length(x) > 0, out))
    } else {
        return(list())
    }
}
