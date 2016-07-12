compareDatasets <- function (A, B) {
    varsA <- variableMetadata(A, parent=TRUE)
    varsB <- variableMetadata(B, parent=TRUE)

    ## Create alias to url maps for lookup below
    a2uA <- structure(urls(varsA), .Names=aliases(varsA))
    a2uB <- structure(urls(varsB), .Names=aliases(varsB))

    comp.vars <- compareVariables(varsA[!are.subvars(varsA)],
        varsB[!are.subvars(varsB)])

    same.type <- comp.vars$type.A == comp.vars$type.B ## is NA if either is NA, i.e. not found
    ## How to address CA vs MR not a problem?
    vars.in.both <- same.type & !is.na(same.type)
    intersect.vars <- comp.vars[vars.in.both,]

    arrays <- intersect.vars$type.A %in% c("categorical_array", "multiple_response")
    has.categories <- intersect.vars$type.A %in% c("categorical_array", "multiple_response", "categorical")

    ## TODO:
    ## 2c) tests for variableMetadata shape
    ## 3) analysis
    ## 4) tests for the compare functions
    ## 5) print/summary methods for comparison results (as S3)
    ## 6) export metadata? use same shape here (bc keyed by alias)?
    return(structure(list(
        variables=comp.vars,
        categories=sapply(intersect.vars$alias[has.categories],
            function (x) {
                compareCategories(Categories(data=varsA[[a2uA[x]]]$categories),
                    Categories(data=varsB[[a2uB[x]]]$categories))
            },
            simplify=FALSE),
        subvariables=sapply(intersect.vars$alias[arrays],
            function (x) {
                compareSubvariables(varsA[varsA[[a2uA[x]]]$subvariables],
                    varsB[varsB[[a2uB[x]]]$subvariables])
            },
            simplify=FALSE)
        ),
        class="compareDatasets"
    ))
}

summarizeCompareDatasets <- function (comp) {
    ## summarize the summaries of each
    cats <- lapply(comp$categories, summarizeCompareCategories)
    ok.cats <- vapply(cats,
        function (x) length(x$problems$mismatched.ids) == 0,
        logical(1))
    subs <- lapply(comp$subvariables, summarizeCompareSubvariables)
    ok.subs <- vapply(subs,
        function (x) {
            p <- x$problems
            return(length(p$mismatched.name) == 0 &
                    length(p$parents$A) <= 1 &
                    length(p$parents$B) <= 1)
        },
        logical(1))
    vars <- summarizeCompareVariables(comp$variables)
    return(structure(list(cats=cats, ok.cats=ok.cats, subs=subs,
        ok.subs=ok.subs, vars=vars),
        class="compareDatasetsSummary"))
}

summary.compareDatasets <- function (object, ...) summarizeCompareDatasets(object)

print.compareDatasetsSummary <- function (object, ...) {
    ## Variables
    bad.var.count <- length(object$vars$problems$mismatched.type) +
        length(object$vars$problems$mismatched.name)
    print(object$vars)

    ## Categories
    bad.cats <- !object$ok.cats
    bad.cat.count <- sum(bad.cats)
    cat("\nVariables with categories:", length(object$cats), "\n")
    if (bad.cat.count) {
        cat("With issues:", bad.cat.count, "\n\n")
        for (i in names(object$cats[bad.cats])) {
            cat("$", i, "\n", sep="")
            print(object$cats[[i]])
            cat("\n")
        }
    }

    ## Subvariables
    bad.subs <- !object$ok.subs
    bad.sub.count <- sum(bad.subs)
    cat("\nArray variables:", length(object$subs), "\n")
    if (bad.sub.count) {
        cat("With subvariable issues:", bad.sub.count, "\n\n")
        for (i in names(object$subs[bad.subs])) {
            cat("$", i, "\n", sep="")
            print(object$subs[[i]])
            cat("\n")
        }
    }

    ## Else:
    if (bad.var.count + bad.cat.count + bad.sub.count == 0) {
        cat("All good :)\n")
    }
    invisible(object)
}
