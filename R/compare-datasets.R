compareDatasets <- function (A, B) {
    varsA <- variableMetadata(A)
    varsB <- variableMetadata(B)

    ## Create alias to url maps for lookup below
    a2uA <- structure(urls(varsA), .Names=aliases(varsA))
    a2uB <- structure(urls(varsB), .Names=aliases(varsB))

    comp.vars <- compareVariables(varsA[are.subvars(varsA)],
        varsB[are.subvars(varsB)])

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
    return(list(
        variables=comp.vars,
        categories=sapply(intersect.vars$alias[has.categories],
            function (x) {
                compareCategories(Categories(data=varsA[[a2uA[x]]]$categories),
                    Categories(data=varsB[[a2ub[x]]]$categories))
            },
            simplify=FALSE),
        subvariables=sapply(intersect.vars$alias[arrays],
            function (x) {
                compareSubvariables(varsA[varsA[[a2uA[x]]]$subvariables],
                    varsB[varsB[[a2uB[x]]]$subvariables])
            },
            simplify=FALSE)
    ))
}

summarizeCompareDatasets <- function (comp) {
    ## summarize the summaries of each
    cats <- lapply(comp$categories, summarizeCompareCategories)
    ok.cats <- vapply(cats,
        function (x) length(x$mismatched.ids) + length(x$unmatched.ids) == 0,
        logical(1))
    subs <- lapply(comp$subvariables, summarizeCompareSubvariables)
    ok.subs <- vapply(subs,
        function (x) {
            length(x$mismatched.name) == 0 &
            length(x$parents$A) <= 1 &
            length(x$parents$B) <= 1
        },
        logical(1))
    vars <- summarizeCompareVariables(comp$variables)
    ## Do stuff with that

}
