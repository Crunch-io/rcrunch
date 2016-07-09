compareCategories <- function (A, B) {
    ## Assumes matching on names (because that's what append does)
    na <- names(A)
    nb <- names(B)
    allnames <- c(na, setdiff(nb, na))
    match.a <- match(allnames, na)
    match.b <- match(allnames, nb)
    return(data.frame(
        numeric_value.A=values(A)[match.a],
        id.A=ids(A)[match.a],
        name=allnames,
        id.B=ids(B)[match.b],
        numeric_value.B=values(B)[match.b],
        stringsAsFactors=FALSE))
}

compareVariables <- function (A, B) {
    ## Match variable catalogs on alias
    aa <- aliases(A)
    ab <- aliases(B)
    allaliases <- c(na, setdiff(ab, aa))
    match.a <- match(allaliases, aa)
    match.b <- match(allaliases, ab)
    return(data.frame(
        name.A=names(A)[match.a],
        type.A=types(A)[match.a],
        alias=allnames,
        type.B=types(B)[match.b],
        name.B=names(B)[match.b],
        stringsAsFactors=FALSE))
}

compareSubvariables <- function (A, B) {
    out <- compareVariables(A, B)
    out$type.A <- NULL
    out$type.B <- NULL
    return(out)
}

compareDatasets <- function (A, B) {
    varsA <- variableMetadata(A)
    varsB <- variableMetadata(B)

    comp.vars <- compareVariables(varsA, varsB)
    ## How to address unbound subvariables?

    same.type <- comp.vars$type.A == comp.vars$type.B ## is NA if either is NA, i.e. not found
    ## How to address CA vs MR not a problem?
    vars.in.both <- same.type & !is.na(same.type)
    intersect.vars <- comp.vars[vars.in.both,]

    arrays <- intersect.vars$type.A %in% c("categorical_array", "multiple_response")
    has.categories <- intersect.vars$type.A %in% c("categorical_array", "multiple_response", "categorical")

    ## TODO:
    ## 1) need to extract below from varsA and varsB. map alias to URL for each
    ## 2) compareCategories and compareSubvariables need to deal with raw data structures (list)
    ## instead of S4 objects with methods
    ## 2b) subvariables are how exactly in variableMetadata? How should they be? nested or flat?
    ## 2c) tests for variableMetadata shape
    ## 3) analysis
    ## 4) tests for the compare functions
    ## 5) print/summary methods for comparison results (as S3)
    ## 6) export metadata? use same shape here (bc keyed by alias)?
    return(list(
        variables=comp.vars,
        categories=sapply(intersect.vars$alias[has.categories],
            function (x) compareCategories(varsA[[x]]$categories, varsB[[x]]$categories),
            simplify=FALSE),
        subvariables=sapply(intersect.vars$alias[arrays],
            function (x) compareSubvariables(varsA[[x]]$subvariables, varsB[[x]]$subvariables),
            simplify=FALSE)
    ))
}
