##' Combine categories or responses
##'
##' @param variable Categorical, Categorical Array, or Multiple Response
##' variable
##' @param combinations list of named lists containing (1) "categories":
##' category ids or names for categorical types, or for multiple response,
##' "responses": subvariable names, aliases, or positional indices; (2) a
##' "name" for the new category or response; and (3) optionally, other category
##' ("missing", "numeric_value") or subvariable ("alias", "description")
##' attributes
##' @param ... Additional variable metadata for the new derived variable
##' @return A \code{\link{VariableDefinition}} that will create the new
##' comined-category or -response derived variable. Categories/responses not
##' referenced in \code{combinations} will be appended to the end of the
##' combinations.
##' @export
combine <- function (variable, combinations, ...) {
    ## Get basic metadata
    newvar <- copyVariableReferences(variable)
    newvar$alias <- NULL ## Let server specify, or specify in ..., or on <-
    newvar <- updateList(newvar, list(...))
    newvar$type <- NULL ## Type is function of the derivation

    ## Construct expr
    newvar$expr <- zfunc("combine_categories", zcl(variable),
        list(value=combCats(categories(variable), combinations)))
    ## Give default name based on number of categories
    if (identical(newvar$name, name(variable))) {
        newvar$name <- paste0(newvar$name, " (",
            length(newvar$expr$args[[2]]$value), " categories)")
    }
    class(newvar) <- "VariableDefinition"
    return(newvar)
}

combCats <- function (cats, combs) {
    defaultCat <- list(missing=FALSE, numeric_value=NULL)
    ## Convert category names to ids
    ## Update each comb with default
    combs <- lapply(combs, function (x) {
        if (is.character(x$categories)) {
            x$categories <- n2i(x$categories, cats)
        }
        x$combined_ids <- I(x$categories)
        x$categories <- NULL
        return(updateList(defaultCat, x))
    })

    ## Validate that they're all unique and nonmissing
    idsToCombine <- unlist(lapply(combs, function (x) x$combined_ids))
    stopifnot(all(idsToCombine %in% ids(cats)))
    stopifnot(!anyDuplicated(idsToCombine))
    stopifnot(!any(is.na(idsToCombine)))

    ## Give valid ids to new combinations
    usedIds <- setdiff(ids(cats), idsToCombine)
    newIds <- setdiff(seq_len(length(usedIds) + length(combs)),
        usedIds)[seq_len(length(combs))]
    combs <- mapply(function (comb, i) {
        comb$id <- i
        return(comb)
    }, combs, newIds, SIMPLIFY=FALSE, USE.NAMES=FALSE)

    ## Append unreferenced cats to end
    oldCats <- lapply(cats@.Data[ids(cats) %in% usedIds], function (x) {
        x$combined_ids <- I(id(x))
        return(x)
    })
    combs <- c(combs, oldCats)
    return(combs)
}
