#' Combine categories or responses
#'
#' Create a new categorical or multiple-response variable by collapsing
#' the categories/responses of another. For instance, you might want to
#' recode a categorical variable with three categories small, medium, and large
#' to one that has just small and large.
#'
#' @param variable Categorical, Categorical Array, or Multiple Response
#' variable
#' @param combinations list of named lists containing
#' 1. "categories": category ids or names for categorical types, or for multiple
#' response, "responses": subvariable names, aliases, or positional indices;
#' 1. a "name" for the new category or response; and
#' 1. optionally, other category ("missing", "numeric_value") or subvariable
#' ("alias", "description") attributes. If `combinations` is omitted, the
#' resulting variable will essentially be a copy (but see [copy()] for a more
#' natural way to copy variables.
#' @param ... Additional variable metadata for the new derived variable
#' @param replace Logical: should this derived variable, when added to the
#' dataset, take the place of the input `variable`, i.e. same folder/location?
#' If so, `variable` will also become "hidden".
#' @return A [`VariableDefinition`] that will create the new combined-category
#' or -response derived variable. Categories/responses not referenced in
#' `combinations` will be appended to the end of the combinations.
#' @examples
#' \dontrun{
#' ds$fav_pet2 <- combine(ds$fav_pet, name="Pets (combined)",
#'     combinations=list(list(name="Mammals", categories=c("Cat", "Dog")),
#'                       list(name="Reptiles", categories=c("Snake", "Lizard"))))
#' ds$pets_owned2 <- combine(ds$allpets, name="Pets owned (collapsed)",
#'     combinations=list(list(name="Mammals", responses=c("Cat", "Dog"))))
#' }
#' @export
#' @importFrom utils modifyList
combine <- function (variable, combinations=list(), ..., replace=FALSE) {
    ## Validate inputs
    if (!(type(variable) %in% c("categorical", "categorical_array", "multiple_response"))) {
        halt("Cannot combine ", dQuote(name(variable)), ": must be type ",
            "categorical, categorical_array, or multiple_response")
    }
    if (!is.list(combinations) || !all(vapply(combinations, is.list, logical(1)))) {
        halt("'combinations' must be a list of combination specifications")
    }

    ## Get basic variable metadata
    newvar <- copyVariableReferences(variable)
    newvar$alias <- NULL ## Let server specify, or specify in ..., or on <-
    newvar <- modifyList(newvar, list(...))
    newvar$type <- NULL ## Type is function of the derivation

    ## Construct expr
    if (type(variable) == "multiple_response") {
        combs <- combResps(subvariables(variable), combinations)
        newvar$derivation <- zfunc("combine_responses",
            zcl(variable), list(value=combs))
        ## Give default name based on number of responses
        if (identical(newvar$name, name(variable))) {
            nvalidresps <- length(newvar$derivation$args[[2]]$value)
            newvar$name <- paste0(newvar$name, " (", nvalidresps,
                ifelse(nvalidresps == 1, " response)", " responses)"))
        }
    } else {
        combs <- combCats(categories(variable), combinations)
        newvar$derivation <- zfunc("combine_categories",
            zcl(variable), list(value=combs))
        ## Give default name based on number of categories
        if (identical(newvar$name, name(variable))) {
            nvalidcats <- length(Filter(Negate(function (x) isTRUE(x$missing)),
                newvar$derivation$args[[2]]$value))
            newvar$name <- paste0(newvar$name, " (", nvalidcats,
                ifelse(nvalidcats == 1, " category)", " categories)"))
        }
    }

    if (replace) {
        attr(newvar, ".hide") <- self(variable)
    }
    class(newvar) <- "VariableDefinition"
    return(newvar)
}

combCats <- function (cats, combs) {
    ## Validate combinations
    if (!all(vapply(combs,
        function (x) all(c("name", "categories") %in% names(x)),
        logical(1)))) {

        halt("'combinations' must be a list of combination specifications. ",
            "See '?combine'.")
    }

    defaultCat <- list(missing=FALSE, numeric_value=NULL)
    ## Convert category names to ids
    ## Update each comb with default
    combs <- lapply(combs, function (x) {
        if (is.character(x$categories)) {
            x$categories <- n2i(x$categories, cats)
        }
        if (!is.numeric(x$categories)) {
            halt("Combinations must reference 'categories' by name or id")
        }
        x$combined_ids <- I(x$categories)
        x$categories <- NULL
        return(modifyList(defaultCat, x))
    })

    ## Validate that they're all unique and nonmissing
    idsToCombine <- unlist(lapply(combs, vget("combined_ids")))
    badids <- setdiff(idsToCombine, ids(cats))
    if (length(badids)) {
        badnames <- vapply(
            Filter(function (x) any(x$combined_ids %in% badids), combs),
            vget("name"),
            character(1))
        halt(ifelse(length(badnames) == 1, "Combination ", "Combinations "),
            serialPaste(dQuote(badnames)),
            ifelse(length(badnames) == 1, " references", " reference"),
            ifelse(length(badids) == 1,
                " category with id ", " categories with ids "),
            serialPaste(badids),
            ifelse(length(badids) == 1,
                ", which does not exist", ", which do not exist"))
    }

    dupids <- duplicated(idsToCombine)
    if (any(dupids)) {
        dupnames <- i2n(idsToCombine[dupids], cats)
        halt(ifelse(length(dupnames) == 1, "Category ", "Categories "),
            serialPaste(dQuote(dupnames)),
            " referenced in multiple combinations")
    }

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

    ## One more validation
    newnames <- vapply(combs, vget("name"), character(1))
    dupnames <- duplicated(newnames)
    if (any(dupnames)) {
        halt("Duplicate category name given: ",
            serialPaste(dQuote(unique(newnames[dupnames]))))
    }
    return(combs)
}

combResps <- function (subvars, combs) {
    ## Validate combinations
    if (!all(vapply(combs,
        function (x) all(c("name", "responses") %in% names(x)),
        logical(1)))) {

        halt("'combinations' must be a list of combination specifications. ",
            "See '?combine'.")
    }

    ## Convert response names/aliases to urls
    subnames <- names(subvars)
    subaliases <- aliases(subvars)
    suburls <- urls(subvars)
    combs <- lapply(combs, function (x) {
        if (!is.character(x$responses)) {
            halt("Combinations must reference 'responses' by name or alias")
        }
        matches <- match(x$responses, subnames)
        if (any(is.na(matches))) {
            ## Try aliases instead
            matches <- match(x$responses, subaliases)
        }
        if (any(is.na(matches))) {
            halt("Response ", dQuote(x$name),
                " does not reference valid subvariables")
        }
        x$combined_ids <- I(suburls[matches])
        x$responses <- NULL
        return(x)
    })

    ## Append unreferenced subvars to end
    subvarsToCombine <- unlist(lapply(combs, vget("combined_ids")))
    oldSubvars <- lapply(setdiff(suburls, subvarsToCombine),
        function (u) {
            return(list(name=index(subvars)[[u]]$name,
                combined_ids=I(u)))
        })
    combs <- c(combs, oldSubvars)

    ## One more validation
    newnames <- vapply(combs, vget("name"), character(1))
    dupnames <- duplicated(newnames)
    if (any(dupnames)) {
        halt("Duplicate response name given: ",
            serialPaste(dQuote(unique(newnames[dupnames]))))
    }

    return(combs)
}


#' Combine Categories in place
#'
#' This function allows you to combine the categories of a variable without
#' making a copy of the variable.
#' @param var A categorical Crunch variable
#' @param from A character vector of categories you want to combine.
#' @param to A character string with the destination category.
#' @return the variable duly modified
#' @export
#' @seealso [combine()]
collapseCategories <- function (var, from, to) {
    if (!is.Categorical(var)) {
        halt("Variable must be a categorical.")
    }
    if (!(length(to) == 1 && is.character(to))) {
        halt("Destination category must be a character string of length 1.")
    }
    if (!(is.character(from))) {
        halt(dQuote('from'), " must be a character vector.")
    }
    if (identical(from, to)) {
        # If the user is collapsing categories into itself, no changes are
        # neccesary so the variable is returned.
        return(var)
    }
    cats <- names(categories(var))
    missing_origin <- !(from %in% cats)
    if (any(missing_origin)) {
        err <- ifelse(sum(missing_origin) > 1, " are ", " is ")
        halt(serialPaste(from[missing_origin]),
            err, "not present in variable categories.")
    }
    if (!(to %in% cats)) {
        if (length(from) == 1) {
            #this case is equivalent to renaming a category
            names <- names(categories(var))
            names[names == from] <- to
            names(categories(var)) <- names
            return(var)
        }
        cats <- c(cats, to)
        categories(var) <- c(categories(var),
            Category(id = max(ids(categories(var))) + 1,
            name = to)
        )
    }
    from <- setdiff(from, to) #in case the user tries to collapse a category into itself
    var[var %in% from] <- to
    categories(var) <- categories(var)[!(cats %in% from)]
    return(var)
}
