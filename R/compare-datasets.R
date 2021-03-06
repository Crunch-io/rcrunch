#' Compare two datasets to see how they will append
#'
#' When one dataset is appended to another, variables and subvariables are
#' matched on their aliases, and then categories for variables that have them
#' are matched on category name. This function lines up the metadata between
#' two datasets as the append operation will so that you can inspect how well
#' the datasets will align before you do the append.
#'
#' Calling \code{summary} on the return of this function will print an
#' overview of places where the matching on variable alias and category name
#' may lead to undesired outcomes, enabling you to alter one or both datasets
#' to result in better alignment.
#' @param A CrunchDataset
#' @param B CrunchDataset
#' @return An object of class 'compareDatasets', a list of three elements: (1)
#' 'variables', a data.frame of variable metadata joined on alias; (2)
#' 'categories', a list of data.frames of category metadata joined on category
#' name, one for each variable with categories; and (3) 'subvariables', a
#' list of data.frames of subvariable metadata joined on alias, one for each
#' array variable.
#'
#' Summary output reports on (1) variables that, when matched across datasets
#' by alias, have different types; (2) variables that have the same name but
#' don't match on alias; (3) for variables that match and have categories,
#' any categories that have the same id but don't match on name; (4) for
#' array variables that match, any subvariables that have the same name but
#' don't match on alias; and (5) array variables that, after assembling the
#' union of their subvariables, point to subvariables that belong to other
#' arrays.
#' @examples
#' \dontrun{
#' comp <- compareDataset(ds1, ds2)
#' summary(comp)
#' }
#' @export
compareDatasets <- function(A, B) {
    varsA <- variableMetadata(A)
    varsB <- variableMetadata(B)

    ## Create alias to url maps for lookup below
    a2uA <- structure(urls(varsA), .Names = aliases(varsA))
    a2uB <- structure(urls(varsB), .Names = aliases(varsB))

    ## Do the same but for the flattened metadata, for the subariables
    fvarsA <- flattenVariableMetadata(varsA)
    fvarsB <- flattenVariableMetadata(varsB)
    a2uFA <- structure(urls(fvarsA), .Names = aliases(fvarsA))
    a2uFB <- structure(urls(fvarsB), .Names = aliases(fvarsB))

    ## Compare.
    comp.vars <- compareVariables(varsA, varsB)

    ## is NA if either is NA, i.e. not found
    same.type <- comp.vars$type.A == comp.vars$type.B
    ## How to address CA vs MR not a problem?
    vars.in.both <- same.type & !is.na(same.type)
    intersect.vars <- comp.vars[vars.in.both, ]

    has.cats <- has.categories(intersect.vars$type.A)
    ## Grab all array variables because we need to check subvars across arrays
    ## even for those that don't match.
    arrays <- comp.vars$type.A %in% c("categorical_array", "multiple_response") |
        comp.vars$type.B %in% c("categorical_array", "multiple_response")

    return(structure(list(
        variables = comp.vars,
        categories = sapply(intersect.vars$alias[has.cats],
            function(x) {
                compareCategories(
                    Categories(data = varsA[[a2uA[x]]]$categories),
                    Categories(data = varsB[[a2uB[x]]]$categories)
                )
            },
            simplify = FALSE
        ),
        subvariables = sapply(comp.vars$alias[arrays],
            function(x) {
                ## Pull together the union of aliases
                if (x %in% names(a2uA)) {
                    aa <- aliases(fvarsA[subvariableURLs(varsA[[a2uA[x]]])])
                } else {
                    aa <- c()
                }
                if (x %in% names(a2uB)) {
                    ab <- aliases(fvarsB[subvariableURLs(varsB[[a2uB[x]]])])
                } else {
                    ab <- c()
                }
                allaliases <- c(aa, setdiff(ab, aa))
                ## Grab the subvariables with aliases that match for the union
                compareSubvariables(
                    fvarsA[na.omit(a2uFA[allaliases])],
                    fvarsB[na.omit(a2uFB[allaliases])]
                )
            },
            simplify = FALSE
        )
    ),
    class = "compareDatasets"
    ))
}

summarizeCompareDatasets <- function(comp) {
    ## summarize the summaries of each
    cats <- lapply(comp$categories, summarizeCompareCategories)
    ok.cats <- vapply(
        cats,
        function(x) length(x$problems$mismatched.ids) == 0,
        logical(1)
    )
    subs <- lapply(comp$subvariables, summarizeCompareSubvariables)
    ## Check for array overlap separately: requires comparing against all other
    ## arrays. If any problems found, poke into the summary output.
    overlap <- checkSubvariableParents(comp$subvariables)
    if (length(overlap)) {
        subs <- modifyList(subs, lapply(overlap, function(x) {
            list(problems = list(overlap = x))
        }))
    }
    ok.subs <- vapply(
        subs,
        function(x) {
            p <- x$problems
            return(length(p$mismatched.name) == 0 &
                length(p$parents$A) <= 1 &
                length(p$parents$B) <= 1 &
                is.null(p$overlap))
        },
        logical(1)
    )
    vars <- summarizeCompareVariables(comp$variables)
    return(structure(list(
        cats = cats, ok.cats = ok.cats, subs = subs,
        ok.subs = ok.subs, vars = vars
    ),
    class = "compareDatasetsSummary"
    ))
}

#' @export
summary.compareDatasets <- function(object, ...) summarizeCompareDatasets(object)

#' @export
print.compareDatasetsSummary <- function(x, ...) {
    ## Variables
    bad.var.count <- length(x$vars$problems$mismatched.type) +
        length(x$vars$problems$mismatched.name)
    if (bad.var.count) {
        print(x$vars)
    } else {
        cat("Total variables:", nrow(x$vars$variables), "\n")
    }

    ## Categories
    bad.cats <- !x$ok.cats
    bad.cat.count <- sum(bad.cats)
    if (bad.cat.count) {
        cat("\nMatched variables with categories:", length(x$cats), "\n")
        cat("With issues:", bad.cat.count, "\n\n")
        for (i in names(x$cats[bad.cats])) {
            cat("$", i, "\n", sep = "")
            print(x$cats[[i]])
            cat("\n")
        }
    }

    ## Subvariables
    bad.subs <- !x$ok.subs
    bad.sub.count <- sum(bad.subs)
    if (bad.sub.count) {
        cat("\nArray variables:", length(x$subs), "\n")
        cat("With subvariable issues:", bad.sub.count, "\n\n")
        for (i in names(x$subs[bad.subs])) {
            cat("$", i, "\n", sep = "")
            print(x$subs[[i]])
            cat("\n")
        }
    }

    ## Else:
    if (bad.var.count + bad.cat.count + bad.sub.count == 0) {
        cat("All good :)\n")
    }
    invisible(x)
}
