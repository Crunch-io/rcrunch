##' Compare two datasets to see how they will append
##'
##' When one dataset is appended to another, variables and subvariables are
##' matched on their aliases, and then categories for variables that have them
##' are matched on category name. This function lines up the metadata between
##' two datasets as the append operation will so that you can inspect how well
##' the datasets will align before you do the append.
##'
##' Calling \code{summary} on the return of this function will print an
##' overview of places where the matching on variable alias and category name
##' may lead to undesired outcomes, enabling you to alter one or both datasets
##' to result in better alignment.
##' @param A CrunchDataset
##' @param B CrunchDataset
##' @return An object of class 'compareDatasets', a list of three elements: (1)
##' 'variables', a data.frame of variable metadata joined on alias; (2)
##' 'categories', a list of data.frames of category metadata joined on category
##' name, one for each variable with categories; and (3) 'subvariables', a
##' list of data.frames of subvariable metadata joined on alias, one for each
##' array variable.
##' @examples
##' \dontrun{
##'     comp <- compareDataset(ds1, ds2)
##'     summary(comp)
##' }
##' @export
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

    ## TODO: tests for compareSubvariables
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

##' @export
summary.compareDatasets <- function (object, ...) summarizeCompareDatasets(object)

##' @export
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
