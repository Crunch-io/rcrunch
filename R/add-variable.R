#' Add multiple variables to a dataset
#'
#' This function lets you add more than one variable at a time to a dataset.
#' If you have multiple variables to add, this function will be faster than
#' doing \code{ds$var <- value} assignment because it doesn't refresh the
#' dataset's state in between variable POST requests.
#' @param dataset a CrunchDataset
#' @param ... \code{\link{VariableDefinition}}s or a list of
#' VariableDefinitions.
#' @return \code{dataset} with the new variables added (invisibly)
#' @export
#' @importFrom httpcache halt
addVariables <- function (dataset, ...) {
    var_catalog_url <- shojiURL(dataset, "catalogs", "variables")
    ## Get vardefs and validate
    vardefs <- list(...)
    ## Check for whether a list of vardefs passed
    if (length(vardefs) == 1 &&
            is.list(vardefs[[1]]) &&
            !inherits(vardefs[[1]], "VariableDefinition")) {

        vardefs <- vardefs[[1]]
    }
    ## Check that all are VariableDefinitions
    are.vardefs <- vapply(vardefs, inherits, logical(1),
        what="VariableDefinition")
    if (!all(are.vardefs)) {
        halt("Must supply VariableDefinitions")
    }
    ## Check that if values specified, they have the right length
    vardefs <- lapply(vardefs, validateVarDefRows, numrows=getNrow(dataset))

    ## TODO: check array subvariable rows similarly. and/or pull out and cbind
    ## to a single parent-level "values" column.

    ## Upload one at a time.
    ## TODO: Server should support bulk insert

    new_var_urls <- lapply(vardefs,
        function (x) try(POSTNewVariable(var_catalog_url, x), silent=TRUE))
    ## Be silent so we can throw the errors together at the end
    checkVarDefErrors(new_var_urls)

    dataset <- refresh(dataset)

    ## Check for any "replacement" variables
    hides <- lapply(vardefs, attr, which=".hide")
    todo <- vapply(hides, Negate(is.null), logical(1))
    if (any(todo)) {
        replacements <- new_var_urls[todo]
        locations <- lapply(hides[todo],
            function (u) locateEntity(u[1], ord=ordering(dataset)))
        hides <- unlist(hides)
        allVariables(dataset)[hides]<- hide(allVariables(dataset)[hides])
        for (i in seq_along(locations)) {
            loc <- locations[[i]]
            if (length(loc)) {
                ## If location is length 0, it is already at top level, so
                ## no need to move
                entities(ordering(dataset)[[loc]]) <- c(entities(ordering(dataset)[[loc]]), replacements[i])
                ## TODO: make that be folder(dataset[[replacements[[i]]]]) <- loc
            }
        }
    }

    invisible(dataset)
}

validateVarDefRows <- function (vardef, numrows) {
    ## Pre-check the column length being sent to the server to confirm that
    ## the number of rows matches what's already in the dataset.
    ## Also compact the values being sent, if possible
    if (!any(c("expr", "derivation", "subvariables") %in% names(vardef))) {
        new <- length(vardef$values)
        if (new == 0) {
            warning("Adding variable with no rows of data", call.=FALSE)
        } else if (numrows > 0 && new > 1 && new != numrows) {
            halt("replacement has ", new, " rows, data has ", numrows)
        }
        if (all(is.na(vardef$values))) {
            ## Don't send them at all--same thing.
            ## Server will (oddly) reject if you send "values": null.
            ## (400) Bad Request: If 'values' is included, it cannot be null
            ## TODO: Move this somewhere else so that it's not a side effect
            ## of "validation"
            vardef$values <- NULL
        } else {
            uniques <- unique(vardef$values)
            if (numrows > 0 && length(uniques) == 1) {
                ## Just send the unique value to save bandwidth
                ## TODO: Move this somewhere else so that it's not a side effect
                ## of "validation"
                vardef$values <- uniques
            }
        }
    }
    return(vardef)
}

POSTNewVariable <- function (catalog_url, variable) {

    do.POST <- function (x) crPOST(catalog_url, body=toJSON(x, digits=15))

    if (!any(c("expr", "derivation") %in% names(variable))) {
        ## If deriving a variable, skip this and go straight to POSTing
        if (variable$type %in% c("multiple_response", "categorical_array")) {
            ## Assumes: array of subvariables included, and if MR, at least one
            ## category has selected: TRUE, or "selected_categories" given
            if (!("subvariables" %in% names(variable))) {
                halt("Cannot create array variable without specifying",
                    " subvariables")
            }
            ## Three options supported:
            ## (1) Bind together existing subvariables
            ## (2) Create array from single array definition
            ## (3) Create subvariables individually and then bind them
            ## Sniff to see which case we have. If (1) or (2), proceed normally
            is_catvardef <- function (x) {
                all(c("categories", "values") %in% names(x))
            }
            is_binddef <- is.character(variable$subvariables) &&
                !("categories" %in% names(variable))
            is_arraydef <- is_catvardef(variable) &&
                !any(vapply(variable$subvariables, is_catvardef, logical(1)))
            case3 <- !(is_binddef | is_arraydef)
            if (case3) {
                ## Let's arrange the values so that we can make a single request

                ## First, get the categories
                cats <- lapply(variable$subvariables, vget("categories"))
                if (length(unique(vapply(cats, toJSON, character(1)))) > 1) {
                    halt("All subvariables must have identical categories")
                }
                variable$categories <- cats[[1]]

                ## Now get the values
                variable$values <- matrix(unlist(lapply(variable$subvariables,
                    vget("values"))), ncol=length(variable$subvariables), byrow=FALSE)
                ## Now strip out the extraneous metadata from the subvars
                variable$subvariables <- lapply(variable$subvariables, function (x) {
                    x[!(names(x) %in% c("type", "categories", "values"))]
                })
            }
        }
        if ("categories" %in% names(variable)) {
            Categories(data=variable$categories) ## Will error if cats are invalid
        }
    }
    out <- do.POST(variable)
    invisible(out)
}


checkVarDefErrors <- function(new_var_urls) {
    errs <- vapply(new_var_urls, is.error, logical(1))
    if (any(errs)) {
        if (length(errs) == 1) {
            ## Just one variable added. Throw its error.
            rethrow(new_var_urls[[1]])
        }
        halt("The following variable definitions errored on upload: ",
            paste(which(errs), collapse = ", ")
        )
    }
}
