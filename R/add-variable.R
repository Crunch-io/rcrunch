addVariable <- function (dataset, vardef, ...) {
    ## Construct payload (if not already constructed)
    ## TODO: deprecate this behavior? (Only used in tests)
    if (!inherits(vardef, "VariableDefinition")) {
        vardef <- VariableDefinition(vardef, ...)
    }
    
    if (!any(c("expr", "subvariables") %in% names(vardef))) {
        ## Validate that we're sending the right number of rows
        new <- length(vardef$values)
        old <- getNrow(dataset, filtered=FALSE)
        if (new == 1 && old > 1) {
            vardef$values <- rep(vardef$values, old)
            new <- old
        }
        if (new == 0) {
            warning("Adding variable with no rows of data", call.=FALSE)
        } else if (old > 0 && new != old) {
            halt("replacement has ", new, " rows, data has ", old)
        }
    }
    var_url <- POSTNewVariable(shojiURL(dataset, "catalogs", "variables"), 
        vardef)
    dataset <- refresh(dataset)
    invisible(dataset)
}

POSTNewVariable <- function (catalog_url, variable) {
    
    do.POST <- function (x) crPOST(catalog_url, body=toJSON(x, digits=15))
    
    if (!("expr" %in% names(variable))) {
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
                lapply(variable$subvariables, function (x) {
                    Categories(data=x$categories) ## Will error if invalid
                })
                
                ## Upload subvars, then bind
                var_urls <- lapply(variable$subvariables,
                    function (x) try(do.POST(x)))
                errs <- vapply(var_urls, is.error, logical(1))
                if (any(errs)) {
                    # Delete subvariables that were added, then raise
                    lapply(var_urls[!errs], function (x) crDELETE(x))
                    halt("Subvariables errored on upload")
                }
                # Else prepare to POST array definition
                variable$subvariables <- I(unlist(var_urls))
            }
        }
        if ("categories" %in% names(variable)) {
            Categories(data=variable$categories) ## Will error if cats are invalid
        }
    }
    out <- do.POST(variable)
    invisible(out)
}

addVariables <- function (dataset, vars) {
    ## assume data frame
    nvars <- ncol(vars)
    vars_url <- variableCatalogURL(dataset)
    for (i in seq_len(nvars)) {
        POSTNewVariable(vars_url,
            toVariable(vars[[i]], name=names(vars)[i], alias=names(vars)[i]))
    }
    invisible(refresh(dataset))
}

deriveVariable <- function (dataset, expr, ...) {
    derivation <- list(...)
    derivation$expr <- zcl(expr)
    var_url <- POSTNewVariable(variableCatalogURL(dataset), derivation)
    dataset <- refresh(dataset)
    invisible(dataset)
}
