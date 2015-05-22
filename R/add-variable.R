addVariable <- function (dataset, values, ...) {
    new <- length(values)
    old <- getNrow(dataset, filtered=FALSE)
    if (new == 1 && old > 1) {
        values <- rep(values, old)
        new <- old
    }
    if (old > 0 && new != old) {
        halt("replacement has ", new, " rows, data has ", old)
    }
    var_url <- POSTNewVariable(shojiURL(dataset, "catalogs", "variables"), 
        toVariable(values, ...))
    dataset <- refresh(dataset)
    invisible(dataset)
}

POSTNewVariable <- function (catalog_url, variable) {
    
    do.POST <- function (x) crPOST(catalog_url, body=toJSON(x, digits=15))
    
    if (!("expr" %in% names(variable))) {
        ## If deriving a variable, skip this and go straight to POSTing
        if (variable$type %in% c("multiple_response", "categorical_array")) {
            ## Assumes: array of subvariables included, and if MR, at least one
            ## category has selected: TRUE
            if (!("subvariables" %in% names(variable))) {
                halt("Cannot create array variable without specifying",
                    " subvariables")
            }
            ## Two options supported: 
            ## (1) Create array from single array definition
            ## (2) Create subvariables individually and then bind them
            ## Sniff to see which case we have. If (1), proceed normally
            is_catvardef <- function (x) {
                all(c("categories", "values") %in% names(x))
            }
            is_arraydef <- is_catvardef(variable) &&
                !any(vapply(variable$subvariables, is_catvardef, logical(1)))
            if (!is_arraydef) {
                lapply(variable$subvariables, function (x) {
                    Categories(x$categories) ## Will error if invalid
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
            Categories(variable$categories) ## Will error if cats are invalid
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
