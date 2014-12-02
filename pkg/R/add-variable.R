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
    var_url <- POSTNewVariable(dataset@urls$variables_url, 
        toVariable(values, ...))
    dataset <- refresh(dataset)
    invisible(dataset)
}

POSTNewVariable <- function (catalog_url, variable) {
    
    do.POST <- function (x) crPOST(catalog_url, body=toJSON(x, digits=15))
    
    if (!("expr" %in% names(variable))) {
        ## If deriving a variable, skip this and go straight to POSTing
        if (variable$type %in% c("multiple_response", "categorical_array")) {
            ## assumes: array of subvariables included, and if MR, at least one
            ## category has selected: TRUE
            ## TODO: make the data import API take array types directly

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
        } else if (variable$type == "categorical") {
            Categories(variable$categories) ## will error if cats are invalid
        }
    }
    out <- do.POST(variable)
    invisible(out)
}

addVariables <- function (dataset, vars) {
    ## assume data frame
    nvars <- ncol(vars)
    vars_url <- dataset@urls$variables_url
    for (i in seq_len(nvars)) {
        POSTNewVariable(vars_url,
            toVariable(vars[[i]], name=names(vars)[i], alias=names(vars)[i]))
    }
    invisible(refresh(dataset))
}

deriveVariable <- function (dataset, expr, ...) {
    derivation <- list(...)
    derivation$expr <- zcl(expr)
    var_url <- POSTNewVariable(dataset@urls$variables_url, derivation)
    dataset <- refresh(dataset)
    invisible(dataset)
}
