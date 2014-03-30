addVariable <- function (dataset, values, ...) {
    new <- length(values)
    old <- getDim(dataset, filtered=FALSE)[1]
    if (new == 1 && old > 1) {
        values <- rep(values, old)
        new <- old
    }
    if (old > 0 && new != old) {
        stop("replacement has ", new, " rows, data has ", old)
    }
    var_url <- POSTNewVariable(dataset@urls$variables_url, 
        toVariable(values, ...))
    dataset <- refresh(dataset) ## would like not to do this
    # variable <- as.variable(GET(var_url))
    # dataset@.Data[[variable@body$alias]] <- variable
    invisible(dataset)
}

POSTNewVariable <- function (collection_url, variable, bind_url=NULL) {
    
    do.POST <- function (x) POST(collection_url, body=toJSON(x, digits=15))
    is.error <- function (x) inherits(x, "try-error")
    
    if (variable$type %in% c("multiple_response", "categorical_array")) {
        ## assumes: array of subvariables included, and if MR, at least one category has selected: TRUE
        ## TODO: make the data import API take array types directly
        variable$type <- NULL
        subvars <- variable$subvariables
        variable$subvariables <- NULL
        
        var_urls <- lapply(subvars, function (x) try(do.POST(x)))
        errs <- vapply(var_urls, is.error, logical(1))
        if (any(errs)) {
            # Delete subvariables that were added, then raise
            lapply(var_urls[!errs], function (x) DELETE(x))
            stop("Subvariables errored on upload", call.=FALSE)
        } else {
            var_urls <- unlist(var_urls)
        }
        variable$bind_url <- bind_url
        variable$variable_urls <- var_urls
        out <- do.call("POSTBindVariables", variable)
        invisible(out)
    } else {
        invisible(do.POST(variable))
    }
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
