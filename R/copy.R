copyVariable <- function (x, deep=FALSE, ...) {
    stopifnot(is.variable(x))
    if (deep) {
        halt("Deep copying not implemented.")
    }
    
    ## Get the variable catalog's URL to POST to
    d <- ShojiObject(crGET(datasetReference(x)))
    varcat_url <- shojiURL(d, "catalogs", "variables")
    
    newbody <- list(...)
    oldbody <- updateList(x@body, tuple(x)@body)
    oldbody$name <- paste0(oldbody$name, " (copy)")
    oldbody$alias <- paste0(oldbody$alias, "_copy")
    
    body <- updateList(oldbody["name"], newbody) ## dropping other body attrs for now
    body$expr <- zcl(x)
    
    ## Validate that name and alias are unique
    varcat <- VariableCatalog(crGET(varcat_url))
    
    out <- crPOST(varcat_url, body=toJSON(body))
    newvar <- returnNewVariable(out, varcat)
    invisible(newvar)
}

copy <- copyVariable