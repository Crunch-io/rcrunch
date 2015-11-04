copyVariable <- function (x, deep=FALSE, ...) {
    stopifnot(is.variable(x))
    if (deep) {
        halt("Deep copying not implemented.")
    }
    
    ## Get the variable catalog's URL to POST to
    varcat_url <- variableCatalogURL(x)
    
    newbody <- list(...)
    oldbody <- updateList(x@body, tuple(x)@body)
    oldbody$name <- paste0(oldbody$name, " (copy)")
    oldbody$alias <- paste0(oldbody$alias, "_copy")
    
    body <- updateList(oldbody["name"], newbody) ## dropping other body attrs for now; see copyVariableReferences
    body$expr <- zfunc("copy_variable", x)
    
    ## Validate that name and alias are unique
    varcat <- VariableCatalog(crGET(varcat_url))
    
    out <- crPOST(varcat_url, body=toJSON(body))
    newvar <- returnNewVariable(out, varcat)
    invisible(newvar)
}

copy <- copyVariable

copyVariableReferences <- function (x, fields=c("name", "alias",
                                    "description", "discarded", "format",
                                    "view", "type")) {
    return(x@body[intersect(fields, names(x@body))])
}