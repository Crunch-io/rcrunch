##' Copy a variable
##'
##' Makes a copy of a Crunch variable on the server.
##'
##' @param x a CrunchVariable to copy
##' @param deep logical: should this be a deep copy, in which there is no
##' dependence on the original variable, or a shallow one, in which the copy
##' is more of a symbolic link? Default is \code{FALSE}, meaning symlink, and
##' in fact, deep copying is not yet supported.
##' @param ... Additional metadata to give to the new variable. If not given,
##' the new variable will have a name that is the same as the original but with
##' " (copy)" appended, and its alias will be the old alias with "_copy"
##' appended.
##' @return the copy CrunchVariable
##' @export
copyVariable <- function (x, deep=FALSE, ...) {
    stopifnot(is.variable(x))
    if (deep) {
        halt("Deep copying not implemented.")
    }
    
    ## Get the variable catalog's URL to POST to
    varcat_url <- variableCatalogURL(x)
    
    newbody <- list(...)
    oldbody <- updateList(copyVariableReferences(x), tuple(x)@body)
    oldbody$name <- paste0(oldbody$name, " (copy)")
    oldbody$alias <- paste0(oldbody$alias, "_copy")
    
    body <- updateList(oldbody, newbody)
    body$type <- NULL
    body$id <- NULL
    body$expr <- zfunc("copy_variable", x)
    
    ## Validate that name and alias are unique
    varcat <- VariableCatalog(crGET(varcat_url))
    
    out <- crPOST(varcat_url, body=toJSON(body))
    newvar <- returnNewVariable(out, varcat)
    invisible(newvar)
}

##' @rdname copyVariable
##' @export
copy <- copyVariable

copyVariableReferences <- function (x, fields=c("name", "alias",
                                    "description", "discarded", "format",
                                    "view", "type")) {
    return(x@body[intersect(fields, names(x@body))])
}