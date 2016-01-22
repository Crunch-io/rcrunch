##' Add subvariable to an array
##'
##' This function conceals the dirty work in making this happen. The array
##' gets unbound, the subvariables added, and then the subvariables
##' are rebound into a new array.
##' @param variable the array variable
##' @param subvariable subvariable to add
##' @return a new version of variable with the indicated subvariables
##' @export
addSubvariable <- function(variable, subvariable){
    ## Store some metadata up front
    payload <- copyVariableReferences(variable)
    subvars <- subvariables(variable)
    subvar.urls <- urls(subvars)
    subvar.names <- names(subvars)
    
    ## Identify subvariable URLs
#     if (inherits(subvariable, 'VariableDefinition')) {
#         ds <- addVariables(ds, subvariable)
#         subvariable <- ds[[subvariable$alias]]
#     }
    new_subvar.url <- self(subvariable)
    new_subvar.name <- name(subvariable)
    ## Unbind
    all.subvar.urls <- unlist(unbind(variable))
    
    ## Setdiff those deleted from those returned from unbind
    payload$subvariables <- I(c(all.subvar.urls, new_subvar.url))
    class(payload) <- "VariableDefinition"
    
    ## Rebind
    new_url <- POSTNewVariable(variableCatalogURL(variable), payload)
        
    ## Prune subvariable name prefix, or otherwise reset the names
    subvars <- Subvariables(crGET(absoluteURL("subvariables/", new_url)))
    names(subvars) <- c(subvar.names[na.omit(match(urls(subvars), subvar.urls))], new_subvar.name)

    ## What to return? This function is kind of a hack.
    invisible(new_url)
    
}

# addSubvariable <- addSubvariables
