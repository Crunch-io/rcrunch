##' Add subvariable to an array
##'
##' This function conceals the dirty work in making this happen. The array
##' gets unbound and rebound into a new array with the new variable added.
##' @param variable the array variable to modify
##' @param subvariable the subvariable to add
##' @return a new version of \code{variable} with the indicated subvariables
##' @export
<<<<<<< HEAD
addSubvariable <- function(variable, subvariable, ds=NULL){
=======
addSubvariable <- function (variable, subvariable){
>>>>>>> upstream/master
    ## Store some metadata up front
    payload <- copyVariableReferences(variable)
    subvars <- subvariables(variable)
    subvar.urls <- urls(subvars)
    subvar.names <- names(subvars)
<<<<<<< HEAD
    
    ## Identify subvariable URLs
    if (inherits(subvariable, 'VariableDefinition')) {
        ds <- addVariables(ds, subvariable)
        subvariable <- ds[[subvariable$alias]]
    }
    new_subvar.url <- self(subvariable)
    new_subvar.name <- name(subvariable)
=======

    # TODO: could support taking a VariableDefinition for subvariable
    # if (inherits(subvariable, 'VariableDefinition')) {
    #     ds <- addVariables(ds, subvariable)
    #     subvariable <- ds[[subvariable$alias]]
    # }

>>>>>>> upstream/master
    ## Unbind
    old.subvar.urls <- unlist(unbind(variable))

    ## Add the new variable URL to those we had before
    payload$subvariables <- I(c(subvar.urls, self(subvariable)))
    class(payload) <- "VariableDefinition"

    ## Rebind
    new_url <- POSTNewVariable(variableCatalogURL(variable), payload)

    ## Prune subvariable name prefix, or otherwise reset the names
    subvars <- Subvariables(crGET(absoluteURL("subvariables/", new_url)))
<<<<<<< HEAD
    print(names(subvars))
    print(urls(subvars))
    print(c(subvar.urls, new_subvar.url))
    subvars <- subvars[match(urls(subvars),c(subvar.urls, new_subvar.url))]
    names(subvars) <- c(subvar.names[na.omit(match(urls(subvars), subvar.urls))], new_subvar.name)
    print(names(subvars))
        
=======
    subvar.urls <- c(subvar.urls, self(subvariable))
    subvar.names <- c(subvar.names, name(subvariable))
    names(subvars) <- subvar.names[match(urls(subvars), subvar.urls)]

>>>>>>> upstream/master
    ## What to return? This function is kind of a hack.
    invisible(new_url)
}

# addSubvariable <- addSubvariables
