##' Make a Categorical Array or Multiple Response variable
##'
##' @param list_of_variables a list of Variable objects to bind together, or a
##' Dataset object containing only the Variables to bind (as in from subsetting
##' a Dataset), or values (e.g. names) of variables corresponding to \code{key}. If omitted, must supply \code{dataset} and \code{pattern}. If specifying values, must include \code{dataset}.
##' @param dataset the Crunch Dataset to which the variables in 
##' \code{list_of_variables} belong, or in which to search for variables based
##' on \code{pattern}. If omitted, \code{list_of_variables} must exist and all
##' Variables in the list must belong to the same Dataset
##' @param pattern An optional regular expression to search for variables to 
##' bind within \code{dataset}.
##' @param key character, the name of the Variable field in which to search
##' with \code{pattern}. Default is 'alias'.
##' @param name character, the name that the new Categorical Array variable
##' should have. Required.
##' @param selections character, for \code{makeMR}, the names of the 
##' categories to mark as the dichotomous selections. Required for 
##' \code{makeMR}; ignored in \code{makeArray}.
##' @param ... Optional additional attributes to set on the new variable. Not
##' yet supported.
##' @return The object of class CategoricalArrayVariable or
##' MultipleResponseVariable corresponding to the just-created variable on the
##' server.
##' @export
makeArray <- function (list_of_variables, dataset=NULL, pattern=NULL, key=namekey(dataset), name, ...) {
    
    Call <- match.call(expand.dots=FALSE)
    
    if (missing(name)) {
        stop("Must provide the name for the new variable", call.=FALSE)
    }
    
    Call[[1L]] <- quote(rcrunch:::prepareBindInputs)
    x <- eval.parent(Call)
    
    invisible(bindVariables(x$variable_urls, x$dataset, name, type="categorical_array", ...))
}

##' Given inputs to makeArray/makeMR, parse and validate
##' @param ... Stuff from calling function that will be ignored.
prepareBindInputs <- function (list_of_variables=NULL, dataset=NULL, pattern=NULL,
                               key=namekey(dataset), ...) {
    
    listOfVariablesIsValid <- function (lov) {
        return(is.list(lov) && all(vapply(lov, is.variable, logical(1))))
    }
    datasetURLfromVariables <- function (lov) {
        ds_urls <- unique(vapply(lov, datasetReference, character(1)))
        if (length(ds_urls) > 1) {
            ## see if list of variables actually do belong to same dataset
            stop("All variables to be bound together must be from the same dataset",
                call.=FALSE)
        }
        return(ds_urls)
    }
    
    variable_urls <- NULL
    if (is.null(dataset)) {
        if (is.dataset(list_of_variables)) {
            ## as in, if the list of variables is a [ extraction from a Dataset
            dataset <- list_of_variables
            variable_urls <- urls(dataset@variables)
        } else if (listOfVariablesIsValid(list_of_variables)) {
            ds_url <- datasetURLfromVariables(list_of_variables)
            dataset <- as.dataset(GET(ds_url))
            variable_urls <- vapply(list_of_variables, function (x) self(x), character(1), USE.NAMES=FALSE)
        } else {
            stop("Must provide a Dataset and either a list of Variables to combine or a pattern to identify Variables within that Dataset")
        }
    }
    if (is.null(dataset)) {
        stop("Must supply a Crunch dataset in which to make the array variable", call.=FALSE)
    }
    
    if (is.null(variable_urls)) {
        variable_urls <- findVariableURLs(dataset, refs=list_of_variables, pattern=pattern, key=key)
        if (!length(variable_urls)) {
            stop("Pattern did not match any variables", call.=FALSE)
        }
    }
    
    return(list(dataset=dataset, variable_urls=variable_urls))
}

##' Take variables and their dataset and bind them into a new array variable
bindVariables <- function (var_urls, dataset, name, ...) {
    out <- POSTBindVariables(dataset@urls$variables_url, var_urls, name=name, ...)
    invisible(returnNewVariable(out, dataset))
}

returnNewVariable <- function (variable_url, dataset) {
    dataset <- refresh(dataset)
    return(entity(dataset@variables[[variable_url]]))
}

POSTBindVariables <- function (bind_url, variable_urls, ...) {
    payload <- list(variables=I(variable_urls), ...)
    return(POST(bind_url, body=toJSON(payload)))
}