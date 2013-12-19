##' Make a Categorical Matrix variable
##'
##' @param list_of_variables a list of Variable objects to bind together, or a
##' Dataset object containing only the Variables to bind (as in from subsetting
##' a Dataset). If omitted, must supply \code{dataset} and \code{pattern}.
##' @param dataset the Crunch Dataset to which the variables in 
##' \code{list_of_variables} belong, or in which to search for variables based
##' on \code{pattern}. If omitted, \code{list_of_variables} must exist and all
##' Variables in the list must belong to the same Dataset
##' @param pattern An optional regular expression to search for variables to 
##' bind within \code{dataset}.
##' @param key character, the name of the Variable field in which to search
##' with \code{pattern}. Default is 'alias'.
##' @param name character, the name that the new Categorical Matrix variable
##' should have. Required.
##' @param ... Optional additional attributes to set on the new variable. Not
##' yet supported.
##' @return The object of class CategoricalMatrixVariable corresponding to the 
##' just-created variable on the server.
##' @export
makeMatrix <- function (list_of_variables, dataset=NULL, pattern=NULL, key="alias", name, ...) {
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
    
    if (missing(name)) {
        stop("Must provide the name for the new variable", call.=FALSE)
    }
    if (is.null(dataset)) {
        if (is.dataset(list_of_variables)) {
            ## as in, if the list of variables is a [ extraction from a Dataset
            dataset <- list_of_variables
        } else if (!listOfVariablesIsValid(list_of_variables)) {
            stop("Must provide a Dataset and either a list of Variables to combine or a pattern to identify Variables within that Dataset")
        } else {
            ds_url <- datasetURLfromVariables(list_of_variables)
            dataset <- as.dataset(GET(ds_url))
        }
    }
    if (is.null(dataset)) {
        stop("Must supply a Crunch dataset in which to make the Multiple Response variable", call.=FALSE)
    }
    
    if (!is.null(pattern)) {
        keys <- selectFrom(key, lapply(dataset[], function (x) x@body))
        matches <- grep(pattern, keys)
        if (!length(matches)) {
            stop("Pattern did not match any variables", call.=FALSE)
        }
        list_of_variables <- dataset[matches]
    }
    
    ## Assert all variables are Variables
    if (!listOfVariablesIsValid(list_of_variables)) {
        stop("Invalid list of Variables to combine")
    }
    ## Assert variables correspond to the dataset
    ## NPR: re-enable this to prevent a possible 500 error when the user-datasets stuff
    ## is cleared up on the server
    # if (datasetURLfromVariables(list_of_variables) != self(dataset)) {
    #     stop("`list_of_variables` must be from `dataset`")
    # }
    var_urls <- vapply(list_of_variables, self, character(1), USE.NAMES=FALSE)
    payload <- list(name=name, variables=I(var_urls)) ## extend backend to take ...
    out <- POST(dataset@urls$bind_url, body=toJSON(payload))
    ## could apply ... here
    invisible(as.variable(GET(out)))
}