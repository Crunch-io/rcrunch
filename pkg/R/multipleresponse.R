##' Make a Multiple Response variable
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
##' @param name character, the name that the new Multiple Response variable
##' should have. Required.
##' @param ... Optional additional attributes to set on the new variable. Not
##' yet supported.
##' @return The object of class MultipleResponseVariable corresponding to the 
##' just-created variable on the server.
##' @export
makeMR <- function (list_of_variables, dataset=NULL, pattern=NULL, key="alias", name, ...) {
    if (is.null(dataset)) {
        if (is.dataset(list_of_variables)) {
            ## as in, if the list of variables is a [ extraction from a Dataset
            dataset <- list_of_variables
        } else {
            ## see if list of variables actually do belong to same dataset
            ds_urls <- unique(vapply(list_of_variables, datasetReference, character(1)))
            if (length(ds_urls) == 1) {
                dataset <- as.dataset(GET(ds_urls))
            } else {
                stop("All variables to be bound together must be from the same dataset",
                    call.=FALSE)
            }
        }
    }
    if (is.null(dataset)) {
        stop("Must supply a Crunch dataset in which to make the Multiple Response variable", call.=FALSE)
    }
    
    if (!is.null(pattern)) {
        keys <- selectFrom(key, dataset)
        matches <- grep(pattern, keys)
        if (!length(matches)) {
            stop("Pattern did not match any variables", call.=FALSE)
        }
        list_of_variables <- dataset[matches]
    }
    
    ## Assert all variables are Variables, then:
    var_urls <- vapply(list_of_variables, self, character(1), USE.NAMES=FALSE)
    payload <- list(name=name, variables=I(var_urls)) ## extend backend to take ...
    out <- POST(dataset@urls$bind_url, body=toJSON(payload))
    refresh(dataset)
    invisible(as.variable(GET(out)))
}