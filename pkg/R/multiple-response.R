##' @rdname makeArray
##' @export
makeMR <- function (list_of_variables, dataset=NULL, pattern=NULL, key=namekey(dataset), name, selections, ...) {
    Call <- match.call(expand.dots=FALSE)
    
    if (missing(name)) {
        stop("Must provide the name for the new variable", call.=FALSE)
    }
    if (missing(selections)) {
        stop(paste("Must provide the names of the", 
            "category or categories that indicate the dichotomous",
            "selection"), call.=FALSE)
    }
    
    Call[[1L]] <- as.name("prepareBindInputs")
    x <- eval.parent(Call)
    
    ## Get the actual variables so that we can validate
    vars <- lapply(x$list_of_variables, function (u) entity(x$dataset@variables[[u]]))
    are.categorical <- vapply(vars, is.Categorical, logical(1))
    if (!all(are.categorical)) {
        varnames <- vapply(vars[!are.categorical], 
            function (x) name(x),
            character(1))
        stop(serialPaste(varnames), 
            " are not Categorical variables. Convert them to ",
            "Categorical before combining to Multiple Response", call.=FALSE)
    }
    
    ## Validate selections before binding
    catnames <- unique(unlist(lapply(vars, 
        function (y) names(categories(y)))))
    if (!all(selections %in% catnames)) {
        stop("Selection(s) not found in variable's categories. ", 
            "Category names are: ", serialPaste(catnames), call.=FALSE)
        ## Could return more useful messaging here
    }
    
    var <- bindVariables(x$list_of_variables, x$dataset, name, ...)
    var <- dichotomize(var, selections)
    invisible(var)
}