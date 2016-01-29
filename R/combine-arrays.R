##' Combine array variables into a new array
##'
##' This function lets you combine array variables into a new array
##' Arguments include at least two array variables as well as any metadata
##' for the new variable that is being created. This function uses the data from 
##' the arrays in the order that they are given. If the first variable is NA for a given 
##' row of a subvariable, it will use the data from the second variable and so on.
##' @param variable 1 to combine
##' @param variable 2 to combine
##' @param other desired attributes such as new variable alias, name, 
##' description as well as any other variables you would like to combine
##' @return a new variable with combine values from the arrays
##' @export
combineArrays <- function(var1, var2, ...){
    vars <- c(var1, var2, list(...)[sapply(1:length(list(...)), function(i) is.variable(list(...)[[i]]))])
    stopifnot(is.variable(var1) & is.variable(var2))
    stopifnot(any(sapply(vars, is.MultipleResponse)))

    newbody <- list()
    newbody$alias <- paste0(sapply(vars, alias), collapse = '_')
    newbody$name <- paste0(paste0(sapply(vars, name), collapse = ', '), ", combine")
    newbody$type <- 'categorical_array'
    
    newbody <- updateList(newbody, list(...)[!sapply(1:length(list(...)), function(i) is.variable(list(...)[[i]]))])
    
    subs <- unique(unlist(lapply(vars, function(var) names(subvariables(var)))))
    
    nrows <- length(as.vector(var1[[1]]))
    values <- sapply(subs, function(subvar) {
        tmp <- rep(NA, nrows)
        for (var in vars){
            if (is.na(alias))
            if (subvar %in% names(subvariables(var))) {
                tmp[is.na(tmp)] <- as.character(as.vector(var[[subvar]][is.na(tmp)]))
            }
        }
        return(tmp)
    })    
    colnames(values) <- subs
    
    newbody$subvariables <- lapply(subs, function(sub){
        VarDef(data=factor(values[[sub]]), 
            alias=paste0(newbody$alias, "_", gsub(" ", "_", gsub("[[:punct:]]", "", tolower(sub)))),name=sub)
    })

    class(newbody) <- 'VariableDefinition'
    return(newbody)
}



