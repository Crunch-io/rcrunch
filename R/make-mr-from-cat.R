##' Combine categorical variables into an MR
##'
##' This function lets you combine categorical variables into an MR variable
##' Arguments include at least two categorical variables as well as any metadata
##' for the new variable that is being created. The final argument useNAcats
##' is given the defult value FALSE. If it is made TRUE, any categories that
##' are NA in the categorical variables will become subvariables.

makeMRfromCat <- function(var1, var2, ..., useNAcats = FALSE){
    vars <- c(var1, var2, list(...)[sapply(1:length(list(...)), function(i) is.variable(list(...)[[i]]))])
    stopifnot(is.variable(var1) & is.variable(var2))
    stopifnot(any(sapply(vars, is.Categorical)))
    
    newbody <- list()
    newbody$alias <- paste0(sapply(vars, alias), collapse = '_')
    newbody$name <- paste0(paste0(sapply(vars, name), collapse = ', '), ", combine")
    newbody$type <- 'categorical_array'
    
    newbody <- updateList(newbody, list(...)[!sapply(1:length(list(...)), function(i) is.variable(list(...)[[i]]))])

    values <- matrix(sapply(vars, function(var) names(categories(var))[as.vector(var, mode='id')]), ncol=length(vars))
    if (useNAcats) subs <- unique(unlist(lapply(vars, function(var) names(categories(var)))))
    else subs <- unique(unlist(lapply(vars, function(var) names(categories(var))[!is.na(categories(var))])))

    newbody$subvariables <- lapply(subs, function(sub){
        VarDef(data=factor(ifelse(rowSums(values == sub,na.rm = TRUE) > 0, 'selected', 
            ifelse(rowSums(is.na(values)) == length(vars), NA, 'not selected'))), 
            alias=paste0(newbody$alias, "_", gsub(" ", "_", gsub("[[:punct:]]", "", tolower(sub)))),name=sub)
    })

    class(newbody) <- 'VariableDefinition'
    return(newbody)
}
