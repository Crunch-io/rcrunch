##' Combine multiple_response variables into a new multiple_response
##'
##' This function lets you combine multiple_response variables into a new multiple_response
##' Arguments include at least two multiple_response variables as well as any metadata
##' for the new variable that is being created. The argument useNAsubs
##' is given the defult value FALSE. If it is made TRUE, any subvariables that
##' are NA in the categorical variables will become subvariables.
##' This function uses the data from the arrays in the order that they are
##' given. If the first variable is NA for a given row of a subvariable,
##' it will use the data from the second variable and so on.
##' @param variable 1 to combine
##' @param variable 2 to combine
##' @param other desired attributes such as new variable alias, name, 
##' description as well as any other variables you would like to combine
##' @param a boolean describing whether subvariables that are NA in the
##' original variables should be made into subvariables in the new
##' variable
##' @param a boolean describing whether 'selected' should be given priority
##' if false, 'not selected' will be given priority
##' @return a new variable with combined responses from the given variables
##' @export
combineMRs <- function(var1, var2, ..., selectedFirst=TRUE){
    ## NOTE THAT THIS IS ONLY GOING TO WORK FOR VARIABLES WHERE THE CATEGORIES ARE SELECTED AND NOT SELECTED. IF THE CATEGORIES ARE MADE BY SPSS, THIS WON'T WORK.
    ## I've thought of a fix but it'll take more time (and is not currently useful to me).
    if (length(list(...)) > 0) vars <- c(var1, var2, list(...)[sapply(1:length(list(...)), function(i) is.variable(list(...)[[i]]))])
    else vars <- c(var1, var2)
    stopifnot(is.variable(var1) & is.variable(var2))
    stopifnot(any(sapply(vars, is.MultipleResponse)))
    
    newbody <- list()
    newbody$alias <- paste0(sapply(vars, alias), collapse = '_')
    newbody$name <- paste0(paste0(sapply(vars, name), collapse = ', '), ", combine")
    newbody$type <- 'categorical_array'
    
    if (length(list(...)) > 0) newbody <- updateList(newbody, list(...)[!sapply(1:length(list(...)), function(i) is.variable(list(...)[[i]]))])
    
    subs <- unique(unlist(lapply(vars, function(var) names(subvariables(var)))))
    
    values <- sapply(subs, function(subvar) {
        tmp <- list()
        for (var in vars){
            if (subvar %in% names(subvariables(var))) tmp[[alias(var)]] <- as.character(as.vector(var[[subvar]]))
        }
        tmp <- matrix(unlist(tmp), ncol=length(tmp))
        if (selectedFirst) tmp2 <- ifelse(rowSums(tmp == 'selected', na.rm = TRUE) > 0, 'selected', ifelse(rowSums(tmp == 'not selected', na.rm = TRUE) > 0, 'not selected', NA))
        if (!selectedFirst) tmp2 <- ifelse(rowSums(tmp == 'not selected', na.rm = TRUE) > 0, 'not selected', ifelse(rowSums(tmp == 'selected', na.rm = TRUE) > 0, 'selected', NA))
        return(tmp2)
    }, simplify=FALSE)    

    newbody$subvariables <- lapply(subs, function(sub){
        VarDef(data=factor(values[[sub]]), 
            alias=paste0(newbody$alias, "_", gsub(" ", "_", gsub("[[:punct:]]", "", tolower(sub)))),name=sub)
    })
    print(3)
    class(newbody) <- 'VariableDefinition'
    return(newbody)


}    

