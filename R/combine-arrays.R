combineArrays <- function(var1, var2, ..., useNAsubs = FALSE){
    vars <- c(var1, var2, list(...)[sapply(1:length(list(...)), function(i) is.variable(list(...)[[i]]))])
    stopifnot(is.variable(var1) & is.variable(var2))
    stopifnot(any(sapply(vars, is.MultipleResponse)))

    newbody <- list()
    newbody$alias <- paste0(sapply(vars, alias), collapse = '_')
    newbody$name <- paste0(paste0(sapply(vars, name), collapse = ', '), ", combine")
    newbody$type <- 'categorical_array'
    
    newbody <- updateList(newbody, list(...)[!sapply(1:length(list(...)), function(i) is.variable(list(...)[[i]]))])
    
    if (useNAcats) subs <- unique(unlist(lapply(vars, function(var) names(subvariables(var)))))
    else subs <- unique(unlist(lapply(vars, function(var) names(subvariables(var))[!is.na(subvariables(var))])))
    
    nrows <- length(as.vector(var1[[1]]))
    
    values <- sapply(subs, function(subvar) {
        tmp <- rep(NA, nrows)
        alias <- NA
        for (var in vars){
            if (is.na(alias))
            if (subvar %in% names(subvariables(var))) {
                if (is.na(alias)) {alias <- alias(var[[subvar]]); var_first <- alias(var)}
                tmp[is.na(tmp)] <- as.vector(var[[subvar]][is.na(tmp)])
            }
        }
        return(list(data=tmp, alias=alias, var_first=var_first))
    })    
    colnames(values) <- subs
    
    newbody$subvariables <- lapply(subs, function(sub){
        VarDef(data=factor(values[[sub]]$data), 
            alias=gsub(values[[sub]]$var_first, newbody$alias, values[[sub]]$alias),name=sub)
    })

    class(newbody) <- 'VariableDefinition'
    return(newbody)
}



