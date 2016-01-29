combineMRs <- function(var1, var2, ..., useNAsubs = FALSE, selectedFirst=TRUE){
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
    
    values <- matrix(sapply(subs, function(subvar) {
        tmp <- list()
        for (var in vars){
            if (subvar %in% names(subvariables(var))) tmp[[alias(var)]] <- as.vector(var[[subvar]])
        }
        tmp <- matrix(unlist(tmp), ncol=length(tmp))
        if (selectedFirst) tmp2 <- ifelse(rowSums(tmp == 'selected', na.rm = TRUE) > 0, 'selected', ifelse(rowSums(tmp == 'not selected', na.rm = TRUE) > 0, 'not selected', NA))
        if (!selectedFirst) tmp2 <- ifelse(rowSums(tmp == 'not selected', na.rm = TRUE) > 0, 'not selected', ifelse(rowSums(tmp == 'selected', na.rm = TRUE) > 0, 'selected', NA))
        return(tmp2)
    }), ncol=length(subs))    
    colnames(values) <- subs
    
    newbody$subvariables <- lapply(subs, function(sub){
        VarDef(data=factor(values[[sub]]), 
            alias=paste0(newbody$alias, "_", gsub(" ", "_", gsub("[[:punct:]]", "", tolower(sub)))),name=sub)
    })

    class(newbody) <- 'VariableDefinition'
    return(newbody)


}    

