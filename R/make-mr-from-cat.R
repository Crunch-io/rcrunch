makeMRfromCat <- function(var1, var2, ...){
    stopifnot(is.variable(var1) & is.variable(var2))
    stopifnot(is.Categorical(var1) & is.Categorical(var2))
    
    newbody <- list()
    oldbody1 <- updateList(copyVariableReferences(var1), tuple(var1)@body)
    oldbody2 <- updateList(copyVariableReferences(var2), tuple(var2)@body)

    newbody$name <- paste0(oldbody1$name, ", ", oldbody2$name, ", combine")
    newbody$alias <- paste0(oldbody1$alias, "_", oldbody2$alias)
    newbody$type <- 'categorical_array'

    newbody <- updateList(newbody, list(...))
    
    var1.values <- as.vector(var1)
    var2.values <- as.vector(var2)
    newbody$subvariables <- lapply(union(names(categories(var1)), names(categories(var2))), function(subvar){
        tmp <- factor(ifelse(var1.values %in% subvar | var2.values %in% subvar, 'selected', 'not selected'), levels=c('selected', 'not selected'))
        VarDef(data=tmp,name=subvar, alias=paste0(newbody$alias, "_", gsub(" ", "_", tolower(subvar))))
    })

    class(newbody) <- 'VariableDefinition'
    return(newbody)
}