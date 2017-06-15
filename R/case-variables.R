#' @rdname describe
#' @export
setMethod("name", "Case", function (x) x@name)
#' @rdname describe
#' @export
setMethod("name<-", "Case",
          function (x, value) {
              x@name <- validateNewName(value)
              return(x)
          })

#' Case variables
#' 
#' @param cases A list of objects of class [`Case`]
#' @param name a name for the new case variable
#' @param ... other properties to pass about the case variable
#' @name Case
makeCaseVariable <- function (cases, name, ...) {
    #check if all cases are cases.
    
    # make ids if cases don't have ids.
    need_ids <- sapply(cases, function(x) length(x@id) == 0)
    cases[need_ids] <- lapply(seq_along(cases[need_ids]), function(i) {
        cases[need_ids][[i]]@id <- i
        return(cases[need_ids][[i]])
    })
    
    # create the new categorical variable
    new_cat_type <- list(value=list(class="categorical",
                                    categories=lapply(cases, function (case) {
                    num_value <- switch(length(case@numeric_value)>0,case@numeric_value)
                                 list(id=case@id, name=name(case), 
                                      numeric_value=num_value,
                                      missing=case@missing)
                             })))
    new_cat_ids <- sapply(new_cat_type$value$categories, function(x) x$id)
    new_cat <- list(column=I(new_cat_ids), type=new_cat_type)
    
    casevar <- list(...)
    casevar$name <- name
    casevar$derivation <- zfunc("case", c(list(new_cat), lapply(cases, zcl)))
  
    # remove a level of embedding
    casevar$derivation$args <- casevar$derivation$args[[1]]
    class(casevar) <- "VariableDefinition"
    return(casevar)
}