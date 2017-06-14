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



makeCaseVariable <- function (cases, name, ...) {
    #check if all cases are cases.
    
    # make ids if cases don't have ids.

    # create the new categorical variable
    new_cat_type <- list(value=list(class="categorical",
                                    categories=lapply(seq_along(cases), function (i) {
                                 list(id=i, name=name(cases[[i]]), numeric_value=NULL, missing=FALSE)
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