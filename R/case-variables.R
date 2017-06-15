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

#' Make a case variable
#' 
#' The `makeCaseVariable` function derives a variable using values from other 
#' variables. These are evaluated in the order they are supplied in the list 
#' as the `cases` argument (they proceed in an IF, ELSE IF, ELSE IF, ..., ELSE 
#' fashion); the first one that matches selects the corresponding value from 
#' the [`Case`] object.
#' 
#' @param cases A list of objects of class [`Case`]
#' @param ... other properties to pass about the case variable (e.g. name)
makeCaseVariable <- function (cases, ...) {
    # check if all cases are cases.
    if (any(sapply(cases, class) != "Case")) {
        halt("All elements of the cases arugment must be of class Case")
    }

    # make ids if cases don't have ids.
    need_ids <- sapply(cases, function(x) length(x@id) == 0)
    cases[need_ids] <- lapply(seq_along(cases[need_ids]), function(i) {
        cases[need_ids][[i]]@id <- i
        return(cases[need_ids][[i]])
    })

    # create the new categorical variable
    new_cat_type <- list(
        value=list(class="categorical",
        categories=lapply(cases, function (case) {
            # if the numeric_value is empty, then use null
            num_value <- switch(length(case@numeric_value)>0,case@numeric_value)
            list(id=case@id, name=name(case),
                 numeric_value=num_value,
                 missing=case@missing)
        })))
    new_cat_ids <- sapply(new_cat_type$value$categories, function(x) x$id)
    new_cat <- list(column=I(new_cat_ids), type=new_cat_type)
    
    casevar <- list(...)
    casevar$derivation <- zfunc("case", c(list(new_cat), lapply(cases, zcl)))

    # remove a level of duplicate embedding
    casevar$derivation$args <- casevar$derivation$args[[1]]

    class(casevar) <- "VariableDefinition"
    return(casevar)
}