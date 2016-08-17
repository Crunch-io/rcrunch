#' @rdname makeArray
#' @export
makeMR <- function (subvariables, dataset=NULL, name, selections, ...) {
    if (missing(selections)) {
        halt("Must provide the names of the category or categories that ",
            "indicate the dichotomous selection")
    }

    ## Do `makeArray` to build the definition.
    vardef <- makeArray(subvariables=subvariables, dataset=dataset, name=name,
        selected_categories=I(selections), ...)
    vardef$type <- "multiple_response"
    ## We're done. But let's do some validation first.

    ## Get the actual variables so that we can validate
    vars <- lapply(vardef$subvariables, function (u) VariableEntity(crGET(u)))
    are.categorical <- vapply(vars,
        function (x) isTRUE(x@body$type == "categorical"), ## Make a type method?
        logical(1))
    if (!all(are.categorical)) {
        varnames <- vapply(vars[!are.categorical],
            function (x) x@body$name, ## Make a name() method for VariableEntity
            character(1))
        halt(serialPaste(varnames),
            " are not Categorical variables. Convert them to ",
            "Categorical before combining to Multiple Response")
    }

    ## Validate selections before binding
    catnames <- unique(unlist(lapply(vars, function (y) names(categories(y)))))
    if (!all(selections %in% catnames)) {
        halt("Selection(s) not found in variable's categories. ",
            "Category names are: ", serialPaste(catnames))
        ## Could return more useful messaging here
    }

    return(vardef)
}
