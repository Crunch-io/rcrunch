
setMethod("groupClass", "VariableOrder", function(x) "VariableGroup")
setMethod("groupClass", "VariableGroup", function(x) "VariableGroup")
setMethod("entityClass", "VariableOrder", function(x) "CrunchVariable")
setMethod("entityClass", "VariableGroup", function(x) "CrunchVariable")

# Special methods for adding a subset of variables as Dataset to a VariableOrder

variableGroupEntitiesInit <- function(x) {
    return(function(entities, ...) {
        if (is.dataset(entities)) {
            entities <- allVariables(entities)
        }
        return(.initEntities(entities, ...,
            group.class = "VariableGroup",
            entity.class = "CrunchVariable"
        ))
    })
}

setMethod("entitiesInitializer", "VariableOrder", variableGroupEntitiesInit)
setMethod("entitiesInitializer", "VariableGroup", variableGroupEntitiesInit)

#' @rdname crunch-extract
#' @export
setMethod(
    "[[<-", c("VariableOrder", "character", "missing", "CrunchDataset"),
    .setNestedGroupByName
)

#' @rdname crunch-extract
#' @export
setMethod(
    "[[<-", c("VariableGroup", "character", "missing", "CrunchDataset"),
    .setNestedGroupByName
)
