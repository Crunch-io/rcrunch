setMethod("initialize", "ShojiOrder", function (.Object, ..., duplicates=FALSE,
                                                catalog_url="") {
    .Object <- callNextMethod(.Object, ...)
    dots <- list(...)
    ents <- entitiesInitializer(.Object)
    if (length(dots) && !is.shoji(dots[[1]])) {
        .Object@graph <- ents(dots, url.base=NULL)
    } else {
        .Object@graph <- ents(.Object@graph, url.base=.Object@self)
    }
    duplicates(.Object) <- duplicates
    .Object@catalog_url <- catalog_url
    return(.Object)
})

setMethod("initialize", "OrderGroup", function (.Object, group, entities,
                                                url.base=NULL, duplicates=FALSE,
                                                ...) {
    dots <- list(...)
    ents <- entitiesInitializer(.Object)
    if ("variables" %in% names(dots)) entities <- dots$variables
    if ("name" %in% names(dots)) group <- dots$name
    .Object@group <- group
    .Object@entities <- ents(entities, url.base)
    duplicates(.Object) <- duplicates
    return(.Object)
})

setMethod("groupClass", "ShojiOrder", function (x) "OrderGroup")
setMethod("groupClass", "OrderGroup", function (x) "OrderGroup")
setMethod("groupClass", "VariableOrder", function (x) "VariableGroup")
setMethod("groupClass", "VariableGroup", function (x) "VariableGroup")
setMethod("groupClass", "DatasetOrder", function (x) "DatasetGroup")
setMethod("groupClass", "DatasetGroup", function (x) "DatasetGroup")
setMethod("groupClass", "ShojiOrder", function (x) "ShojiObject")
setMethod("groupClass", "OrderGroup", function (x) "ShojiObject")
setMethod("entityClass", "VariableOrder", function (x) "CrunchVariable")
setMethod("entityClass", "VariableGroup", function (x) "CrunchVariable")
setMethod("entityClass", "DatasetOrder", function (x) "CrunchDataset")
setMethod("entityClass", "DatasetGroup", function (x) "CrunchDataset")

variableGroupEntitiesInit <- function (x) {
    return(function (entities, ...) {
        if (is.dataset(entities)) {
            entities <- allVariables(entities)
        }
        return(.initEntities(entities, ..., group.class="VariableGroup",
            entity.class="CrunchVariable"))
    })
}

orderEntitiesInit <- function (x) {
    gc <- groupClass(x)
    ec <- entityClass(x)
    return(function (entities, ...) {
        return(.initEntities(entities, ..., group.class=gc, entity.class=ec))
    })
}

setMethod("entitiesInitializer", "VariableOrder", variableGroupEntitiesInit)
setMethod("entitiesInitializer", "VariableGroup", variableGroupEntitiesInit)
setMethod("entitiesInitializer", "ShojiOrder", orderEntitiesInit)
setMethod("entitiesInitializer", "OrderGroup", orderEntitiesInit)

.setNestedGroupByName <- function (x, i, j, value) {
    ents <- entitiesInitializer(x)
    w <- match(i, names(x))
    value <- ents(value)
    if (!duplicates(x)) {
        x <- setdiff_entities(x, value)
    }
    if (any(is.na(w))) {
        ## New group.
        entities(x) <- c(entities(x), do.call(groupClass(x), list(name=i, entities=value)))
    } else {
        ## Existing group. Assign entities
        entities(x[[w]]) <- value
    }
    ## Ensure duplicates setting persists
    duplicates(x) <- duplicates(x)
    return(removeMissingEntities(x))
}
##' @rdname ShojiOrder-extract
##' @export
setMethod("[[<-", c("VariableOrder", "character", "missing", "CrunchDataset"),
    .setNestedGroupByName)

##' @rdname ShojiOrder-extract
##' @export
setMethod("[[<-", c("ShojiOrder", "character", "missing", "ShojiOrder"),
    function (x, i, j, value) {
        .setNestedGroupByName(x, i, j, entities(value))
    })

##' @rdname ShojiOrder-extract
##' @export
setMethod("[[<-", c("ShojiOrder", "ANY", "missing", "OrderGroup"),
    function (x, i, j, value) {
        if (!duplicates(x) && length(entities(value))) {
            x <- setdiff_entities(x, value)
        }
        x@graph[[i]] <- value
        ## Ensure duplicates setting persists
        duplicates(x) <- duplicates(x)
        return(removeMissingEntities(x))
    })

###############################
# 4. Assign into VariableGroup
###############################

##' @rdname ShojiOrder-extract
##' @export
setMethod("[[<-", c("VariableGroup", "character", "missing", "CrunchDataset"),
    .setNestedGroupByName)

##' @rdname ShojiOrder-extract
##' @export
setMethod("[[<-", c("OrderGroup", "character", "missing", "list"),
    .setNestedGroupByName)

##' @rdname ShojiOrder-extract
##' @export
setMethod("[[<-", c("OrderGroup", "character", "missing", "character"),
    .setNestedGroupByName)

##' @rdname ShojiOrder-extract
##' @export
setMethod("[[<-", c("OrderGroup", "character", "missing", "ShojiOrder"),
    function (x, i, j, value) {
        .setNestedGroupByName(x, i, j, entities(value))
    })

##' @rdname ShojiOrder-extract
##' @export
setMethod("[[<-", c("OrderGroup", "character", "missing", "OrderGroup"),
    function (x, i, j, value) {
        .setNestedGroupByName(x, i, j, entities(value))
    })

##' Get un(grouped) VariableGroups
##'
##' "ungrouped" is a magic VariableGroup that contains all variables not found
##' in groups at a given level of nesting.
##' @param var.order an object of class VariableOrder or VariableGroup
##' @return For grouped(), a VariableOrder/Group, respectively, with "ungrouped"
##' omitted. For ungrouped(), a VariableGroup.
##' @seealso \code{\link{VariableOrder}}
##' @export
grouped <- function (var.order) {
    Filter(Negate(is.character), var.order)
}

##' @rdname grouped
##' @export
ungrouped <- function (var.order) {
    return(do.call(groupClass(var.order), list(name="ungrouped",
        entities=entities(Filter(is.character, var.order)))))
}
