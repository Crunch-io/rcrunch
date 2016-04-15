init.VariableOrder <- function (.Object, ..., duplicates=FALSE, catalog_url="") {
    .Object <- callNextMethod(.Object, ...)
    dots <- list(...)
    if (length(dots) && !is.shoji(dots[[1]])) {
        .Object@graph <- variableGroupEntities(dots, url.base=NULL)
    } else {
        .Object@graph <- variableGroupEntities(.Object@graph, url.base=.Object@self)
    }
    duplicates(.Object) <- duplicates
    .Object@catalog_url <- catalog_url
    return(.Object)
}
setMethod("initialize", "VariableOrder", init.VariableOrder)

init.VariableGroup <- function (.Object, group, entities, url.base=NULL, duplicates=FALSE, ...) {
    dots <- list(...)
    if ("variables" %in% names(dots)) entities <- dots$variables
    if ("name" %in% names(dots)) group <- dots$name
    .Object@group <- group
    .Object@entities <- variableGroupEntities(entities, url.base)
    duplicates(.Object) <- duplicates
    return(.Object)
}
setMethod("initialize", "VariableGroup", init.VariableGroup)

variableGroupEntities <- function (entities, ...) {
    if (is.dataset(entities)) {
        return(variableGroupEntities(allVariables(entities), ...))
    }
    return(.initEntities(entities, ..., group.class="VariableGroup",
        entity.class="CrunchVariable"))
}

.setNestedGroupByName <- function (x, i, j, value) {
    w <- match(i, names(x))
    value <- variableGroupEntities(value)
    if (!duplicates(x)) {
        x <- setdiff_entities(x, value)
    }
    if (any(is.na(w))) {
        ## New group.
        entities(x) <- c(entities(x), VariableGroup(name=i, entities=value))
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
setMethod("[[<-", c("VariableOrder", "character", "missing", "VariableOrder"),
    function (x, i, j, value) {
        .setNestedGroupByName(x, i, j, entities(value))
    })

##' @rdname ShojiOrder-extract
##' @export
setMethod("[[<-", c("VariableOrder", "ANY", "missing", "VariableGroup"),
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
setMethod("[[<-", c("VariableGroup", "character", "missing", "list"),
    .setNestedGroupByName)

##' @rdname ShojiOrder-extract
##' @export
setMethod("[[<-", c("VariableGroup", "character", "missing", "character"),
    .setNestedGroupByName)

##' @rdname ShojiOrder-extract
##' @export
setMethod("[[<-", c("VariableGroup", "character", "missing", "VariableOrder"),
    function (x, i, j, value) {
        .setNestedGroupByName(x, i, j, entities(value))
    })

##' @rdname ShojiOrder-extract
##' @export
setMethod("[[<-", c("VariableGroup", "character", "missing", "VariableGroup"),
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
    return(VariableGroup(name="ungrouped",
        entities=entities(Filter(is.character, var.order))))
}
