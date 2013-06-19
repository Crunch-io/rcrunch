## To insulate us from API nomenclature and changes to
CATEGORY_NAME_MAP = list(
    name="name",
    value="code",
    id="_id"
)

validCategories <- function (object) {
    are.cats <- vapply(object, is.category, logical(1))
    if (!all(are.cats)) {
        badcount <- sum(!are.cats)
        val <- paste0("Invalid categories: ", badcount, 
            ifelse(badcount>1, 
                " elements are not Crunch category objects.", 
                " element is not a Crunch category object."))
    } else {
        val <- TRUE  
    }
    return(val)
}
setValidity("Categories", validCategories)

init.Categories <- function (.Object, ...) {
    .Object@.Data <- lapply(..1, Category)
    return(.Object)
}
setMethod("initialize", "Categories", init.Categories)

is.categories <- function (x) inherits(x, "Categories")

setMethod("names", "Categories", function (x) vapply(x, name, character(1)))
setGeneric("values", function (x) standardGeneric("values"))
setMethod("values", "Categories", function (x) {
    out <- try(vapply(x, value, numeric(1)), silent=TRUE)
    if (is.error(out)) out <- rep(list(NULL), length(x))
    return(out)
})
setGeneric("ids", function (x) standardGeneric("ids"))
setMethod("ids", "Categories", function (x) {
    # vapply(x, id, character(1))
    sapply(x, id)
})
## for summaries
setMethod("ids", "list", function (x) {
    # vapply(x, id, character(1))
    sapply(x, id)
})

setNames <- function (x, value) {
    mapply(setName, x, value=value)
}
setValues <- function (x, value) {
    mapply(setValue, x, value=value)
}

setMethod("names<-", "Categories", setNames)
setGeneric("values<-", function (x, value) standardGeneric("values<-"))
setMethod("values<-", "Categories", setValues)
setMethod("toJSON", "Categories", function (x, ...) toJSON(I(x@.Data)))

#####

is.category <- function (x) inherits(x, "Category")

validCategory <- function (object) {
    is.cat <- all(c("_id", "name") %in% names(object))
    if (!all(is.cat)) {
        val <- "Not a category"
    } else {
        val <- TRUE  
    }
    return(val)
}
setValidity("Category", validCategory)

setName <- function (x, value) {
    x[[CATEGORY_NAME_MAP[["name"]]]] <- value
    return(x)
}
setValue <- function (x, value) {
    x[[CATEGORY_NAME_MAP[["value"]]]] <- value
    return(x)
}

setMethod("$", "Category", function (x, name) callNextMethod())
setMethod("$<-", "Category", function (x, name, value) callNextMethod())
setMethod("name", "Category", function (x) x[[CATEGORY_NAME_MAP[["name"]]]])
setMethod("name<-", "Category", setName)
setGeneric("value", function (x) standardGeneric("value"))
setMethod("value", "Category", function (x) x[[CATEGORY_NAME_MAP[["value"]]]])
setGeneric("value<-", function (x, value) standardGeneric("value<-"))
setMethod("value<-", "Category", setValue)

setGeneric("id", function (x) standardGeneric("id"))
setMethod("id", "Category", function (x) x[[CATEGORY_NAME_MAP[["id"]]]])
setMethod("id", "list", function (x) x[[CATEGORY_NAME_MAP[["id"]]]])
