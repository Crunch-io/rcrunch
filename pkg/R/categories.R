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

setMethod("[", c("Categories", "ANY"), function (x, i, ...) {
    x@.Data <- x@.Data[i]
    return(x)
})
setMethod("[<-", c("Categories", "ANY"), function (x, i, ..., value) {
    x@.Data[i] <- value
    return(x)
})
setMethod("names", "Categories", function (x) vapply(x, name, character(1)))
setGeneric("values", function (x) standardGeneric("values"))
setMethod("values", "Categories", function (x) {
    # out <- as.numeric(unlist(lapply(x, value)))
    # out <- try(vapply(x, value, numeric(1)), silent=TRUE)
    # if (is.error(out)) out <- rep(list(NULL), length(x))
    return(vapply(x, value, numeric(1)))
    # return(out)
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
    x[] <- mapply(setName, x, value=value, SIMPLIFY=FALSE)
    return(x)
}
setValues <- function (x, value) {
    x[] <- mapply(setValue, x[], value=value, SIMPLIFY=FALSE)
    return(x)
}

setMethod("names<-", "Categories", setNames)
setGeneric("values<-", function (x, value) standardGeneric("values<-"))
setMethod("values<-", "Categories", setValues)
setMethod("toJSON", "Categories", function (x, ...) toJSON(I(x@.Data)))
