## To insulate us from API nomenclature and changes to
CATEGORY_NAME_MAP = list(
    name="name",
    value="code",
    id="id"
)

is.category <- function (x) inherits(x, "Category")

validCategory <- function (object) {
    is.cat <- all(c('id', "name") %in% names(object))
    if (!all(is.cat)) {
        val <- "Not a category"
    } else {
        val <- TRUE  
    }
    return(val)
}
setValidity("Category", validCategory)

init.Category <- function (.Object, ...) {
    .Object <- callNextMethod()
    ## Make sure category elements are sorted so that identical categories are
    ## evaluated identically. Order doesn't matter for object, but R lists are
    ## ordered.
    s <- order(.Object@names)
    .Object@.Data <- .Object@.Data[s]
    .Object@names <- .Object@names[s]
    return(.Object)
}
setMethod("initialize", "Category", init.Category)

setName <- function (x, value) {
    x[[CATEGORY_NAME_MAP[["name"]]]] <- value
    return(x)
}
setValue <- function (x, value) {
    value_to_set <- suppressWarnings(as.numeric(value))
    if (is.na(value_to_set) && !is.na(value)) {
        stop("Category values must be numeric", call.=FALSE)
    }
    x[[CATEGORY_NAME_MAP[["value"]]]] <- value_to_set
    return(x)
}

setMethod("$", "Category", function (x, name) x[[name]])
setMethod("$<-", "Category", function (x, name, value) {
    x[[name]] <- value
    return(x)
})

setMethod("name", "Category", function (x) x[[CATEGORY_NAME_MAP[["name"]]]])
setMethod("name<-", "Category", setName)
setMethod("value", "Category", function (x) {
    v <- x[[CATEGORY_NAME_MAP[["value"]]]]
    return(ifelse(is.null(v), NA_real_, as.numeric(v)))
})
setMethod("value<-", "Category", setValue)

setMethod("id", "Category", function (x) x[[CATEGORY_NAME_MAP[["id"]]]])
setMethod("id", "list", function (x) x[[CATEGORY_NAME_MAP[["id"]]]])

show.values <- function (x) TRUE ## make this actually do something? need to point at variable, not categories, or otherwise embed that attribute in the categories object.

showCategory <- function (x) {
    out <- name(x)
    if (show.values(x)) out <- paste0("[ ", value(x), " ]  ", out)
    return(out)
}

setMethod("show", "Category", function (object) {
    out <- showCategory(object)
    cat(out)
    invisible(out)
})

setMethod("is.selected", "Category", function (x) isTRUE(x$selected))