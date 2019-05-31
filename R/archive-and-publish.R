#' Get and set "archived" and "published" status of a dataset
#'
#' "Archived" datasets are excluded from some views. "Draft" datasets are
#' visible only to editors, while published datasets are available to all viewers.
#' A dataset can either be published or in draft, but not both.
#' These properties are accessed and set with the "is" methods. You can also
#' set the properties by assigning into the function. The verb functions
#' `archive` and `publish` are alternate versions of the setters.
#'
#' @param x CrunchDataset
#' @param value logical
#' @return For the getters, the logical value of whether the dataset is
#' archived, in draft mode, or published, where draft and published are
#' inverses. The setters return the dataset.
#' @name archive-and-publish
#' @aliases archive is.archived is.draft is.published is.archived<- is.draft<- is.published<- publish
#' @examples
#' \dontrun{
#' ds <- loadDataset("mtcars")
#' is.draft(ds)     # FALSE
#' is.published(ds) # TRUE
#' identical(is.draft(ds), !is.published(ds))
#' # Can make a dataset a "draft" by:
#' is.draft(ds) <- TRUE
#' is.published(ds) # FALSE
#' # Could also have set is.published(ds) <- FALSE
#' # Now, can go the other way by setting is.draft, is.published, or:
#' ds <- publish(ds)
#' is.published(ds) # TRUE
#'
#' is.archived(ds)  # FALSE
#' is.archived(ds) <- TRUE
#' is.archived(ds)  # TRUE
#' # Could have achieved the same effect by:
#' ds <- archive(ds)
#' }
setGeneric("is.archived", function(x) standardGeneric("is.archived"))
#' @rdname archive-and-publish
setGeneric("is.archived<-", function(x, value) standardGeneric("is.archived<-"))
#' @rdname archive-and-publish
setGeneric("is.draft", function(x) standardGeneric("is.draft"))
#' @rdname archive-and-publish
setGeneric("is.draft<-", function(x, value) standardGeneric("is.draft<-"))
#' @rdname archive-and-publish
setGeneric("is.published", function(x) standardGeneric("is.published"))
#' @rdname archive-and-publish
setGeneric("is.published<-", function(x, value) standardGeneric("is.published<-"))

#' @rdname archive-and-publish
#' @export
setMethod("is.archived", "CrunchDataset", function(x) tuple(x)$archived)
#' @rdname archive-and-publish
#' @export
setMethod("is.draft", "CrunchDataset", function(x) !is.published(x))
#' @rdname archive-and-publish
#' @export
setMethod("is.published", "CrunchDataset", function(x) tuple(x)$is_published %||% TRUE)

#' @rdname archive-and-publish
#' @export
setMethod("is.archived<-", c("CrunchDataset", "logical"), function(x, value) {
    stopifnot(is.TRUEorFALSE(value))
    setEntitySlot(x, "archived", value)
})
#' @rdname archive-and-publish
#' @export
archive <- function(x) {
    is.archived(x) <- TRUE
    return(x)
}
#' @rdname archive-and-publish
#' @export
setMethod("is.draft<-", c("CrunchDataset", "logical"), function(x, value) {
    stopifnot(is.TRUEorFALSE(value))
    setEntitySlot(x, "is_published", !value)
})
#' @rdname archive-and-publish
#' @export
setMethod("is.published<-", c("CrunchDataset", "logical"), function(x, value) {
    stopifnot(is.TRUEorFALSE(value))
    setEntitySlot(x, "is_published", value)
})
#' @rdname archive-and-publish
#' @export
publish <- function(x) {
    is.published(x) <- TRUE
    return(x)
}

#' @rdname archive-and-publish
#' @export
setMethod("is.archived", "DatasetCatalog",
    function(x) getIndexSlot(x, "archived", logical(1))
)
#' @rdname archive-and-publish
#' @export
setMethod("is.draft", "DatasetCatalog", function(x) !is.published(x))
#' @rdname archive-and-publish
#' @export
setMethod("is.published", "DatasetCatalog",
    function(x) getIndexSlot(x, "is_published", logical(1), ifnot = TRUE)
)

#' @rdname archive-and-publish
#' @export
setMethod("is.archived<-", c("DatasetCatalog", "logical"), function(x, value) {
    setIndexSlot(x, "archived", value)
})
#' @rdname archive-and-publish
#' @export
setMethod("is.draft<-", c("DatasetCatalog", "logical"), function(x, value) {
    setIndexSlot(x, "is_published", !value)
})
#' @rdname archive-and-publish
#' @export
setMethod("is.published<-", c("DatasetCatalog", "logical"), function(x, value) {
    setIndexSlot(x, "is_published", value)
})
