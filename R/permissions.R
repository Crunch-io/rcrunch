#' Manage access to datasets and other objects
#'
#' These methods allow you to work with teams.
#'
#' @param x `CrunchDataset`, `ProjectFolder`, or `CrunchTeam`
#' @param value for `members<-`, a character vector of emails or URLs of
#' users to add to the team.
#' @return `members()` returns a
#' `MemberCatalog`, which has references to the users that are members
#' of the team. `members<-` returns `x` with the given users added
#' to the members catalog. `permissions()` returns a `PermissionCatalog` with
#' similar semantics.
#' @aliases members members<-
#' @seealso [users()]
#' @name members
#' @aliases members members<- permissions
setGeneric("members", function(x) standardGeneric("members"))
#' @rdname members
setGeneric("members<-", function(x, value) standardGeneric("members<-"))

#' @rdname members
#' @export
permissions <- function(x) {
    stopifnot(is.dataset(x))
    perm_url <- shojiURL(x, "catalogs", "permissions")
    return(PermissionCatalog(crGET(perm_url)))
}

#' @rdname members
#' @export
setMethod("members", "ProjectFolder", function(x) {
    MemberCatalog(crGET(shojiURL(x, "catalogs", "members")))
})

#' @rdname members
#' @export
setMethod("members<-", c("ProjectFolder", "MemberCatalog"), function(x, value) {
    ## TODO: something
    ## For now, assume action already done in other methods, like NULL
    ## assignment above.
    return(x)
})

#' @rdname members
#' @export
setMethod("members", "CrunchTeam", function(x) {
    MemberCatalog(crGET(shojiURL(x, "catalogs", "members")))
})

#' @rdname members
#' @export
setMethod("members<-", c("ProjectFolder", "character"), function(x, value) {
    value <- setdiff(value, emails(members(x)))
    if (length(value)) {
        payload <- sapply(value, emptyObject, simplify = FALSE)
        crPATCH(self(members(x)), body = toJSON(payload))
    }
    return(x)
})

#' @rdname members
#' @export
setMethod("members<-", c("CrunchTeam", "MemberCatalog"), function(x, value) {
    ## TODO: something
    ## For now, assume action already done in other methods, like NULL
    ## assignment above.
    return(x)
})

#' @rdname members
#' @export
setMethod("members<-", c("CrunchTeam", "character"), function(x, value) {
    payload <- sapply(value, emptyObject, simplify = FALSE)
    crPATCH(self(members(x)), body = toJSON(payload))
    return(refresh(x))
})


#' View and modify the "public" attribute of artifacts
#'
#' View and modify whether all dataset viewers have access to the dataset. This
#' will return `FALSE` if the dataset is in draft.
#'
#' @param x a Crunch object
#' @param value an attribute to set
#' @return For `is.public`, a logical value for whether the object is
#' flagged as shared with all dataset viewers. (Its setter thus takes a
#' logical value as well.) Catalogs of datasets return a vector of logicals
#' corresponding to the length of the catalog, while entities return a single value.
#' @name is-public
#' @aliases is.public<- is.public
setGeneric("is.public", function(x) standardGeneric("is.public"))
#' @rdname is-public
setGeneric("is.public<-", function(x, value) standardGeneric("is.public<-"))

#' @rdname is-public
#' @export
setMethod("is.public", "CrunchFilter", function(x) x@body$is_public)

#' @rdname is-public
#' @export
setMethod("is.public<-", "CrunchFilter", function(x, value) {
    stopifnot(is.TRUEorFALSE(value))
    setEntitySlot(x, "is_public", value)
})

#' @rdname is-public
#' @export
setMethod("is.public", "CrunchDeck", function(x) x@body$is_public)
#' @rdname is-public
#' @export
setMethod("is.public<-", "CrunchDeck", function(x, value) {
    stopifnot(is.TRUEorFALSE(value))
    setEntitySlot(x, "is_public", value)
})
#' @rdname is-public
#' @export
setMethod("is.public", "MultitableCatalog", function(x) {
    getIndexSlot(x, "is_public", what = logical(1))
})

#' @rdname is-public
#' @export
setMethod("is.public<-", "MultitableCatalog", function(x, value) {
    setIndexSlotOnEntity(x, "is_public", value, what = logical(1))
})

#' @rdname is-public
#' @export
setMethod("is.public", "Multitable", function(x) x@body$is_public)

#' @rdname is-public
#' @export
setMethod("is.public<-", "Multitable", function(x, value) {
    stopifnot(is.TRUEorFALSE(value))
    setEntitySlot(x, "is_public", value)
})

#' Read and set edit privileges
#'
#' @param x PermissionCatalog or MemberCatalog
#' @param value For the setter, logical: should the indicated users be allowed
#' to edit the associated object?
#' @return `is.editor` returns a logical vector corresponding to whether
#' the users in the catalog can edit or not.`is.editor<-` returns the
#' catalog, modified.
#' @name is.editor
#' @aliases is.editor is.editor<-
setGeneric("is.editor", function(x) standardGeneric("is.editor"))
#' @rdname is.editor
setGeneric("is.editor<-", function(x, value) standardGeneric("is.editor<-"))

#' @rdname is.editor
#' @export
setMethod("is.editor", "MemberCatalog", function(x) {
    ## N.B.: this is for projects; teams don't work this way.
    vapply(index(x), function(a) {
        isTRUE(a[["permissions"]][["edit"]])
    }, logical(1), USE.NAMES = FALSE)
})

#' @rdname is.editor
#' @export
setMethod("is.editor<-", c("MemberCatalog", "logical"), function(x, value) {
    stopifnot(length(x) == length(value))
    changed <- is.editor(x) != value
    if (any(changed)) {
        payload <- structure(lapply(value[changed], {
            function(v) list(permissions = list(edit = v))
        }), .Names = urls(x)[changed])
        crPATCH(self(x), body = toJSON(payload))
        x <- refresh(x)
    }
    return(x)
})

#' @rdname is.editor
#' @export
setMethod("is.editor", "PermissionCatalog", function(x) {
    out <- vapply(index(x), function(a) {
        isTRUE(a[["dataset_permissions"]][["edit"]])
    }, logical(1), USE.NAMES = FALSE)
    names(out) <- emails(x) ## Drop this
    return(out)
})

#' @rdname is.editor
#' @export
setMethod("is.editor", "PermissionTuple", function(x) {
    isTRUE(x[["dataset_permissions"]][["edit"]])
})
