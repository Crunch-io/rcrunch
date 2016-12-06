#' See who has access to this dataset
#'
#' @param x CrunchDataset
#' @return A PermissionCatalog containing information on the users and teams
#' that have access to this dataset.
#' @name permissions
#' @aliases permissions
NULL

#' @rdname permissions
#' @export
setMethod("permissions", "CrunchDataset", function (x) {
    perm_url <- shojiURL(x, "catalogs", "permissions")
    return(PermissionCatalog(crGET(perm_url)))
})

#' @rdname is.editor
#' @export
setMethod("is.editor", "PermissionCatalog", function (x) {
    out <- vapply(index(x), function (a) {
            isTRUE(a[["dataset_permissions"]][["edit"]])
        }, logical(1), USE.NAMES=FALSE)
    names(out) <- emails(x) ## Drop this
    return(out)
})

#' @rdname is.editor
#' @export
setMethod("is.editor", "PermissionTuple", function (x) {
    isTRUE(x[["dataset_permissions"]][["edit"]])
})

#' Share a dataset
#'
#' @param dataset a CrunchDataset
#' @param users character: email address(es) or URLs of the users or teams with
#' whom to share the dataset. If there is no Crunch user associated with an
#' email, an invitation will be sent.
#' @param edit logical: should the specified user(s) be given edit privileges
#' on the dataset? Default is \code{FALSE}. \code{edit} can be a single value
#' or, if inviting multiple users, a vector of logical values of equal length
#' of the number of emails given.
#' @param notify logical: should users who are getting new privileges on this
#' dataset be sent an email informing them of this fact? Default is
#' @param message character: what message should users who are getting notified
#' about new permissions on this dataset receive?
#' \code{TRUE}.
#' @return Invisibly, the dataset.
#' @seealso \code{\link{unshare}}
#' @export
share <- function (dataset, users, edit=FALSE, notify=TRUE, message=NA) {
    perms <- permissions(dataset)
    if (length(edit) == 1) {
        edit <- rep(edit, length(users))
    }
    if (length(edit) != length(users)) {
        halt("Must supply `edit` permissions of equal length as the number of `emails` supplied")
    }
    if (!is.na(message) & !notify){
        halt("Cannot send message if not notifying")
    }
    payload <- lapply(edit,
        function (e) list(dataset_permissions=list(edit=e, view=TRUE)))
    names(payload) <- users
    payload$send_notification <- notify
    if (!is.na(message)) payload$message <- message
    if (notify) {
        payload$url_base <- passwordSetURLTemplate()
        payload$dataset_url <- webURL(dataset)
    }
    payload <- toJSON(payload)
    crPATCH(self(perms), body=payload)
    invisible(dataset)
}

passwordSetURLTemplate <- function () {
    absoluteURL("/password/change/${token}/", getOption("crunch.api"))
}

#' Revoke a user's access to a dataset
#'
#' @param dataset a CrunchDataset
#' @param users character: email address(es) or URLs of the users or teams to
#' unshare with.
#' @return Invisibly, the dataset.
#' @seealso \code{\link{share}}
#' @export
unshare <- function (dataset, users) {
    stopifnot(is.character(users))
    payload <- sapply(users, null, simplify=FALSE)
    crPATCH(shojiURL(dataset, "catalogs", "permissions"), body=toJSON(payload))
    invisible(dataset)
}

## TODO: test and release this
# shareDataset <- function (x, emails, notify=TRUE) {
#     ## Share one more more datasets without loading them
#     dscat <- active(datasets())
#     if (!is.numeric(x)) {
#         x <- selectDatasetFromCatalog(x, dscat, strict=TRUE)
#     }
#     dsurls <- urls(dscat)[x]
#     perm_urls <- vapply(dsurls, function (x) absoluteURL("permissions/", x),
#         character(1))
#
#     payload <- sapply(emails,
#             function (x) list(dataset_permissions=list(edit=FALSE, view=TRUE)),
#             simplify=FALSE)
#     payload$send_notification <- notify
#     payload <- toJSON(payload)
#
#     out <- lapply(perm_urls, function (x) {
#         message("Sharing ", x)
#         crPATCH(x, body=payload)
#     })
#     invisible(out)
# }
