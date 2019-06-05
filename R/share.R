#' Share a dataset
#'
#' @param dataset a CrunchDataset
#' @param users character: email address(es) or URLs of the users or teams with
#' whom to share the dataset. If there is no Crunch user associated with an
#' email, an invitation will be sent.
#' @param edit logical: should the specified user(s) be given edit privileges
#' on the dataset? Default is `FALSE`. `edit` can be a single value
#' or, if inviting multiple users, a vector of logical values of equal length
#' of the number of emails given.
#' @param notify logical: should users who are getting new privileges on this
#' dataset be sent an email informing them of this fact? Default is
#' `TRUE`.
#' @param message character: a message to send to the users who are receiving new
#' privileges.
#' @return Invisibly, the dataset.
#' @seealso [`unshare`]
#' @export
share <- function(dataset, users, edit = FALSE, notify = TRUE, message = NULL) {
    perms <- permissions(dataset)
    if (length(edit) == 1) {
        edit <- rep(edit, length(users))
    }
    if (length(edit) != length(users)) {
        halt(
            "Must supply `edit` permissions of equal length as the number of ",
            "`emails` supplied"
        )
    }
    if (!is.null(message) && !notify) {
        halt("Cannot send message if not notifying")
    }
    payload <- lapply(
        edit,
        function(e) list(dataset_permissions = list(edit = e, view = TRUE))
    )
    names(payload) <- users
    payload$send_notification <- notify
    if (notify) {
        payload$message <- message
        payload$url_base <- passwordSetURLTemplate()
        payload$dataset_url <- APIToWebURL(dataset)
    }
    payload <- toJSON(payload)
    crPATCH(self(perms), body = payload)
    invisible(dataset)
}

passwordSetURLTemplate <- function() {
    absoluteURL("/password/change/${token}/", getOption("crunch.api")) # nolint
}

#' Revoke a user's access to a dataset
#'
#' @param dataset a CrunchDataset
#' @param users character: email address(es) or URLs of the users or teams to
#' unshare with.
#' @return Invisibly, the dataset.
#' @seealso [`share`]
#' @export
unshare <- function(dataset, users) {
    stopifnot(is.character(users))
    payload <- sapply(users, null, simplify = FALSE)
    crPATCH(shojiURL(dataset, "catalogs", "permissions"), body = toJSON(payload))
    invisible(dataset)
}
