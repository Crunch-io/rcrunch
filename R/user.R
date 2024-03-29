userURL <- function() sessionURL("user", "views")

#' My user entity
#'
#' Get the user entity of the currently authenticated user.
#'
#' @return A UserEntity
#' @export
me <- function() {
    UserEntity(crGET(userURL()))
}

getUserCatalog <- function(x = sessionURL("users")) {
    UserCatalog(crGET(x))
}

getAccount <- function(x = rootURL("account", me())) {
    ShojiObject(crGET(x))
}

#' @rdname user-email
#' @export
setMethod("email", "UserEntity", function(x) x@body$email)

#' Find all users on your account
#'
#' @param x URL of the user catalog. The default is your own user catalog and
#' you should not need to change this default.
#' @return a `UserCatalog`
#' @export
#' @keywords internal
getAccountUserCatalog <- function(x = shojiURL(getAccount(), "catalogs", "users")) {
    UserCatalog(crGET(x))
}

invite <- function(email, name = NULL, notify = TRUE, id_method = "pwhash",
                   advanced = FALSE, admin = FALSE, ...) {
    payload <- list(
        email = email,
        send_invite = notify,
        id_method = id_method,
        account_permissions = list(
            alter_users = isTRUE(admin),
            create_datasets = isTRUE(advanced)
        ),
        ...
    )
    if (!is.null(name)) {
        payload$first_name <- name
    }
    if (id_method == "pwhash") {
        payload$url_base <- "/password/change/${token}/" # nolint
    }

    url <- shojiURL(getAccount(), "catalogs", "users")
    return(crPOST(url, body = toJSON(do.call("wrapEntity", payload))))
}

#' @rdname crunch-extract
#' @export
setMethod("[", c("UserCatalog", "character"), function(x, i, ...) {
    dots <- list(x = x, i = i, secondary = emails(x))
    dots <- modifyList(dots, list(...))
    do.call("callNextMethod", dots)
})

#' @rdname crunch-extract
#' @export
setMethod("[[", c("UserCatalog", "character"), function(x, i, ...) {
    dots <- list(x = x, i = i, secondary = emails(x))
    dots <- modifyList(dots, list(...))
    do.call("callNextMethod", dots)
})

#' Reassign all Crunch objects from a user
#'
#' If you want to transfer all teams, projects, and datasets owned by one user
#' to another you can with `reassignUser`. To have permission to use
#' `reassignUser` you must be an account admin and be from the same account
#' as the user who is being reassigned. This is useful if a user leaves your
#' organization and you want to transfer all of the teams, projects, and
#' datasets they own to someone else.
#'
#' The user given in `to` will become the owner of all of the teams, projects,
#' and datasets that were previously owned by the user given in `from`.
#'
#' Reassigning requires confirmation. In an interactive session, you will be
#' asked to confirm. To avoid that prompt, or to reassign datasets from a
#' non-interactive session, wrap the call in [with_consent()] to give your
#' permission to reassign
#'
#' @param from a character of the email address of the user to reassign from
#' @param to a character of the email address of the user who should be the new
#' owner
#'
#' @return `NULL` if successful
#' @export
reassignUser <- function(from, to) {
    if (!askForPermission(paste0(
        "This will transfer ownership of all teams, ",
        "projects, and datasets from ", dQuote(from),
        " to ", dQuote(to), ". Proceed?"
    ))) {
        halt("Must confirm reassigning of datasets")
    }
    u <- getAccountUserCatalog()
    from <- UserEntity(crGET(urls(u)[emails(u) == from]))
    crPOST(shojiURL(from, "fragments", "reassign"),
        body = toJSON(wrapEntity(owner = to))
    )
}
