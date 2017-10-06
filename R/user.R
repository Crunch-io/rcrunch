userURL <- function () rootURL("user")

#' My user entity
#'
#' Get the user entity of the currently authenticated user.
#'
#' @return A UserEntity
#' @export
me <- function () {
    UserEntity(crGET(userURL()))
}

getUserCatalog <- function (x=sessionURL("users")) {
    UserCatalog(crGET(x))
}

getAccount <- function (x=rootURL("account", me())) {
    ShojiObject(crGET(x))
}

setMethod("email", "UserEntity", function (x) x@body$email)

#' Find all users on your account
#'
#' @param x URL of the user catalog. The default is your own user catalog and
#' you should not need to change this default.
#' @return a `UserCatalog`
#' @export
#' @keywords internal
getAccountUserCatalog <- function (x=shojiURL(getAccount(), "catalogs", "users")) {
    UserCatalog(crGET(x))
}

invite <- function (email, name=NULL, notify=TRUE, id_method="pwhash",
                    advanced=FALSE, admin=FALSE, ...) {
    payload <- list(
        email=email,
        send_invite=notify,
        id_method=id_method,
        account_permissions=list(
            alter_users=isTRUE(admin),
            create_datasets=isTRUE(advanced)),
        ...)
    if (!is.null(name)) {
        payload$first_name <- name
    }
    if (id_method == "pwhash") {
        payload$url_base <- "/password/change/${token}/"
    }

    url <- shojiURL(getAccount(), "catalogs", "users")
    return(crPOST(url, body=toJSON(do.call("wrapEntity", payload))))
}

#' Reset your password
#'
#' Trigger the password reset process. Password reset
#' instructions will be emailed to you.
#'
#' @param email Your email
#' @return NULL, invisibly. Called for its side effects.
#' @export
#' @examples
#' \dontrun{
#' resetPassword("me@example.com")
#' }
resetPassword <- function (email) {
    app <- getOption("crunch.api")
    invisible(crPOST(absoluteURL("public/password_reset/", app),
        body=toJSON(list(
            email=email,
            url_base=absoluteURL("../password/change/${token}/", app)
        ))))
}

expropriateUser <- function (from, to) {
    u <- getAccountUserCatalog()
    from <- UserEntity(crGET(urls(u)[emails(u) == from]))
    crPOST(shojiURL(from, "fragments", "expropriate"),
        body=toJSON(wrapEntity(owner=to)))
}
