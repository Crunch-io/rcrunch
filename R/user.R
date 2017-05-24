userURL <- function () rootURL("user")

#' My user entity
#'
#' @return A UserEntity that corresponds to you, the authenticated user
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
#' @param x URL of the user catalog. Default is the right thing; you shouldn't
#' specify one
#' @return a \code{UserCatalog}
#' @export
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
