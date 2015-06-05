userURL <- function () rootURL("user")

getUser <- function (x=userURL()) {
    ShojiObject(crGET(x))
}

getAccount <- function (x=rootURL("account", getUser())) {
    ShojiObject(crGET(x))
}

getAccountUserCatalog <- function (x=rootURL("users", getAccount())) {
    UserCatalog(crGET(x))
}

setMethod("names", "UserCatalog", function (x) getIndexSlot(x, "full_name"))

setMethod("emails", "UserCatalog", function (x) getIndexSlot(x, "email"))

invite <- function (email, name=NULL, notify=TRUE, id_method="pwhash", ...) {
    ## TODO: Add permissions
    payload <- list(email=email, send_invite=notify, id_method=id_method, ...)
    if (nchar(name)) {
        payload$first_name <- name
    }
    if (id_method == "pwhash") {
        payload$url_base <- "/password/change/${token}/"
    }
    
    url <- rootURL("users", getAccount())
    return(crPOST(url, body=toJSON(list(element="shoji:entity", body=payload))))
}