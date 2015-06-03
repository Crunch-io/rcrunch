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
