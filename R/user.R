userURL <- function () rootURL("user")

getUser <- function (x=userURL()) {
    ShojiObject(crGET(x))
}