getUser <- function (x=sessionURL("user_url")) {
    ShojiObject(crGET(x))
}