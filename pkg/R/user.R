getUser <- function (x=sessionURL("user_url")) {
    ShojiObject(GET(x))
}