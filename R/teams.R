getTeams <- function () {
    TeamCatalog(crGET(sessionURL("teams")))
}

setMethod("names", "TeamCatalog", function (x) getIndexSlot(x, "name"))

setMethod("[[", c("TeamCatalog", "character"), function (x, i, ...) {
    stopifnot(length(i) == 1)
    z <- match(i, names(x))
    if (is.na(z)) {
        return(NULL)
    }
    return(x[[z]])
})

setMethod("[[", c("TeamCatalog", "numeric"), function (x, i, ...) {
    stopifnot(length(i) == 1)
    url <- urls(x)[i]
    return(CrunchTeam(crGET(url)))
})

setMethod("$", "TeamCatalog", function (x, name) x[[name]])

setMethod("name", "CrunchTeam", function (x) x@body$name)

setMethod("members", "CrunchTeam", function (x) {
    MemberCatalog(crGET(shojiURL(x, "catalogs", "members")))
})

setMethod("names", "MemberCatalog", function (x) getIndexSlot(x, "display_name"))
