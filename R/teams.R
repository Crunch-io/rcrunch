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

setMethod("[[<-", c("TeamCatalog", "character", "missing", "list"),
    function (x, i, j, value) {
        if (i %in% names(x)) {
            ## TODO: update team attributes
            halt("Cannot (yet) modify team attributes")
        } else {
            ## Creating a new team
            u <- crPOST(self(x), body=toJSON(list(name=i)))
            x <- refresh(x)
            ## Add members to team, if given
            if (!is.null(value[["members"]]))
            members(x[[i]]) <- value[["members"]]
            return(x)
        }
    })

setMethod("[[<-", c("TeamCatalog", "character", "missing", "CrunchTeam"),
    function (x, i, j, value) {
        ## TODO: something
        return(x)
    })

setMethod("$", "TeamCatalog", function (x, name) x[[name]])

setMethod("name", "CrunchTeam", function (x) x@body$name)

setMethod("members", "CrunchTeam", function (x) {
    MemberCatalog(crGET(shojiURL(x, "catalogs", "members")))
})

setMethod("names", "MemberCatalog", function (x) getIndexSlot(x, "display_name"))

setMethod("members<-", c("CrunchTeam", "character"), function (x, value) {
    ## value can be URL or email
    ## TODO: grep for @, assume urls if not @?
    are.emails <- grep("@", value)
    ## look them up
    if (length(are.emails)) {
        ucat <- getAccountUserCatalog()
        urls <- urls(ucat)[match(value[are.emails], emails(ucat))]
        if (any(is.na(urls))) {
            plural <- sum(is.na(urls)) > 1
            halt("Could not find user", ifelse(plural, "s", ""), 
                " associated with ",
                serialPaste(value[are.emails][is.na(urls)]))
        }
        value[are.emails] <- urls
    }

    payload <- sapply(value, 
        function (x) structure(list(), .Names=character(0)),
        simplify=FALSE)
    crPATCH(self(members(x)), body=toJSON(payload))
    return(refresh(x))
})
