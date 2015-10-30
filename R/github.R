checkForNewVersion <- function (
    github.url="https://api.github.com/repos/Crunch-io/rcrunch/tags",
    installed.version=as.character(packageVersion("crunch"))) {

    ## Get the names of the tagged versions on GitHub
    gh.tags <- vapply(crGET(github.url), function (x) x[["name"]], character(1))
    ## Filter to keep only those that match x.y.z
    version.tags <- grep("^[0-9]+\\.[0-9]+\\.[0-9]+$", gh.tags, value=TRUE)
    
    if (length(version.tags) && 
        versionIsGreaterThan(version.tags[1], installed.version) ) {
        
        return(version.tags[1])
    }
    return(NULL)
}

versionIsGreaterThan <- function (x, y) {
    if (x == y) return(FALSE)
    ## Split into major/minor/patch, and as.numeric. Then do the same for
    ## installed.version. Confirm that latest on GitHub is indeed newer
}