##' @importFrom httpcache uncached
##' @importFrom utils txtProgressBar setTxtProgressBar
pollProgress <- function (progress_url, wait=1) {
    ## TODO: Take Location url, and on.exit(print(it)) so that user can interrupt
    ## progress polling

    ## Configure polling interval. Will increase by rate (>1) until reaches max
    max.wait <- 30
    increase.by <- 1.2

    starttime <- Sys.time()
    timeout <- crunchTimeout()
    timer <- function (since, units="secs") {
        difftime(Sys.time(), since, units=units)
    }
    ## Set up the progress bar
    pb <- txtProgressBar(0, 100, style=3)

    status <- uncached(as.numeric(crGET(progress_url)$progress))
    setTxtProgressBar(pb, status)
    while (status < 100 && timer(starttime) < timeout) {
        Sys.sleep(wait)
        status <- uncached(as.numeric(crGET(progress_url)$progress))
        setTxtProgressBar(pb, status)
        wait <- min(max.wait, wait * increase.by)
    }

    if (status != 100) {
        halt('Your process is still running on the server. It is currently ',
            round(status), '% complete. Check `httpcache::uncached(crGET("',
            progress_url, '"))` until it reports 100% complete')
    }
    return(status)
}
