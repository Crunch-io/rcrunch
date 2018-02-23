#' @importFrom httpcache uncached
pollProgress <- function (progress_url, wait=.5) {
    ## Configure polling interval. Will increase by rate (>1) until reaches max
    max.wait <- 30
    increase.by <- 1.2

    starttime <- Sys.time()
    timeout <- crunchTimeout()
    timer <- function (since, units="secs") {
        difftime(Sys.time(), since, units=units)
    }
    ## Set up the progress bar
    pb <- setup_progress_bar(0, 100, style=3)

    prog <- uncached(crGET(progress_url))
    status <- prog$progress
    update_progress_bar(pb, status)
    while (status >= 0 && status < 100 && timer(starttime) < timeout) {
        Sys.sleep(wait)
        prog <- uncached(crGET(progress_url))
        status <- prog$progress
        update_progress_bar(pb, status)
        wait <- min(max.wait, wait * increase.by)
    }
    close(pb)

    if (status < 0) {
        msg <- prog$message %||% "There was an error on the server. Please contact support@crunch.io"
        halt(msg)
    } else if (status != 100) {
        ## TODO: export pollProgress and reference it in the message below
        halt('Your process is still running on the server. It is currently ',
            round(status), '% complete. Check `httpcache::uncached(crGET("',
            progress_url, '"))` until it reports 100% complete')
    }
    return(status)
}

#' @importFrom utils txtProgressBar
setup_progress_bar <- function (...) {
    if (isTRUE(getOption("crunch.show.progress", TRUE))) txtProgressBar(...)
}

#' @importFrom utils setTxtProgressBar
update_progress_bar <- function (...) {
    if (isTRUE(getOption("crunch.show.progress", TRUE))) setTxtProgressBar(...)
}

crunchTimeout <- function () {
    opt <- getOption("crunch.timeout")
    if (!is.numeric(opt)) opt <- 900
    return(opt)
}
