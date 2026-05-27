# This maybe should be moved to rcrunch, or `pollProgress` should be
# configurable so that it can behave like this
# The data lake export is apparently the first API that has non-error content
# on the progress result.
# (Usually progress has a location in the header of the original,
# the only other time we've had to mess with progress results
# is for CrunchAutomation, which only has errors in the progress)
pollProgressContent <- function(progress_url, wait = .5, error_handler = NULL) {
    max.wait <- 30
    increase.by <- 1.2

    starttime <- Sys.time()
    timeout <- crunch:::crunchTimeout()
    timer <- function(since, units = "secs") {
        difftime(Sys.time(), since, units = units)
    }
    ## Set up the progress bar
    pb <- crunch:::setup_progress_bar(0, 100, style = 3)

    prog <- httpcache::uncached(crunch::crGET(progress_url))
    status <- prog$progress
    crunch:::update_progress_bar(pb, status)
    while (status >= 0 && status < 100 && timer(starttime) < timeout) {
        Sys.sleep(wait)
        prog <- httpcache::uncached(crunch::crGET(progress_url))
        status <- prog$progress
        crunch:::update_progress_bar(pb, status)
        wait <- min(max.wait, wait * increase.by)
    }
    close(pb)

    if (status < 0 && !is.null(error_handler)) {
        return(error_handler(prog))
    } else if (status < 0) {
        email <- "There was an error on the server. Please contact support@crunch.io"
        msg <- prog$message %||% email
        httpcache::halt(msg)
    } else if (status != 100) {
        httpcache::halt(
            "Your process is still running on the server. It is currently ",
            round(status), '% complete. Check `pollProgress("',
            progress_url, '")` until it reports 100% complete'
        )
    }
    return(prog)
}
