context("Timings")


skip({
if (!run.only.local.tests) {
    with(test.authentication, {
        with(test.dataset(), {
            ds <- .setup
            with(timed.HTTP("timings.tsv"), {
                for (i in as.vector(outer(letters, letters, FUN=paste0))) {
                    ds[[i]] <- rnorm(5)
                }
            })
        })
    })
}
})