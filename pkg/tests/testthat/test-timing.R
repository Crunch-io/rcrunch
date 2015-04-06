context("Timings")


skip({
if (run.integration.tests) {
    with(test.authentication, {
        with(test.dataset(), {
            with(timed.HTTP("hierarchical_timings.tsv"), {
                for (i in as.vector(outer(letters, letters, FUN=paste0))) {
                    ds[[i]] <- rnorm(5)
                }
            })
        })
    })
}
})


mockArrayDataset <- function (nvars, nsubvars=5, nrows=3) {
    ds <- createDataset(name=paste("Arrays", crunch:::now()))
    for (i in as.vector(outer(letters, letters, FUN=paste0))[seq_len(nvars)]) {
        crunch:::POSTNewVariable(variableCatalogURL(ds), list(
            name=i,
            type="categorical_array",
            subvariables=lapply(seq_len(nsubvars),
                function (x) toVariable(as.factor(seq_len(nrows)), 
                    name=paste(i, x, sep="_")))
        ))
    }
    invisible(refresh(ds))
}

