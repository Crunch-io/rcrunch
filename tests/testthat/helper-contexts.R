with_fake_input <- function(input, expr) {
    with_mock(
        `crunch:::is.interactive` = function() return(TRUE),
        `crunch:::read_input` = function(...) input,
        eval.parent(expr)
    )
}

uniqueDatasetName <- now

## Create a test dataset and then destroy it after tests
objects_to_purge <- c()
new.dataset.with.setup <- function(df = NULL, ...) {
    unique.name <- uniqueDatasetName()
    if (is.dataset(df)) {
        ## Passing a dataset already made in, just to ensure its cleanup
        ## Just return it
        out <- df
    } else if (is.null(df)) {
        out <- createDataset(name = unique.name, ...)
    } else {
        out <- suppressMessages(newDataset(df, name = unique.name, ...))
    }
    objects_to_purge <<- c(objects_to_purge, self(out))
    return(out)
}

purge.object <- function() {
    len <- length(objects_to_purge)
    if (len) {
        try(crDELETE(objects_to_purge[len]), silent = TRUE)
        objects_to_purge <<- objects_to_purge[-len]
    }
}

test.dataset <- function(df = NULL, obj.name = "ds", ...) {
    return(ContextManager(
        function() new.dataset.with.setup(df, ...),
        purge.object,
        as = obj.name
    ))
}

reset.option <- function(opts) {
    ## Don't set any options in the setup, but reset specified options after
    old <- sapply(opts, getOption, simplify = FALSE)
    return(ContextManager(
        null,
        function() do.call(options, old)
    ))
}

uniqueEmail <- function() paste0("test+", as.numeric(Sys.time()), "@crunch.io")
testUser <- function(email = uniqueEmail(), name = paste("Ms.", email, "User"), ...) {
    u.url <- invite(email, name = name, notify = FALSE, ...)
    return(UserEntity(crGET(u.url)))
}
