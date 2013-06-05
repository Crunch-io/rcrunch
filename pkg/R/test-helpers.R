setup.and.teardown <- function (setup, teardown) {
    structure(list(setup=setup, teardown=teardown), class="SUTD")
}

with.SUTD <- function (data, expr, ...) {
    on.exit(data$teardown())
    data$setup()
    eval(substitute(expr), enclos = parent.frame())
}