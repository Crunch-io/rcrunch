context("Polling progress")

test_that("progressMessage", {
    with(temp.option(crunch.show.progress=FALSE), {
        expect_silent(progressMessage("Message!"))
    })
    with(temp.option(crunch.show.progress=NULL), {
        expect_message(progressMessage("Message!"), "Message!")
    })
})

with_mock_crunch({
    options(crunch.show.progress=NULL)
    on.exit(options(crunch.show.progress=FALSE))
    test_that("If progress polling gives up, it tells you what to do", {
        with(temp.option(crunch.timeout=0.0005), {
            expect_error(
                expect_output(pollProgress("https://app.crunch.io/api/progress/1/", wait=0.001),
                    "|================"),
                paste('Your process is still running on the server. It is',
                    'currently 23% complete. Check',
                    '`pollProgress("https://app.crunch.io/api/progress/1/")`',
                    'until it reports 100% complete'),
                fixed=TRUE)
        })
    })

    ## Setup to test the auto-polling
    fakeProg <- function (progress_url) {
        return(fake_response(progress_url,
            status_code=202,
            headers=list(
                location="https://app.crunch.io/api/datasets/",
                `Content-Type`="application/json"
            ),
            content=list(element="shoji:view", value=progress_url)))
    }

    counter <- 1
    with_mock(
        ## GET something slightly different each time through so we can
        ## approximate polling a changing resource
        `httr::GET`=function (url, ...) {
            url <- build_mock_url(paste0(url, counter)) ## Add counter
            counter <<- counter + 1 ## Increment
            return(fake_response(url, "GET",
                content=readBin(url, "raw", 4096), ## Assumes mock is under 4K
                status_code=200, headers=list(`Content-Type`="application/json")))
        },
        test_that("Progress polling goes until 100 and has a newline", {
            ## Use capture.output rather than expect_output because the latter
            ## concatenates things together and does not easily capture the effect
            ## of closing the progress bar, which is another item on the buffer.
            out <- capture.output(
                expect_equal(pollProgress("https://app.crunch.io/api/progress/", wait=.001),
                             100), cat('command on next line'))
            expect_equal(length(out), 2)
            expect_match(out[1], "=| 100%", fixed=TRUE)
            expect_match(out[2], "command on next line", fixed=TRUE)
        }),
        test_that("Progress polling goes until 100 when silent", {
            counter <<- 1
            with(temp.option(crunch.show.progress=FALSE), {
                expect_silent(
                    expect_equal(pollProgress("https://app.crunch.io/api/progress/",
                                              wait=.001),
                        100
                    )
                )
            })
        }),
        test_that("Auto-polling with a progress resource", {
            counter <<- 1
            logfile <- tempfile()
            with(temp.option(httpcache.log=logfile), {
                expect_output(
                    expect_identical(handleAPIresponse(fakeProg("https://app.crunch.io/api/progress/")),
                        "https://app.crunch.io/api/datasets/"),
                    "=| 100%", fixed=TRUE)
            })
            logs <- loadLogfile(logfile)
            expect_identical(logs$verb, c("GET", "GET"))
            expect_identical(logs$url,
                c("app.crunch.io/api/progress/1.json", "app.crunch.io/api/progress/2.json"))
        }),
        test_that("Auto-polling when progress reports failure", {
            counter <<- 1
            logfile <- tempfile()
            with(temp.option(httpcache.log=logfile), {
                expect_output(
                    expect_message(
                        expect_error(handleAPIresponse(fakeProg("https://app.crunch.io/api/progress2/")),
                            paste("Education, Commerce, and, uh, oops."), fixed=TRUE),
                        "Result URL: https://app.crunch.io/api/datasets/"),
                    "|  23%", fixed=TRUE)
            })
            logs <- loadLogfile(logfile)
            expect_identical(logs$verb, c("GET", "GET"))
            expect_identical(logs$url,
                c("app.crunch.io/api/progress2/1.json", "app.crunch.io/api/progress2/2.json"))
        })
    )
})
