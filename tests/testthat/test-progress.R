context("Polling progress")

with_mock_HTTP({
    test_that("If progress polling gives up, it tells you what to do", {
        with(temp.option(crunch.timeout=0.005), {
            expect_error(
                expect_output(pollProgress("/api/progress/1.json", wait=0.01),
                    "|================"),
                paste('Your process is still running on the server. It is',
                    'currently 23% complete. Check',
                    '`httpcache::uncached(crGET("/api/progress/1.json"))`',
                    'until it reports 100% complete'),
                fixed=TRUE)
        })
    })

    ## Setup to test the auto-polling
    fake_response <- structure(list(status_code=202,
            headers=list(
                `Content-Type`="application/json",
                location="/api/datasets.json"
            ),
            content=charToRaw(toJSON(list(
                    element="shoji:view",
                    value="/api/progress/"
                )))),
        class="response")

    counter <- 1
    with_mock(
        ## GET something slightly different each time through so we can
        ## approximate polling a changing resource
        `httr::GET`=function (url, ...) {
            url <- paste0(url, counter, ".json") ## Add counter
            counter <<- counter + 1 ## Increment
            url <- sub("^\\/", "", url) ## relative to cwd
            return(fakeResponse(url))
        },
        test_that("Progress polling goes until 100", {
            expect_output(
                expect_identical(pollProgress("/api/progress/", wait=.01),
                    100),
                "=| 100%", fixed=TRUE)
        }),
        test_that("Auto-polling with a progress resource", {
            counter <<- 1
            logfile <- tempfile()
            with(temp.option(httpcache.log=logfile), {
                expect_output(
                    expect_identical(handleAPIresponse(fake_response),
                        "/api/datasets.json"),
                    "=| 100%", fixed=TRUE)
            })
            logs <- loadLogfile(logfile)
            expect_identical(logs$verb, c("GET", "GET"))
            expect_identical(logs$url,
                c("api/progress/1.json", "api/progress/2.json"))
        })
    )
})
