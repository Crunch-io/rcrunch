context("Polling progress")

with_mock_HTTP({
    test_that("If progress polling gives up, it tells you what to do", {
        with(temp.option(crunch.timeout=0.0005), {
            expect_error(
                expect_output(pollProgress("https://app.crunch.io/api/progress/1/", wait=0.001),
                    "|================"),
                paste('Your process is still running on the server. It is',
                    'currently 23% complete. Check',
                    '`httpcache::uncached(crGET("https://app.crunch.io/api/progress/1/"))`',
                    'until it reports 100% complete'),
                fixed=TRUE)
        })
    })

    ## Setup to test the auto-polling
    fakeProg <- function (progress_url) {
        return(fakeResponse(status_code=202,
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
            url <- httptest::buildMockURL(paste0(url, counter)) ## Add counter
            counter <<- counter + 1 ## Increment
            return(fakeResponse(url, "GET",
                content=readBin(url, "raw", 4096), ## Assumes mock is under 4K
                status_code=200, headers=list(`Content-Type`="application/json")))
        },
        test_that("Progress polling goes until 100", {
            expect_output(
                expect_equal(pollProgress("https://app.crunch.io/api/progress/", wait=.001),
                    100),
                "=| 100%", fixed=TRUE)
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
                c("https://app.crunch.io/api/progress/1.json", "https://app.crunch.io/api/progress/2.json"))
        }),
        test_that("Auto-polling when progress reports failure", {
            counter <<- 1
            logfile <- tempfile()
            with(temp.option(httpcache.log=logfile), {
                expect_output(
                    expect_message(
                        expect_error(handleAPIresponse(fakeProg("https://app.crunch.io/api/progress2/")),
                            paste("Education, Commerce, and, uh, oops."), fixed=TRUE),
                        "Result URL: api/datasets/"),
                    "|  23%", fixed=TRUE)
            })
            logs <- loadLogfile(logfile)
            expect_identical(logs$verb, c("GET", "GET"))
            expect_identical(logs$url,
                c("https://app.crunch.io/api/progress2/1.json", "https://app.crunch.io/api/progress2/2.json"))
        })
    )
})
