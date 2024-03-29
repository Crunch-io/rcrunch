context("GitHub version check")

with(temp.option(crunch = list(crunch.check.updates = CRUNCH_OPT_DEFAULT)), {
    with_mock_crunch({
        without_interactive({
            test_that("checkForNewVersion doesn't check if session is not interactive", {
                expect_null(checkForNewVersion("foo", "1.5.1"))
            })
        })

        with_interactive({
            test_that("checkForNewVersion parses github json", {
                expect_identical(
                    checkForNewVersion("foo", "1.5.3"),
                    NULL
                )
                expect_identical(
                    checkForNewVersion("foo", "1.5.1"),
                    "1.5.3"
                )
                expect_identical(
                    checkForNewVersion("foo", "1.6.3"),
                    NULL
                )
            })

            with(temp.option(crunch = list(crunch.check.updates = FALSE)), {
                test_that("checkForNewVersion doesn't check if option is set", {
                    expect_null(checkForNewVersion("foo", "1.5.1"))
                })
            })

            test_that("notifyIfNewVersion messages correctly", {
                expect_message(
                    notifyIfNewVersion("crunch", "foo", "1.5.1"),
                    "There's a new version"
                )
                expect_silent(notifyIfNewVersion("crunch", "foo"))
            })
        })
    })

    without_internet({
        test_that("notifyIfNewVersion doesn't hang if GitHub doesn't respond", {
            expect_message(
                uncached(notifyIfNewVersion("crunch", "foo", "1.5.1")),
                NA
            )
        })
    })
})
