context("GitHub version check")

with(temp.option(crunch.check.updates=NULL), {
    with_mock_crunch({
        test_that("checkForNewVersion parses github json", {
            expect_identical(checkForNewVersion("github-versions/", "1.5.3"),
                NULL)
            expect_identical(checkForNewVersion("github-versions/", "1.5.1"),
                "1.5.3")
            expect_identical(checkForNewVersion("github-versions/", "1.6.3"),
                NULL)
            ## Now that version is greater than 1.5.3:
            expect_null(checkForNewVersion("github-versions/"))
        })

        with(temp.option(crunch.check.updates=FALSE), {
            test_that("checkForNewVersion doesn't check if option is set", {
                expect_null(checkForNewVersion("github-versions/", "1.5.1"))
            })
        })

        test_that("notifyIfNewVersion messages correctly", {
            expect_message(notifyIfNewVersion("github-versions/", "1.5.1"),
                "There's a new version")
            expect_silent(notifyIfNewVersion("github-versions/"))
        })
    })

    without_internet({
        test_that("notifyIfNewVersion doesn't hang if GitHub doesn't respond", {
            expect_message(uncached(notifyIfNewVersion("github-versions/", "1.5.1")),
                NA)
        })
    })
})
