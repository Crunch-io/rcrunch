context("Multiple response cube specialness")

with_test_authentication({
    ds <- newDatasetFromFixture("apidocs")
    test_that("as_selected in cube query", {
        cub <- as.array(crtabs(~ as_selected(allpets), data=ds))
        expect_identical(dimnames(cub),
            list(
                allpets=c("Cat", "Dog", "Bird"),
                allpets=c("Selected", "Not Selected")
            ))
    })
})
