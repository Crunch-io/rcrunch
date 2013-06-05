context("Getting values to make local R objects")

## Variable fake fixtures for dataset
test.ds <- .cr.dataset.shojiObject(as.shojiObject(ds), vars2)

with(fake.HTTP, {
    test_that("as.vector on Variables", {
        expect_true(is.numeric(getValues(vars2$age)))
        expect_false(is.factor(getValues(vars2$gender)))
        expect_true(all(getValues(vars2$gender) %in% names(categories(vars2$gender))))
        expect_true(is.factor(as.vector(vars2$gender)))
        expect_true(all(levels(as.vector(vars2$gender)) %in% names(categories(vars2$gender))))
    })

    test_that("as.data.frame on CrunchDataset", {
        expect_true(is.data.frame(as.data.frame(test.ds)))
        expect_identical(dim(as.data.frame(test.ds)), c(25L, length(test.ds)))
        expect_identical(names(as.data.frame(test.ds)), names(test.ds))
        expect_identical(as.data.frame(test.ds)$age, as.vector(vars2$age))
    })
    
    test.df <- as.data.frame(test.ds)
    
    test_that("model.frame thus works on CrunchDataset", {
        expect_identical(model.frame(age ~ gender, data=test.df),
            model.frame(age ~ gender, data=test.ds))
    })
    
    test_that("so lm() should work too", {
        test.lm <- lm(age ~ gender, data=test.ds)
        expected <- lm(age ~ gender, data=test.df)
        expect_true(inherits(test.lm, "lm"))
        expect_identical(names(test.lm), names(expected))
        for (i in setdiff(names(expected), "call")) {
            expect_identical(test.lm[[i]], expected[[i]])
        }
    })
})

if (!run.only.local.tests) {
    ## Do this with real data
}

