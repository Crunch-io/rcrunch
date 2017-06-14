context("Variable types")

with_mock_crunch({
    ds <- loadDataset("test ds")

    test_that("Variable type method", {
        expect_identical(type(ds[["birthyr"]]), "numeric")
        expect_identical(type(ds$gender), "categorical")
    })

    test_that("Changing numeric type by <- makes requests", {
        expect_POST(type(ds$birthyr) <- "categorical",
            'https://app.crunch.io/api/datasets/1/variables/birthyr/cast/',
            '{"cast_as":"categorical"}')
        expect_POST(type(ds$birthyr) <- "text",
            'https://app.crunch.io/api/datasets/1/variables/birthyr/cast/',
            '{"cast_as":"text"}')
    })
    test_that("Setting the same type is a no-op", {
        expect_no_request(type(ds$birthyr) <- "numeric")
    })
    test_that("Attempting to set an unsupported type fails", {
        for (i in c("multiple_response", "categorical_array", "datetime", "foo")) {
            expect_error(type(ds$birthyr) <- i,
                "is not a Crunch variable type that can be assigned",
                info=i)
        }
    })

    test_that("Changing multiple_response type by <- fails", {
        for (i in c("categorical", "text", "numeric", "categorical_array", "datetime", "foo")) {
            expect_error(type(ds$mymrset) <- i,
                "Cannot change the type of a MultipleResponseVariable by type<-",
                info=i)
        }
    })
})

with_test_authentication({
    test_that("Type changing alters data on the server", {
        ds <- newDataset(df[,1,drop=FALSE])
        
        testvar <- ds$v1
        expect_true(is.Numeric(testvar))
        type(testvar) <- "text"
        expect_true(is.Text(testvar))

        ds <- refresh(ds)
        expect_true(is.Text(ds$v1))
        type(ds$v1) <- "numeric"
        expect_true(is.Numeric(ds$v1))
    })
})
