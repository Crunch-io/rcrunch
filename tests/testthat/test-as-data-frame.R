context("Getting values to make local R objects")

ca.values <- data.frame(
    subvar2=structure(c(2L, 2L, 1L, NA, 1L, 2L, 1L, 2L, 2L, 2L, NA, 2L, NA, NA,
        1L, 1L, 2L, 2L, 2L, 1L, NA, 1L, NA, NA, 1L),
        levels=c("A", "B"), class="factor"),
    subvar1=structure(c(1L, 1L, 2L, NA, NA, NA, NA, 1L, 2L, NA, NA, 1L, NA, NA,
        NA, 1L, 2L, 1L, 1L, 1L, NA, 1L, NA, 1L, 2L),
        levels=c("A", "B"), class="factor"),
    subvar3=structure(c(1L, NA, 1L, 2L, NA, 1L, 2L, NA, 1L, NA, 2L, 1L, 1L, 2L,
        1L, 1L, 2L, 1L, NA, 1L, 2L, 1L, 1L, 1L, 2L),
        levels=c("A", "B"), class="factor"))

mr.values <- data.frame(
    subvar2=structure(c(2L, 2L, 1L, NA, 1L, 2L, 1L, 2L, 2L, 2L, NA, 2L, NA, NA,
        1L, 1L, 2L, 2L, 2L, 1L, NA, 1L, NA, NA, 1L),
        levels=c("0.0", "1.0"), class="factor"),
    subvar1=structure(c(1L, 1L, 2L, NA, NA, NA, NA, 1L, 2L, NA, NA, 1L, NA, NA,
        NA, 1L, 2L, 1L, 1L, 1L, NA, 1L, NA, 1L, 2L),
        levels=c("0.0", "1.0"), class="factor"),
    subvar3=structure(c(1L, NA, 1L, 2L, NA, 1L, 2L, NA, 1L, NA, 2L, 1L, 1L, 2L,
        1L, 1L, 2L, 1L, NA, 1L, 2L, 1L, 1L, 1L, 2L),
        levels=c("0.0", "1.0"), class="factor"))

mr.ids <- data.frame(
    subvar2=c(2, 2, 1, -1, 1, 2, 1, 2, 2, 2, -1, 2, -1, -1,
        1, 1, 2, 2, 2, 1, -1, 1, -1, -1, 1),
    subvar1=c(1, 1, 2, -1, -1, -1, -1, 1, 2, -1, -1, 1, -1, -1,
        -1, 1, 2, 1, 1, 1, -1, 1, -1, 1, 2),
    subvar3=c(1, -1, 1, 2, -1, 1, 2, -1, 1, -1, 2, 1, 1, 2,
        1, 1, 2, 1, -1, 1, 2, 1, 1, 1, 2))

with_mock_crunch({
    ds <- loadDataset("test ds")
    test_that("setup", {
        expect_identical(dim(ds), c(nrow(ds), ncol(ds)))
        expect_identical(dim(ds), c(25L, 7L))
        expect_identical(names(ds),
            c("birthyr", "gender", "location", "mymrset", "textVar", "starttime", "catarray"))
    })

    test_that("as.vector on Variables", {
        expect_true(is.numeric(as.vector(ds$birthyr)))
        expect_true(is.factor(as.vector(ds$gender)))
        expect_true(all(levels(as.vector(ds$gender)) %in% names(categories(ds$gender))))
    })

    test_that("as.vector on Categorical Array", {
        expect_true(is.CA(ds$catarray))
        expect_true(is.data.frame(as.vector(ds$catarray)))
        expect_identical(as.vector(ds$catarray), ca.values)
    })

    test_that("as.vector on Multiple Response", {
        expect_true(is.MR(ds$mymrset))
        expect_true(is.data.frame(as.vector(ds$mymrset)))
        expect_identical(as.vector(ds$mymrset), mr.values)
        ## Check that getting subvar by $ from the as.vector of the array
        ## is the same as just getting the subvar as.vector directly
        expect_identical(as.vector(ds$mymrset)$subvar1,
            as.vector(ds$mymrset$subvar1))
    })

    test_that("as.vector on Multiple Response with mode", {
        expect_identical(as.vector(ds$mymrset, mode="id"), mr.ids)
        expect_identical(as.vector(ds$mymrset, mode="id")$subvar1,
            as.vector(ds$mymrset$subvar1, mode="id"))
    })

    test_that("as.data.frame on CrunchDataset yields CrunchDataFrame", {
        expect_false(is.data.frame(as.data.frame(ds)))
        expect_is(as.data.frame(ds), "CrunchDataFrame")
        expect_identical(dim(as.data.frame(ds)), c(25L, ncol(ds)))
        expect_identical(names(as.data.frame(ds)), names(ds))
        expect_identical(as.data.frame(ds)$birthyr, as.vector(ds$birthyr))
        expect_identical(evalq(gender, as.data.frame(ds)),
            as.vector(ds$gender))
    })

    test_that("as.data.frame(force = TRUE) generates a POST", {
        expect_POST(as.data.frame(ds, force = TRUE),
            'https://app.crunch.io/api/datasets/1/export/csv/',
            '{"filter":null,"options":{"use_category_ids":true}}')
    })
    csv_df <- read.csv("dataset-fixtures/test_ds.csv", stringsAsFactors = FALSE)
    test_that("csvToDataFrame produces the correct data frame", {
        expected <- readRDS("dataset-fixtures/test_ds.rds")
        cdf <- as.data.frame(ds[, c("birthyr", "gender", "location", "mymrset", "textVar", "starttime")])
        #test local CDF variables
        cdf$newvar <- expected$newvar <- c(1:24, NA)
        expect_identical(csvToDataFrame(csv_df, cdf), expected)
    })

    test_that("csvToDataFrame handles hidden variables", {
        new_ds <- loadDataset("test ds")[, c("birthyr", "gender", "location", "mymrset", "textVar", "starttime")]
        new_ds$birthyr@tuple[["discarded"]] <- TRUE
        new_ds_df <- as.data.frame(new_ds, include.hidden = FALSE)
        expect_silent(
            expect_equal(names(csvToDataFrame(csv_df, new_ds_df)),
                c("gender", "location", "subvar2", "subvar1", "subvar3", "textVar",
                    "starttime"))
        )
        # now we want the hidden vars to be included
        new_ds_df <- as.data.frame(new_ds, include.hidden = TRUE)
        expect_warning(
            expect_equal(names(csvToDataFrame(csv_df, new_ds_df)),
                c("birthyr", "gender", "location", "subvar2", "subvar1", "subvar3", "textVar",
                    "starttime")),
            paste0("Variable birthyr is hidden"))
    })

    test_that("as.data.frame when a variable has an apostrophe in its alias", {
        t2 <- ds
        t2@variables@index[[2]]$alias <- "Quote 'unquote' alias"
        expect_is(as.data.frame(t2), "CrunchDataFrame")
    })

    test_that("as.data.frame(as.data.frame())", {
        expect_POST(write.csv(ds, file=""),
            'https://app.crunch.io/api/datasets/1/export/csv/',
            '{"filter":null,"options":{"use_category_ids":false}}')
    })

    test_that("as.data.frame() works with hidden variables", {
        new_ds <- loadDataset("test ds")
        new_ds$gender@tuple[["discarded"]] <- TRUE
        expect_equivalent(hiddenVariables(new_ds), "gender")
        new_ds_df <- as.data.frame(new_ds)
        expect_equal(names(new_ds_df),
                     aliases(variables(new_ds)))
        expect_equal(ncol(new_ds_df), 6)

        # now we want the hidden vars to be includes
        new_ds_df <- as.data.frame(new_ds, include.hidden = TRUE)
        expect_equal(names(new_ds_df),
                     aliases(allVariables(new_ds)))
        expect_equal(ncol(new_ds_df), 7)
    })

    test_that(".crunchPageSize", {
        expect_identical(.crunchPageSize(ds$birthyr), 100000L)
        expect_identical(.crunchPageSize(ds$gender), 200000L)
        expect_identical(.crunchPageSize(ds$textVar), 5000L)
        expect_identical(.crunchPageSize(ds$mymrset), 66666L)
        expect_identical(.crunchPageSize(ds$catarray), 66666L)
        expect_identical(.crunchPageSize(ds$starttime), 100000L)
        expect_identical(.crunchPageSize(2016 - ds$birthyr), 50000L)
    })


    test.df <- as.data.frame(ds)


    test_that("model.frame thus works on CrunchDataset", {
        expect_identical(model.frame(birthyr ~ gender, data=test.df),
                         model.frame(birthyr ~ gender, data=ds))
    })


    test_that("so lm() should work too", {
        test.lm <- lm(birthyr ~ gender, data=ds)
        expected <- lm(birthyr ~ gender, data=test.df)
        expect_is(test.lm, "lm")
        expect_identical(names(test.lm), names(expected))
        for (i in setdiff(names(expected), "call")) {
            expect_identical(test.lm[[i]], expected[[i]])
        }
    })
})

with_test_authentication({
    ds <- newDataset(df)
    test_that("Check the types of our imported data", {
        expect_true(is.Numeric(ds[["v1"]]))
        expect_true(is.Text(ds[["v2"]]))
        expect_true(is.Numeric(ds[["v3"]]))
        expect_true(is.Categorical(ds[["v4"]]))
        expect_true(is.Datetime(ds$v5))
    })
    test_that("as.vector on Numeric", {
        expect_true(is.numeric(as.vector(ds$v1)))
        expect_identical(sum(is.na(as.vector(ds$v1))), 5L)
        expect_equivalent(as.vector(ds$v1), df$v1)
    })
    test_that("as.vector on Text", {
        expect_true(is.character(as.vector(ds$v2)))
        expect_identical(sum(is.na(as.vector(ds$v2))), 5L)
        expect_equivalent(as.vector(ds$v2), df$v2)
    })
    test_that("as.vector on a different Numeric", {
        expect_true(is.numeric(as.vector(ds$v3)))
        expect_equivalent(as.vector(ds$v3), df$v3)
    })
    ## Test on a version with missings
    ds$v4b <- df$v4
    ds$v4b[3:5] <- "No Data"
    values(categories(ds$v4b)[1:2]) <- c(5, 3)
    test_that("as.vector on a Categorical", {
        expect_true(is.factor(as.vector(ds$v4b)))
        expect_equivalent(as.vector(ds$v4b)[-(3:5)], df$v4[-(3:5)])
        expect_true(all(is.na(as.vector(ds$v4b)[3:5])))
    })
    test_that("as.vector with mode specified on Categorical", {
        expect_identical(as.vector(ds$v4b, mode="id"),
            c(1, 2, -1, -1, -1, 2, rep(1:2, 7)))
        expect_identical(as.vector(ds$v4b, mode="numeric"),
            c(5, 3, NA, NA, NA, 3, rep(c(5, 3), 7)))
    })
    ## Delete v4b for the later tests (should just make a fresh ds)
    with(consent(), ds$v4b <- NULL)

    test_that("as.vector on Datetime", {
        expect_is(as.vector(ds$v5), "Date")
        expect_equivalent(as.vector(ds$v5), df$v5)
    })

    test_that("as.data.frame with API", {
        expect_false(is.data.frame(as.data.frame(ds)))
        expect_is(as.data.frame(ds), "CrunchDataFrame")
        expect_identical(dim(as.data.frame(ds)), dim(df))
        expect_identical(names(as.data.frame(ds)), names(df))
        expect_identical(as.data.frame(ds)$v1, as.vector(ds$v1))
    })

    test_that("as.data.frame(force) with API", {
        skip_locally("Vagrant host doesn't serve files correctly")
        expect_true(is.data.frame(as.data.frame(as.data.frame(ds))))
        expect_true(is.data.frame(as.data.frame(ds, force=TRUE)))
    })

    ds$hidden_var <- 1:20
    ds <- hideVariables(ds, "hidden_var")

    test_that("as.data.frame(force) pulls hidden variables when include.hidden is set", {
        skip_locally("Vagrant host doesn't serve files correctly")
        expect_equal(hiddenVariables(ds), "hidden_var")

        df <- as.data.frame(ds, force = TRUE)
        expect_equal(names(df), c("v1", "v2", "v3", "v4", "v5", "v6"))

        expect_warning(
            df <- as.data.frame(ds, force = TRUE, include.hidden = TRUE),
            "Variable hidden_var is hidden"
        )
        expect_equal(names(df), c("v1", "v2", "v3", "v4", "v5", "v6", "hidden_var"))
    })

    test_that("as.data.frame(force) includes  hidden variables when specified and include.hidden isn't set", {
        skip_locally("Vagrant host doesn't serve files correctly")
        expect_equal(hiddenVariables(ds), "hidden_var")
        expect_warning(
            df <- as.data.frame(ds[, c("v1", "hidden_var")], force = TRUE),
            "Variable hidden_var is hidden"
        )
        expect_equal(names(df),
            c("v1", "hidden_var"))
    })

    test_that("Multiple response variables in as.data.frame(force=TRUE)", {
        skip_locally("Vagrant host doesn't serve files correctly")
        mrds <- mrdf.setup(newDataset(mrdf, name = "test-mrdfmr"), selections = "1.0")
        mrds_df <- as.data.frame(mrds, force = TRUE)
        expect_equal(ncol(mrds_df), 4)
        expect_equal(names(mrds_df), c("mr_1", "mr_2", "mr_3", "v4"))
        expect_equal(mrds_df$mr_1, as.vector(mrds$MR$mr_1))
        expect_equal(mrds_df$mr_2, as.vector(mrds$MR$mr_2))
        expect_equal(mrds_df$mr_3, as.vector(mrds$MR$mr_3))
        expect_equal(mrds_df$v4, as.vector(mrds$v4))
    })

    v2 <- ds$v2
    with_consent(delete(v2))
    test_that("CrunchDataFrame lazily fetches columns", {
        expect_true("v2" %in% names(ds)) ## ds is stale
        expect_is(as.data.frame(ds), "CrunchDataFrame")
        ## This should error because it will try to get values for v2
        expect_error(as.data.frame(ds, force=TRUE))
    })

    uncached({
        with_mock(`crunch::.crunchPageSize`=function (x) 5L, {
            with(temp.option(httpcache.log=""), {
                avlog <- capture.output(v1 <- as.vector(ds$v1))
            })
            test_that("getValues can be paginated", {
                logdf <- loadLogfile(textConnection(avlog))
                ## GET entity to get /values/ URL, then GET /values/ 4x
                ## to get data, then a 5th GET /values/ that returns 0
                ## values, which breaks the pagination loop
                expect_identical(logdf$verb, rep("GET", 6))
                expect_identical(grep("values", logdf$url), 2:6)
            })
            test_that("getValues returns the same result when paginated", {
                expect_equivalent(v1, df$v1)
            })
        })
    })

    test_that("model.frame thus works on CrunchDataset over API", {
        ## would like this to be "identical" instead of "equivalent"
        expect_equivalent(model.frame(v1 ~ v3, data=ds),
                          model.frame(v1 ~ v3, data=df))
    })

    test_that("so lm() should work too over the API", {
        test.lm <- lm(v1 ~ v3, data=ds)
        expected <- lm(v1 ~ v3, data=df)
        expect_is(test.lm, "lm")
        expect_identical(names(test.lm), names(expected))
        ## would like this to be "identical" instead of "equivalent"
        for (i in setdiff(names(expected), "call")) {
            expect_equivalent(test.lm[[i]], expected[[i]])
        }
    })
})
