context("Getting values to make local R objects")



with_mock_crunch({
    ds <- cachedLoadDataset("Vegetables example")
    ds_dup <- loadDataset(paste0(envOrOption("crunch.api"), "datasets/dup"))
    test_that("setup", {
        expect_identical(nrow(ds), 210L)
        expect_identical(ncol(ds), 12L)

        expect_identical(
            names(ds),
            c(
                "wave", "age", "healthy_eater", "enjoy_mr", "veg_enjoy_ca",
                "ratings_numa", "funnel_aware_mr", "funnel_consider_mr", "funnel_buy_mr",
                "weight", "last_vegetable", "last_vegetable_date"

            )
        )
    })

    test_that("as.vector on Numeric Variable", {
        expect_true(is.numeric(as.vector(ds$age)))
    })

    test_that("as.vector on Categorical Variable", {
        expect_true(is.factor(as.vector(ds$healthy_eater)))
        expect_true(all(
            levels(as.vector(ds$healthy_eater)) %in% names(categories(ds$healthy_eater))
        ))
    })

    test_that("as.vector on Categorical Array", {
        expect_true(is.CA(ds$veg_enjoy_ca))
        vec <- as.vector(ds$veg_enjoy_ca)
        expect_true(is.data.frame(vec))
        expect_identical(nrow(vec), 210L)

        # if veg_filling doesn't have correct factor levels there may have been a mistake
        lvls <- c("Strongly Disagree", "Disagree", "Neither", "Agree", "Strongly Agree")
        expected <- data.frame(
            veg_enjoy_ca_healthy = structure(c(3, 1, 1, 1, 5), .Label = lvls, class = "factor"),
            veg_enjoy_ca_tasty = structure(c(5, 1, 1, 1, 1), .Label = lvls, class = "factor"),
            veg_enjoy_ca_filling = structure(c(1, 1, 1, 3, 4), .Label = lvls, class = "factor"),
            veg_enjoy_ca_env = structure(c(3, 2, NA, 3, 4), .Label = lvls, class = "factor")
        )
        expect_identical(vec[1:5, ], expected)
    })

    test_that("as.vector on Multiple Response", {
        expect_true(is.MR(ds$enjoy_mr))
        vec <- as.vector(ds$enjoy_mr)
        expect_true(is.data.frame(vec))
        ## Check that getting subvar by $ from the as.vector of the array
        ## is the same as just getting the subvar as.vector directly
        expect_identical(
            vec$enjoy_mr_savory,
            as.vector(ds$enjoy_mr$enjoy_mr_savory)
        )

        lvls <- c("Yes", "No")
        expected <- data.frame(
            enjoy_mr_savory = structure(c(1, 2, 1, 2, 1), .Label = lvls, class = "factor"),
            enjoy_mr_spicy = structure(c(2, 1, 2, 1, 2), .Label = lvls, class = "factor"),
            enjoy_mr_sweet = structure(c(2, 1, 1, 1, 1), .Label = lvls, class = "factor")
        )
        expect_identical(vec[1:5, ], expected)
    })

    test_that("as.vector on Multiple Response with mode", {
        expected <- data.frame(
            enjoy_mr_savory = c(1, 2, 1, 2, 1),
            enjoy_mr_spicy = c(2, 1, 2, 1, 2),
            enjoy_mr_sweet = c(2, 1, 1, 1, 1)
        )

        vec <- as.vector(ds$enjoy_mr, mode = "id")
        expect_identical(vec[1:5, ], expected)
        expect_identical(
            vec$enjoy_mr_savory,
            as.vector(ds$enjoy_mr$enjoy_mr_savory, mode = "id")
        )
    })

    test_that("as.data.frame on CrunchDataset yields CrunchDataFrame", {
        expect_false(is.data.frame(as.data.frame(ds)))
        expect_is(as.data.frame(ds), "CrunchDataFrame")
        expect_identical(dim(as.data.frame(ds)), c(210L, length(allVariables(ds))))
        expect_identical(names(as.data.frame(ds)), aliases(allVariables(ds)))
        expect_identical(as.data.frame(ds)$age, as.vector(ds$age))
        expect_identical(
            evalq(healthy_eater, as.data.frame(ds)),
            as.vector(ds$healthy_eater)
        )
    })

    test_that("as.data.frame(force = TRUE) generates a POST", {
        expect_POST(
            as.data.frame(ds, force = TRUE, include.hidden = FALSE),
            "https://app.crunch.io/api/datasets/veg/export/csv/",
            '{"filter":null,"options":{"header_field":"qualified_alias",',
            '"missing_values":"","use_category_ids":true}}'
        )
    })

    test_that("csvColInfo works in simple case with no duplicate aliases", {
        col_info <- csvColInfo(
            ds[, c("wave", "age", "enjoy_mr", "last_vegetable", "last_vegetable_date")]
        )

        expect_s4_class(attr(col_info, "meta"), "VariableCatalog")
        attr(col_info, "meta") <- NULL

        expected <- data.frame(
            stringsAsFactors = FALSE,
            orig_alias = c("wave","age",
                      "last_vegetable","last_vegetable_date","enjoy_mr_savory",
                      "enjoy_mr_spicy","enjoy_mr_sweet"),
            parent_alias = c(NA, NA, NA, NA, "enjoy_mr", "enjoy_mr", "enjoy_mr"),
            qualified_alias = c("wave","age",
                                      "last_vegetable","last_vegetable_date",
                                      "enjoy_mr[enjoy_mr_savory]","enjoy_mr[enjoy_mr_spicy]",
                                      "enjoy_mr[enjoy_mr_sweet]"),
            cond_qualified_alias = c("wave","age",
                                "last_vegetable","last_vegetable_date","enjoy_mr_savory",
                                "enjoy_mr_spicy","enjoy_mr_sweet"),
            var_type = c("categorical",
                         "numeric","text","datetime","categorical",
                         "categorical","categorical")
        )

        expect_equivalent(col_info, expected)
    })

    test_that("csvColInfo works when there are duplicate aliases", {
        col_info <- csvColInfo(ds_dup)

        expect_s4_class(attr(col_info, "meta"), "VariableCatalog")
        attr(col_info, "meta") <- NULL

        expected <- data.frame(
            stringsAsFactors = FALSE,
            orig_alias = c("x1","x2","y1",
                      "y2","z","x1","x2_derived","y1","z"),
            parent_alias = c(NA, NA, NA, NA, NA, "x", "x", "y", "y"),
            qualified_alias = c("x1","x2","y1",
                                      "y2","z","x[x1]","x[x2_derived]","y[y1]","y[z]"),
            cond_qualified_alias = c("x1","x2","y1",
                                "y2","z","x[x1]","x2_derived","y[y1]","y[z]"),
            var_type = c("numeric","numeric",
                         "categorical","categorical","categorical",
                         "numeric","numeric","categorical","categorical")
        )
        expect_equivalent(col_info, expected)
    })

    test_that("csvToDataFrame produces the correct data frame", {
        csv_df <- read.csv(
            datasetFixturePath("veg.csv"),
            stringsAsFactors = FALSE, check.names = FALSE#, na.strings = ""
        )
        expected <- readRDS(datasetFixturePath("veg_df.rds"))
        vars <- c(
            "wave", "age", "healthy_eater", "enjoy_mr", "veg_enjoy_ca", "ratings_numa",
            "last_vegetable", "last_vegetable_date"
        )
        cdf <- as.data.frame(ds[, vars])
        # test local CDF variables
        cdf$newvar <- expected$newvar <- c(1:209, NA)
        col_info <- csvColInfo(ds[, vars])
        actual <- csvToDataFrame(csv_df, cdf, col_info)
        expect_equal(actual, expected)
    })

    test_that("csvToDataFrame respects include.hidden", {
        # mock the include.hidden=FALSE by removing variables from csv_df
        col_info <- csvColInfo(ds)
        csv_df <- read.csv(datasetFixturePath("veg-no-hidden.csv"), stringsAsFactors = FALSE, check.names = FALSE)
        expected <- readRDS(datasetFixturePath("veg_hidden_df.rds"))
        cdf <- as.data.frame(ds, include.hidden = FALSE)
        actual <- csvToDataFrame(csv_df, cdf, col_info)
        expect_equal(actual, expected)
    })

    test_that("csvToDataFrame handles duplicate aliases", {
        csv_df <- read.csv(datasetFixturePath("dup.csv"), stringsAsFactors = FALSE, check.names = FALSE)
        col_info <- csvColInfo(ds_dup)

        expected_flat_df <- data.frame(
            v1 = 1:3,
            v2 = 2:4,
            v3 = factor(letters[1:3], levels = letters[1:5]),
            v4 = factor(letters[2:4], levels = letters[1:5]),
            v5 = factor(letters[11:13], levels = letters[11:15]),
            v6 = 1:3,
            v7 = 2:4,
            v8 = factor(letters[1:3], levels = letters[1:5]),
            v9 = factor(letters[2:4], levels = letters[1:5])
        )

        df_cond_qualified <- csvToDataFrame(csv_df, ds_dup, col_info)
        cond_qualified_names <- c("x1", "x2", "y1", "y2", "z", "x[x1]", "x2_derived", "y[y1]", "y[z]")
        expect_equal(names(df_cond_qualified), cond_qualified_names)
        expect_equivalent(
            setNames(df_cond_qualified, paste0("v", seq_len(ncol(df_cond_qualified)))),
            expected_flat_df
        )

        df_qualified <- csvToDataFrame(csv_df, ds_dup, col_info, "qualified_alias")
        qualified_names <- c("x1", "x2", "y1", "y2", "z", "x[x1]", "x[x2_derived]", "y[y1]", "y[z]")
        expect_equal(names(df_qualified), qualified_names)
        expect_equivalent(
            setNames(df_qualified, paste0("v", seq_len(ncol(df_qualified)))),
            expected_flat_df
        )

        expected_packed_df <- setNames(expected_flat_df[, 1:5], c("x1", "x2", "y1", "y2", "z"))
        expected_packed_df$v6 <- setNames(expected_flat_df[, 6:7], c("x1", "x2_derived"))
        expected_packed_df$v7 <- setNames(expected_flat_df[, 8:9], c("y1", "z"))
        df_packed <- csvToDataFrame(csv_df, ds_dup, col_info, "packed")
        expect_equivalent(df_packed, expected_packed_df)
    })

    test_that("csvToDataFrame handles 1 column subvariables", {
        csv_df <- structure(list(`x[x1]` = 1:3, `x[x2_derived]` = 1:3, `y[y1]` = 1:3), class = "data.frame", row.names = c(NA, -3L))
        expected <- data.frame(`x[x1]` = 1:3, `x2_derived` = 1:3, `y[y1]` = factor(letters[1:3], levels = letters[1:5]), check.names = FALSE)
        col_info <- csvColInfo(ds_dup)

        df_actual <- csvToDataFrame(csv_df, ds_dup, col_info[6:8, ])[c("x[x1]", "x2_derived", "y[y1]")]

        expect_equal(df_actual, expected)
    })

    test_that("as.data.frame when a variable has an apostrophe in its alias", {
        t2 <- forceVariableCatalog(ds)

        t2@variables@index[[2]]$alias <- "Quote 'unquote' alias"
        expect_is(as.data.frame(t2), "CrunchDataFrame")
    })

    test_that("write.csv(ds)", {
        expect_POST(
            write.csv(ds, file = ""),
            "https://app.crunch.io/api/datasets/veg/export/csv/",
            '{"filter":null,"options":{"use_category_ids":false}}'
        )
    })

    test_that("as.data.frame() works with hidden variables", {
        new_ds <- cachedLoadDataset("test ds")
        new_ds$gender@tuple[["discarded"]] <- TRUE
        new_ds_df <- as.data.frame(new_ds, include.hidden = TRUE)
        expect_equal(
            names(new_ds_df),
            aliases(allVariables(new_ds))
        )
        expect_equal(ncol(new_ds_df), 7)
    })

    test_that(".crunchPageSize", {
        expect_identical(.crunchPageSize(ds$age), 100000L)
        expect_identical(.crunchPageSize(ds$healthy_eater), 200000L)
        expect_identical(.crunchPageSize(ds$last_vegetable), 5000L)
        expect_identical(.crunchPageSize(ds$enjoy_mr), 200000L %/% 3L)
        expect_identical(.crunchPageSize(ds$veg_enjoy_ca), 200000L %/% 4L)
        expect_identical(.crunchPageSize(ds$last_vegetable_date), 100000L)
        expect_identical(.crunchPageSize(2016 - ds$birthyr), 50000L)
    })


    test.df <- as.data.frame(ds)


    test_that("model.frame thus works on CrunchDataset", {
        expect_identical(
            model.frame(age ~ healthy_eater, data = test.df),
            model.frame(age ~ healthy_eater, data = ds)
        )
    })


    test_that("so lm() should work too", {
        test.lm <- lm(age ~ healthy_eater, data = ds)
        expected <- lm(age ~ healthy_eater, data = test.df)
        expect_is(test.lm, "lm")
        expect_identical(names(test.lm), names(expected))
        for (i in setdiff(names(expected), "call")) {
            expect_identical(test.lm[[i]], expected[[i]])
        }
    })
})

with_test_authentication({
    ds <- newDataset(df)

    # change the alias of v6 to be something that includes spaces/punctuation
    alias(ds$v6) <- "vee six !"

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
        expect_equivalent(as.vector(ds$v4b)[-(3:5)], df$v4[-(3:5)]) # nolint
        expect_true(all(is.na(as.vector(ds$v4b)[3:5])))
    })
    test_that("as.vector with mode specified on Categorical", {
        expect_identical(
            as.vector(ds$v4b, mode = "id"),
            c(1, 2, -1, -1, -1, 2, rep(1:2, 7))
        )
        expect_identical(
            as.vector(ds$v4b, mode = "numeric"),
            c(5, 3, NA, NA, NA, 3, rep(c(5, 3), 7))
        )
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
        expect_identical(names(as.data.frame(ds)), aliases(variables(ds)))

        # check that all of the values are the same
        expect_identical(as.data.frame(ds)$v1, as.vector(ds$v1))
        expect_identical(as.data.frame(ds)$v2, as.vector(ds$v2))
        expect_identical(as.data.frame(ds)$v3, as.vector(ds$v3))
        expect_identical(as.data.frame(ds)$v4, as.vector(ds$v4))
        expect_identical(as.data.frame(ds)$v5, as.vector(ds$v5))
        expect_identical(as.data.frame(ds)$`vee six !`, as.vector(ds$`vee six !`))
    })

    test_that("as.data.frame(force) with API", {
        skip_on_local_backend("Vagrant host doesn't serve files correctly")
        expect_true(is.data.frame(as.data.frame(as.data.frame(ds))))

        df <- as.data.frame(ds, force = TRUE)
        expect_true(is.data.frame(df))

        # check that all of the values are the same
        expect_identical(df$v1, as.vector(ds$v1))
        # we only compare the non-na (1:15) in the text variable, becuase the NA
        # values come down as "No Data". This should probably be fixed to be NAs
        # at some point
        expect_identical(df$v2[1:15], as.vector(ds$v2)[1:15])
        expect_identical(df$v3, as.vector(ds$v3))
        expect_identical(df$v4, as.vector(ds$v4))
        expect_identical(df$v5, as.vector(ds$v5))
        expect_identical(df$`vee six !`, as.vector(ds$`vee six !`))
    })

    ds$hidden_var <- 1:20
    ds <- hideVariables(ds, "hidden_var")

    test_that("as.data.frame(force) retrieves hidden variables", {
        skip_on_local_backend("Vagrant host doesn't serve files correctly")
        expect_equal(hiddenVariables(ds), "hidden_var")

        df <- as.data.frame(ds, force = TRUE, include.hidden = FALSE)
        expect_equal(names(df), c("v1", "v2", "v3", "v4", "v5", "vee six !"))

        df <- as.data.frame(ds, force = TRUE, include.hidden = TRUE)
        expect_equal(names(df), c("v1", "v2", "v3", "v4", "v5", "vee six !", "hidden_var"))

        df <- as.data.frame(ds[, c("v1", "hidden_var")], force = TRUE)
        expect_equal(names(df), c("v1", "hidden_var"))
    })

    test_that("Multiple response variables in as.data.frame(force=TRUE)", {
        skip_on_local_backend("Vagrant host doesn't serve files correctly")
        mrds <- mrdf.setup(newDataset(mrdf, name = "test-mrdfmr"), selections = "1.0")
        mrds$MR2 <- deriveArray(
            list(
                VarDef(mrds$MR$mr_1, name = "dup mr_1", alias = "mr_1"),
                VarDef(mrds$MR$mr_2, name = "dup v4", alias = "v4")
            ),
            name = "MR 2", numeric = FALSE
        )
        testthat::expect_message(
            mrds_df <- as.data.frame(mrds, force = TRUE),
            paste0(
                "Some column names have been qualified with their array parent's alias to avoid ",
                "duplicate aliases in the data.frame.\nSee the `array_strategy` argument in ",
                "help('as.data.frame.CrunchDataset'). Example renames:\n",
                "mr_1 -> MR[mr_1]\n",
                "mr_1 -> MR2[mr_1]\n",
                "v4 -> MR2[v4]"
            ),
            fixed = TRUE
        )

        expect_equal(ncol(mrds_df), 6)
        expect_equal(names(mrds_df), c("MR[mr_1]", "mr_2", "mr_3", "v4", "MR2[mr_1]", "MR2[v4]"))
        expect_equal(mrds_df[["MR[mr_1]"]], as.vector(mrds$MR$mr_1))
        expect_equal(mrds_df$mr_2, as.vector(mrds$MR$mr_2))
        expect_equal(mrds_df$mr_3, as.vector(mrds$MR$mr_3))
        expect_equal(mrds_df$v4, as.vector(mrds$v4))
        expect_equal(mrds_df[["MR2[mr_1]"]], as.vector(mrds$MR2$mr_1))
        expect_equal(mrds_df[["MR2[v4]"]], as.vector(mrds$MR2$v4))
    })

    v2 <- ds$v2
    # Force variable catalog so that it'll get stale
    ds <- forceVariableCatalog(ds)
    with_consent(delete(v2))
    test_that("CrunchDataFrame lazily fetches columns", {
        skip("ZC-542 is changing this behavior, skip for now (used to error, now column is silently skipped)")
        expect_true("v2" %in% names(ds)) ## ds is stale
        expect_is(as.data.frame(ds), "CrunchDataFrame")
        ## This should error because it will try to get values for v2
        expect_error(as.data.frame(ds, force = TRUE))
    })

    ds <- forceVariableCatalog(ds)
    uncached({
        with_mocked_bindings(.crunchPageSize = function(x) 5L, {
            with(temp.option(httpcache.log = ""), {
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
        expect_equivalent(
            model.frame(v1 ~ v3, data = ds),
            model.frame(v1 ~ v3, data = df)
        )
    })

    test_that("so lm() should work too over the API", {
        test.lm <- lm(v1 ~ v3, data = ds)
        expected <- lm(v1 ~ v3, data = df)
        expect_is(test.lm, "lm")
        expect_identical(names(test.lm), names(expected))
        ## would like this to be "identical" instead of "equivalent"
        for (i in setdiff(names(expected), "call")) {
            expect_equivalent(test.lm[[i]], expected[[i]])
        }
    })
})
