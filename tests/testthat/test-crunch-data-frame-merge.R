context("CrunchDataFrame merging")


with_mock_crunch({
    ds <- loadDataset("test ds")

    test_that("the most basic case of merging a CrunchDataFarme with a data.frame", {
        ds_df <- as.data.frame(ds)
        local_df <- data.frame(gender = c("Male", "Female"), new = "new")
        expect_silent(merged_df <- merge(ds_df,
            local_df,
            by.x = "gender",
            by.y = "gender"
        ))
        expect_is(merged_df, "CrunchDataFrame")
        expect_identical(nrow(merged_df), nrow(ds))
        expect_identical(ncol(merged_df), ncol(ds) + 1L)
        # ds$gender has Male, Female and NA rows, whenever gender is NA, the
        # new column should also be NA. When gender is Male or Female the new
        # column should be new.
        expect_identical(
            merged_df$new,
            factor(c(
                "new", "new", NA, "new", "new", "new", "new",
                NA, NA, "new", "new", "new", "new", NA, NA,
                NA, "new", "new", "new", NA, "new", "new",
                "new", NA, "new"
            ))
        )
        expect_identical(is.na(merged_df$new), is.na(merged_df$gender))
    })

    test_that("merge.CrunchDataFrame input validation", {
        # make sure that sort input is validated
        ds_df <- as.data.frame(ds)
        local_df <- data.frame(gender = c("Male", "Female"), new = "new")
        expect_error(
            merged_df <- merge(ds_df,
                local_df,
                sort = "not_an_input"
            ),
            paste0("'arg' should be one of ", dQuote("x"), ", ", dQuote("y"))
        )
        # check that there is a warning if all is specified.
        expect_warning(
            merge(ds_df, local_df, all = TRUE),
            paste0(
                "options ", serialPaste(dQuote(c("all", "all.x", "all.y"))),
                " are not currently supported by merge.CrunchDataFrame. ",
                "The results will include all rows from whichever argument ",
                "\\(x or y\\) is used to sort."
            )
        )
    })

    test_that("merge.CrunchDataFrame works with sort=y", {
        # when sort=y is specified, the resulting order of the CrunchDataFrame
        # should follow the ordering present in y, and include all of the data
        # for each row in the data.frame and the subset of rows in the
        # CrunchDataset that match
        ds_df <- as.data.frame(ds)
        # Each letter appears twice in textVar
        expect_equal(table(ds_df$textVar %in% c("w", "n"))[["TRUE"]], 4)
        df_local <- data.frame(
            textVar = c("w", "n"),
            new = factor(c("new1", "new2")),
            stringsAsFactors = FALSE
        )
        expect_silent(merged_df <- merge(ds_df,
            df_local,
            by = "textVar",
            sort = "y"
        ))
        expect_identical(nrow(merged_df), 4L)
        expect_identical(merged_df$textVar, c("w", "w", "n", "n"))
        # Check another variable to see that the row order is correct (shifted)
        expect_identical(
            merged_df$starttime,
            from8601(c(
                "1956-02-13", "1956-01-28", "1955-12-28",
                "1955-12-30"
            ))
        )
        expect_identical(
            merged_df$new,
            factor(c("new1", "new1", "new2", "new2"))
        )
    })

    test_that("merge.CrunchDataFrame duplicates rows when needed", {
        # if the data.frame that is being merged with a CrunchDataFrame has
        # duplicates in the column that is used in the by argument, then the
        # rows in the CrunchDataset should be 'duplicated'. This doesn't
        # actually alter the number of rows on Crunch, it just adds more
        # than one instance of the row number in the row.order attribute of the
        # CrunchDataFrame. More than on row number in row.order will return
        # that value multiple times (in the approriate locations) when
        # as.vector is called / the column is used.

        # If sort=y and y only has a subset of the elements in the by columns
        # that the CrunchDataset has, the rows from the dataset that are not in
        # y will be removed from the CrunchDataFrame (again, removed here only
        # means that their row indeces will not be in row.order)
        ds_df <- as.data.frame(ds)
        df_local <- data.frame(
            textVar = c("w", "w"),
            new = factor(c("new1", "new2")),
            stringsAsFactors = FALSE
        )
        expect_silent(merged_df <- merge(ds_df,
            df_local,
            by.x = "textVar",
            by.y = "textVar",
            sort = "y"
        ))
        expect_identical(nrow(merged_df), 4L)
        expect_identical(merged_df$textVar, c("w", "w", "w", "w"))
        expect_identical(
            merged_df$starttime,
            from8601(c(
                "1956-02-13", "1956-01-28", "1956-02-13",
                "1956-01-28"
            ))
        )
        expect_identical(
            merged_df$new,
            factor(c("new1", "new1", "new2", "new2"))
        )

        # Make sure the behavior for sort=x is the same when the CrunchDataset
        # or data.frame don't have the same members: the elements from x are
        # always preserved (and used for ordering), but if there is more than
        # one element in the data.frame's by column, those rows are duplicated.
        ds_df <- as.data.frame(ds) # must over-write the CrunchDataFrame
        expect_silent(merged_df <- merge(ds_df,
            df_local,
            by.x = "textVar",
            by.y = "textVar",
            sort = "x"
        ))
        expect_identical(nrow(merged_df), 27L)
        expect_identical(merged_df$textVar, c(
            "w", "w", "n", "x", "b", "q",
            "s", "l", "v", "v", "y", "m",
            "t", "s", "e", "z", "k", "n",
            "w", "w", "v", "i", "h", "z",
            "m", "c", "x"
        ))
        expect_identical(
            merged_df$starttime,
            from8601(c(
                "1956-02-13", "1956-02-13", "1955-12-28",
                "1955-11-17", "1956-02-08", "1956-01-17",
                "1956-01-21", "1956-02-07", "1955-12-25",
                "1956-01-17", "1955-12-12", "1955-11-21",
                "1955-12-06", "1956-01-19", "1955-12-15",
                "1956-02-07", "1956-02-08", "1955-12-30",
                "1956-01-28", "1956-01-28", "1956-01-01",
                "1956-01-15", "1955-11-13", "1955-11-17",
                "1955-11-09", "1955-12-22", "1955-12-20"
            ))
        )
        expect_identical(
            merged_df$new,
            factor(c(
                "new1", "new2", rep(NA, 16), "new1", "new2",
                rep(NA, 7)
            ))
        )
    })

    test_that("merge.CrunchDataFrame recreates even instantiated columns", {
        ds_df <- as.data.frame(ds)
        brtyr <- ds_df$birthyr
        loc <- ds_df$location
        expect_silent(merged_df <- merge(ds_df,
            data.frame(gender = c("Male", "Female"), new = "new"),
            by.x = "gender",
            by.y = "gender"
        ))
        expect_identical(ncol(merged_df), ncol(ds) + 1L)
        expect_identical(names(merged_df), c(names(ds), "new"))
    })

    test_that("merge.CrunchDataFrame can handle a locally modified crunchdataframe", {
        ds_df <- as.data.frame(ds)
        ds_df$local_var <- c(1:25)
        expect_silent(merged_df <- merge(ds_df,
            data.frame(gender = c("Male", "Female"), new = "new"),
            by.x = "gender",
            by.y = "gender"
        ))
        expect_identical(ncol(merged_df), ncol(ds) + 2L)
        expect_identical(names(merged_df), c(names(ds), "local_var", "new"))
        expect_identical(merged_df$local_var, ds_df$local_var)
    })

    test_that("fix_bys returns the reference to be used for by", {
        df <- data.frame(foo = c(1, 2), bar = c(3, 4))
        expect_equal(fix_bys(df, "bar"), "bar")
    })

    test_that("fix_bys input validation", {
        expect_error(
            fix_bys("foo", "bar"),
            "foo must be a data.frame or CrunchDataFrame"
        )
        df <- data.frame(foo = c(1, 2), bar = c(3, 4))
        expect_error(fix_bys(df, c("foo", "bar")), "by must reference one and only one variable")
        expect_error(fix_bys(df, "baz"), "baz does not reference a variable in df")
    })
})
