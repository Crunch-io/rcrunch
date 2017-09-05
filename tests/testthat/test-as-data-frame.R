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

    test_that("as.data.frame when a variable has an apostrophe in its alias", {
        t2 <- ds
        t2@variables@index[[2]]$alias <- "Quote 'unquote' alias"
        expect_is(as.data.frame(t2), "CrunchDataFrame")
    })

    test_that("as.data.frame(as.data.frame())", {
        expect_true(is.data.frame(as.data.frame(as.data.frame(ds))))
        expect_true(is.data.frame(as.data.frame(ds, force=TRUE)))
    })

    test_that("as.data.frame() works with hidden variables", {
        new_ds <- loadDataset("test ds")
        new_ds$gender@tuple[["discarded"]] <- TRUE
        expect_equivalent(hiddenVariables(new_ds), "gender")
        new_ds_df <- as.data.frame(new_ds)
        expect_equal(names(new_ds_df),
                     aliases(variables(new_ds)))
        expect_equal(ncol(new_ds_df), 6)
        expect_silent(
            expect_equal(names(as.data.frame(new_ds_df)),
                         aliases(variables(new_ds))))
        
        # now we want the hidden vars to be includes
        new_ds_df <- as.data.frame(new_ds, include.hidden = TRUE)
        expect_equal(names(new_ds_df),
                     aliases(allVariables(new_ds)))
        expect_equal(ncol(new_ds_df), 7)
        expect_warning(
            expect_equal(names(as.data.frame(new_ds_df)),
                         aliases(allVariables(new_ds))),
            "Variable gender is hidden")
    })
    
    test_that("as.data.frame size limit", {
        with(temp.option(crunch.data.frame.limit=50), {
            expect_error(as.data.frame(ds, force=TRUE),
                "Dataset too large to coerce")
            expect_true(is.data.frame(as.data.frame(ds[,1:2], force=TRUE)))
        })
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

    test_that(".crunchPageSize", {
        expect_identical(.crunchPageSize(ds$birthyr), 100000L)
        expect_identical(.crunchPageSize(ds$gender), 200000L)
        expect_identical(.crunchPageSize(ds$textVar), 5000L)
        expect_identical(.crunchPageSize(ds$mymrset), 66666L)
        expect_identical(.crunchPageSize(ds$catarray), 66666L)
        expect_identical(.crunchPageSize(ds$starttime), 100000L)
        expect_identical(.crunchPageSize(2016 - ds$birthyr), 50000L)
    })

    test_that("can manipulate the row order of a crunchDataFrame", {
        ds_df <- as.data.frame(ds)
        gndr <- ds_df$gender
        expect_equal(nrow(ds_df), 25)
        # both reording and subsetting the dataset
        new_order <- c(4,3,1,2)
        attr(ds_df, "order") <- new_order
        expect_equal(ds_df$gender, gndr[new_order])
        expect_equal(nrow(ds_df), 4)
        ds_df2 <- as.data.frame(ds, row.order = new_order)
        expect_equal(ds_df2$gender, gndr[new_order])
        expect_equal(nrow(ds_df2), 4)
    })

    test_that("the most basic case of merging a CrunchDataFarme with a data.frame", {
        ds_df <- as.data.frame(ds)
        local_df <- data.frame(gender=c("Male", "Female"), new="new")
        expect_silent(merged_df <- merge(ds_df,
                                         local_df,
                                         by.x = "gender",
                                         by.y = "gender"))
        expect_is(merged_df, "CrunchDataFrame")
        expect_identical(nrow(merged_df), nrow(ds))
        expect_identical(ncol(merged_df), ncol(ds) + 1L)
        # ds$gender has Male, Female and NA rows, whenever gender is NA, the
        # new column should also be NA. When gender is Male or Female the new
        # column should be new.
        expect_identical(merged_df$new,
                         factor(c("new", "new", NA, "new", "new", "new", "new",
                                  NA, NA, "new", "new", "new", "new", NA, NA,
                                  NA, "new", "new", "new", NA, "new", "new",
                                  "new", NA, "new")))
        expect_identical(is.na(merged_df$new), is.na(merged_df$gender))
    })

    test_that("merge.CrunchDataFrame input validation", {
        # make sure that sort input is validated
        ds_df <- as.data.frame(ds)
        local_df <- data.frame(gender=c("Male", "Female"), new="new")
        expect_error(merged_df <- merge(ds_df,
                                        local_df,
                                        sort = "not_an_input"),
                     paste0("'arg' should be one of ", dQuote("x"), ", ", dQuote("y"))
        )
        # check that there is a warning if all is specified.
        expect_warning(merge(ds_df, local_df, all = TRUE),
                       paste0("options ", serialPaste(dQuote(c("all", "all.x", "all.y"))),
                       " are not currently supported by merge.CrunchDataFrame. ",
                       "The results will include all rows from whichever argument ",
                       "\\(x or y\\) is used to sort."))
    })

    test_that("Extract methods work with CrunchDataFrames like they do with data.frames", {
        ds_df <- as.data.frame(ds)
        true_df <- as.data.frame(ds, force = TRUE)
        # single column/row extraction
        expect_equivalent(ds_df[1,],true_df[1,])
        expect_equivalent(ds_df[,1],true_df[,1])
        
        # multiple columns/rows
        expect_equivalent(ds_df[c(1,2),],true_df[c(1,2),])
        expect_equivalent(ds_df[,c(1,2)],true_df[,c(1,2)])

        # single column extraction with characters
        expect_equivalent(ds_df[,"location"],true_df[,"location"])
        
        # multiple columns with characters
        expect_equivalent(ds_df[,c("mymrset","textVar")],true_df[,c("mymrset","textVar")])

        # columns/rows with logicals
        expect_equivalent(ds_df[,c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE)],
                          true_df[,c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE)])
        expect_equivalent(ds_df[c(TRUE, rep(FALSE, 23), TRUE),],
                          true_df[c(TRUE, rep(FALSE, 23), TRUE),])
        
        # both rows and columns
        expect_equivalent(ds_df[c(1,3,5),"location"],true_df[c(1,3,5),"location"])
        
                        
        # $ methods
        expect_equivalent(ds_df$starttime,true_df$starttime)
        
        # [[ methods
        expect_equivalent(ds_df[[1]],true_df[[1]])
        expect_equivalent(ds_df[["catarray"]],true_df[["catarray"]])
        expect_equivalent(ds_df[[2]],true_df[[2]])
        expect_equivalent(ds_df[["birthyr"]],true_df[["birthyr"]])
    })
    
    test_that("Extract methods input checking", {
        ds_df <- as.data.frame(ds)
        
        expect_error(ds_df["foo",],
                     "row subsetting must be done with either numeric or a logical")
        expect_error(ds_df[,list(list(1))], 
                     "column subsetting must be done with either numeric or a character")
        
        expect_error(ds_df[c(TRUE),], 
                     paste0("when using a logical to subset rows, the logical vector ",
                     "must have the same length as the number of rows"))
        expect_error(ds_df[,c(TRUE)],
                     paste0("when using a logical to subset columns, the logical ",
                     "vector must have the same length as the number of columns"))
        
        expect_error(`$`(ds_df,1),
                     paste0("invalid subscript type 'double'"))
        
    })
    
    test_that("get_CDF_var works with variables, including different modes for factors", {
        ds_df <- as.data.frame(ds)
        true_df <- as.data.frame(ds, force = TRUE)

        expect_equal(get_CDF_var("textVar", ds_df),
                     true_df$textVar)
        expect_equal(get_CDF_var("gender", ds_df, mode = 'factor'),
                     true_df$gender)
        expect_equal(get_CDF_var("gender", ds_df, mode = 'id'),
                     ifelse(is.na(true_df$gender), -1,
                            ifelse(true_df$gender == "Female", 2, 1)))
        expect_equal(get_CDF_var("gender", ds_df, mode = 'numeric'),
                     ifelse(true_df$gender == "Female", 2, 1))
    })
    
    test_that("cdf column validators work", {
        ds_df <- as.data.frame(ds)
        expect_error(ds_df$new_local_var <- c(5:1),
                     "replacement has 5 rows, the CrunchDataFrame has 25")
        
        expect_error(ds_df$new_local_var <- c(1,2),
                     "replacement has 2 rows, the CrunchDataFrame has 25")
        
        expect_error(ds_df$new_local_var <- c(1:100),
                     "replacement has 100 rows, the CrunchDataFrame has 25")
    })
    
    test_that("get_CDF_var input validation", {
        ds_df <- as.data.frame(ds)
        
        expect_error(get_CDF_var("textVar", data.frame(textVar = c(1,2))),
                     paste("The cdf argument must be a CrunchDataFrame, got",
                           "data.frame instead."))
        expect_error(get_CDF_var("not_a_var", ds_df),
                     paste("The variable", dQuote("not_a_var") ,
                           "is not found in the CrunchDataFrame."))
    })
    
    test_that("cdf column setters work", {
        ds_df <- as.data.frame(ds)
        
        expect_silent(ds_df$new_local_var <- c(25:1))
        expect_true("new_local_var" %in% names(ds_df))
        expect_equal(ds_df$new_local_var, c(25:1))

        expect_silent(ds_df[["new_local_var2"]] <- c(1:25))
        expect_true("new_local_var2" %in% names(ds_df))
        expect_equal(ds_df$new_local_var2, c(1:25))
        
        # and we can get a dataframe from this new one.
        true_df <- as.data.frame(ds_df)

        expect_equal(true_df$new_local_var, c(25:1))
        expect_equal(true_df$new_local_var2, c(1:25))
        # nothing funny has happened to the ordering
        expect_equal(true_df$gender, as.vector(ds$gender))
        
        
        # try overwritting with [<-
        expect_silent(
            ds_df[c(1, 25),c("new_local_var", "new_local_var2")] <- 
                c(250, 10, 10, 250))
        expect_equal(ds_df$new_local_var, c(250, 24:2, 10))
        expect_equal(ds_df$new_local_var2, c(10, 2:24, 250))
        
        # can add a new column with row indices
        expect_silent(
            ds_df[c(1, 25),"new_local_var3"] <- 
                c(1, 25))
        expect_equal(ds_df$new_local_var3, c(1, rep(NA, 23), 25))
        
        # can add a single value
        expect_silent(ds_df$new_local_var4 <- 1)
        expect_equal(ds_df$new_local_var4, rep(1, 25))
    })
    
    test_that("get_CDF_var input validation", {
        ds_df <- as.data.frame(ds)
        
        expect_error(set_CDF_var(col_name = "new_local_var",
                                 cdf = data.frame(textVar = c(1,2))),
                     paste("The cdf argument must be a CrunchDataFrame, got",
                           "data.frame instead."))
        expect_error(set_CDF_var(col_name = "textVar", cdf = ds_df, value = c(1:25)),
                     paste("Cannot over-write data from a Crunch variable."))
    })
    
    test_that("merge.CrunchDataFrame works with sort=y", {
        # when sort=y is specified, the resulting order of the CrunchDataFrame
        # should follow the ordering present in y, and include all of the data
        # for each row in the data.frame and the subset of rows in the
        # CrunchDataset that match
        ds_df <- as.data.frame(ds)
        # Each letter appears twice in textVar
        expect_equal(table(ds_df$textVar %in% c("w", "n"))[["TRUE"]], 4)
        df_local <- data.frame(textVar=c("w", "n"),
                               new=factor(c("new1", "new2")),
                               stringsAsFactors = FALSE)
        expect_silent(merged_df <- merge(ds_df,
                                         df_local,
                                         by = "textVar",
                                         sort = "y"))
        expect_identical(nrow(merged_df), 4L)
        expect_identical(merged_df$textVar, c("w", "w", "n", "n"))
        # Check another variable to see that the row order is correct (shifted)
        expect_identical(merged_df$starttime,
                         from8601(c("1956-02-13", "1956-01-28", "1955-12-28",
                                  "1955-12-30")))
        expect_identical(merged_df$new,
                         factor(c("new1", "new1", "new2", "new2")))
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
        df_local <- data.frame(textVar=c("w", "w"),
                               new=factor(c("new1", "new2")),
                               stringsAsFactors = FALSE)
        expect_silent(merged_df <- merge(ds_df,
                                         df_local,
                                         by.x = "textVar",
                                         by.y = "textVar",
                                         sort = "y"))
        expect_identical(nrow(merged_df), 4L)
        expect_identical(merged_df$textVar, c("w", "w", "w", "w"))
        expect_identical(merged_df$starttime,
                         from8601(c("1956-02-13", "1956-01-28", "1956-02-13",
                                    "1956-01-28")))
        expect_identical(merged_df$new,
                         factor(c("new1", "new1", "new2", "new2")))

        # Make sure the behavior for sort=x is the same when the CrunchDataset
        # or data.frame don't have the same members: the elements from x are
        # always preserved (and used for ordering), but if there is more than
        # one element in the data.frame's by column, those rows are duplicated.
        ds_df <- as.data.frame(ds) # must over-write the CrunchDataFrame
        expect_silent(merged_df <- merge(ds_df,
                                         df_local,
                                         by.x = "textVar",
                                         by.y = "textVar",
                                         sort = "x"))
        expect_identical(nrow(merged_df), 27L)
        expect_identical(merged_df$textVar, c("w", "w", "n", "x", "b", "q",
                                              "s", "l", "v", "v", "y", "m",
                                              "t", "s", "e", "z", "k", "n",
                                              "w", "w", "v", "i", "h", "z",
                                              "m", "c", "x"))
        expect_identical(merged_df$starttime,
                         from8601(c("1956-02-13", "1956-02-13", "1955-12-28",
                                    "1955-11-17", "1956-02-08", "1956-01-17",
                                    "1956-01-21", "1956-02-07", "1955-12-25",
                                    "1956-01-17", "1955-12-12", "1955-11-21",
                                    "1955-12-06", "1956-01-19", "1955-12-15",
                                    "1956-02-07", "1956-02-08", "1955-12-30",
                                    "1956-01-28", "1956-01-28", "1956-01-01",
                                    "1956-01-15", "1955-11-13", "1955-11-17",
                                    "1955-11-09", "1955-12-22", "1955-12-20")))
        expect_identical(merged_df$new,
                         factor(c("new1", "new2", rep(NA, 16), "new1", "new2",
                                  rep(NA, 7))))
    })

    test_that("merge.CrunchDataFrame modifies in place", {
        # Currently merge.CrunchDataFrame modifies the CrunchDataFrame in
        # place, this is a limitation of promises and copying environments.
        ds_df <- as.data.frame(ds)
        expect_silent(merged_df <- merge(ds_df,
                                         data.frame(gender=c("Male", "Female"), new="new"),
                                         by.x = "gender",
                                         by.y = "gender"))
        expect_identical(ncol(merged_df), ncol(ds_df))
        expect_identical(names(merged_df), names(ds_df))
        skip("merge.CrunchDataFrame currently alters the CDF in place")
        # if/when that is resolved, these should replace above.
        expect_identical(ncol(merged_df), ncol(ds_df)+1L)
        expect_identical(names(merged_df), c(names(ds_df), "new"))
    })

    test_that("fix_bys returns the reference to be used for by", {
        df <- data.frame(foo=c(1,2), bar=c(3,4))
        expect_equal(fix_bys(df, "bar"), "bar")
    })

    test_that("fix_bys input validation", {
        expect_error(fix_bys("foo", "bar"),
                     "foo must be a data.frame or CrunchDataFrame")
        df <- data.frame(foo=c(1,2), bar=c(3,4))
        expect_error(fix_bys(df, c("foo", "bar")), "by must reference one and only one variable")
        expect_error(fix_bys(df, "baz"), "baz does not reference a variable in df")
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
        expect_true(is.data.frame(as.data.frame(as.data.frame(ds))))
        expect_true(is.data.frame(as.data.frame(ds, force=TRUE)))
    })

    v2 <- ds$v2
    with_consent(delete(v2))
    test_that("CrunchDataFrame lazily fetches columns", {
        expect_true("v2" %in% names(ds)) ## ds is stale
        expect_is(as.data.frame(ds), "CrunchDataFrame")
        ## This should error because it will try to get values for v2
        expect_error(as.data.frame(ds, force=TRUE))
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
})
