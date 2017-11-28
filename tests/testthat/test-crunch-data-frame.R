context("CrunchDataFrame Methods")


with_mock_crunch({
    ds <- loadDataset("test ds")

    test_that("can manipulate the row order of a crunchDataFrame", {
        ds_df <- as.data.frame(ds)
        gndr <- as.vector(ds$gender)
        expect_equal(nrow(ds_df), 25)
        # both reording and subsetting the dataset
        new_order <- c(4, 3, 1, 2)
        attr(ds_df, "order") <- new_order
        expect_equal(ds_df$gender, gndr[new_order])
        expect_equal(nrow(ds_df), 4)
        ds_df2 <- as.data.frame(ds, row.order = new_order)
        expect_equal(ds_df2$gender, gndr[new_order])
        expect_equal(nrow(ds_df2), 4)
    })

    test_that("Extract methods work with CrunchDataFrames like they do with data.frames", {
        ds_new <- loadDataset("test ds")
        # remove array variables because they are not equivalent in crdf and
        # data.frames, since in the data.frames they have been flattened.
        ds_new <- ds_new[!{names(ds_new) %in% c("mymrset", "catarray")}]

        ds_df <- as.data.frame(ds_new)
        true_df <- readRDS("dataset-fixtures/test_ds.rds")

        # single column/row extraction
        expect_equivalent(ds_df[1,], true_df[1,])
        expect_equivalent(ds_df[,1], true_df[,1])

        # multiple columns/rows
        expect_equivalent(ds_df[c(1, 2),], true_df[c(1, 2),])
        expect_equivalent(ds_df[,c(1, 2)], true_df[,c(1, 2)])

        # single column extraction with characters
        expect_equivalent(ds_df[,"location"], true_df[,"location"])

        # multiple columns with characters
        expect_equivalent(ds_df[,c("starttime", "textVar")],
                          true_df[,c("starttime", "textVar")])

        # columns/rows with logicals
        expect_equivalent(ds_df[,c(TRUE, FALSE, TRUE, FALSE, TRUE)],
                          true_df[,c(TRUE, FALSE, TRUE, FALSE, TRUE)])
        expect_equivalent(ds_df[c(TRUE, rep(FALSE, 23), TRUE),],
                          true_df[c(TRUE, rep(FALSE, 23), TRUE),])

        # logicals recycle even at a variety of lengths
        expect_equivalent(ds_df[,c(TRUE)],
                          true_df[,c(TRUE)])
        expect_equivalent(ds_df[c(TRUE),],
                          true_df[c(TRUE),])
        expect_equivalent(ds_df[,c(TRUE, FALSE)],
                          true_df[,c(TRUE, FALSE)])
        expect_equivalent(ds_df[c(TRUE, FALSE),],
                          true_df[c(TRUE, FALSE),])
        expect_equivalent(ds_df[,c(TRUE, FALSE, TRUE)],
                          true_df[,c(TRUE, FALSE, TRUE)])
        expect_equivalent(ds_df[c(TRUE, FALSE, TRUE),],
                          true_df[c(TRUE, FALSE, TRUE),])
        expect_equivalent(ds_df[,c(TRUE, FALSE, TRUE, FALSE)],
                          true_df[,c(TRUE, FALSE, TRUE, FALSE)])
        expect_equivalent(ds_df[c(TRUE, FALSE, TRUE, FALSE),],
                          true_df[c(TRUE, FALSE, TRUE, FALSE),])


        # both rows and columns
        expect_equivalent(ds_df[c(1, 3, 5), "location"],
                          true_df[c(1, 3, 5), "location"])


        # $ methods
        expect_equivalent(ds_df$starttime, true_df$starttime)

        # [[ methods
        expect_equivalent(ds_df[[1]], true_df[[1]])
        expect_equivalent(ds_df[["textVar"]], true_df[["textVar"]])
        expect_equivalent(ds_df[[2]], true_df[[2]])
        expect_equivalent(ds_df[["birthyr"]], true_df[["birthyr"]])
    })

    test_that("Extract methods input checking", {
        ds_df <- as.data.frame(ds)

        expect_error(ds_df["foo",],
                     "row subsetting must be done with either numeric or a logical")
        expect_error(ds_df[,list(list(1))],
                     "column subsetting must be done with a numeric, character, or logical")

        expect_error(`$`(ds_df,1),
                     paste0("invalid subscript type 'double'"))

    })

    test_that("getVarFromServer works with variables, including different modes for factors", {
        ds_df <- as.data.frame(ds)
        true_df <- readRDS("dataset-fixtures/test_ds.rds")

        expect_equal(getVarFromServer("textVar", ds_df),
                     true_df$textVar)
        expect_equal(getVarFromServer("gender", ds_df, mode = 'factor'),
                     true_df$gender)
        expect_equal(getVarFromServer("gender", ds_df, mode = 'id'),
                     ifelse(is.na(true_df$gender), -1,
                            ifelse(true_df$gender == "Female", 2, 1)))
        expect_equal(getVarFromServer("gender", ds_df, mode = 'numeric'),
                     ifelse(true_df$gender == "Female", 2, 1))
    })

    test_that("crdf column validators work", {
        ds_df <- as.data.frame(ds)
        expect_error(ds_df$new_local_var <- c(5:1),
                     "replacement has 5 rows, the CrunchDataFrame has 25")

        expect_error(ds_df$new_local_var <- c(1, 2),
                     "replacement has 2 rows, the CrunchDataFrame has 25")

        expect_error(ds_df$new_local_var <- c(1:100),
                     "replacement has 100 rows, the CrunchDataFrame has 25")
    })

    test_that("getCrdfVar input validation", {
        ds_df <- as.data.frame(ds)

        expect_error(getCrdfVar("not_a_var", ds_df),
                     paste("The variable", dQuote("not_a_var") ,
                           "is not found in the CrunchDataFrame."))
    })

    test_that("CrunchDataFrames respect order", {
        ds_df <- as.data.frame(ds[c("gender", "birthyr", "location")])
        expect_equal(names(ds_df), c("gender", "birthyr", "location"))
        expect_equal(names(ds_df[,c("location", "gender", "birthyr")]),
                     c("location", "gender", "birthyr"))

    })

    test_that("crdf column setters work", {
        ds_df <- as.data.frame(ds)

        expect_silent(ds_df$new_local_var <- c(25:1))
        expect_true("new_local_var" %in% names(ds_df))
        expect_equal(ds_df$new_local_var, c(25:1))

        expect_silent(ds_df[["new_local_var2"]] <- c(1:25))
        expect_true("new_local_var2" %in% names(ds_df))
        expect_equal(ds_df$new_local_var2, c(1:25))

        # try overwritting with [<-
        expect_silent(
            ds_df[c(1, 25), c("new_local_var", "new_local_var2")] <-
                c(250, 10, 10, 250))
        expect_equal(ds_df$new_local_var, c(250, 24:2, 10))
        expect_equal(ds_df$new_local_var2, c(10, 2:24, 250))

        # [<- overwritting recycles
        expect_silent(
            ds_df[c(1, 25), c("new_local_var", "new_local_var2")] <-
                c(250, 10))
        expect_equal(ds_df$new_local_var, c(250, 24:2, 10))
        expect_equal(ds_df$new_local_var2, c(250, 2:24, 10))

        # [<- recycling is not magic, and will fail if the numbers don't match up
        expect_error(
            ds_df[c(1, 25), c("new_local_var", "new_local_var2")] <- c(1, 2, 3),
            "replacement has 3 items, need 4")


        # can add a new column with row indices
        expect_silent(
            ds_df[c(1, 25), "new_local_var3"] <-
                c(1, 25))
        expect_equal(ds_df$new_local_var3, c(1, rep(NA, 23), 25))

        # can add a single value
        expect_silent(ds_df$new_local_var4 <- 1)
        expect_equal(ds_df$new_local_var4, rep(1, 25))

        expect_silent(ds_df$textVar <- c(1:25))
        expect_equal(ds_df$textVar, c(1:25))
    })

    test_that("setting a column to NULL works", {
        ds_df <- as.data.frame(ds)
        ds_df$new_local_var <- c(25:1)
        expect_true("new_local_var" %in% names(ds_df))
        expect_silent(ds_df$new_local_var <- NULL)
        expect_false("new_local_var" %in% names(ds_df))

        expect_silent(ds_df$gender <- NULL)
        expect_false("gender" %in% names(ds_df))
    })
})

with_test_authentication({
    ds <- mrdf.setup(newDataset(mrdf, name = "test-mrdfmr"), selections = "1.0")
    ds_df <- as.data.frame(ds, force = TRUE)
    test_that("Multiple response variables", {
        expect_equal(ncol(ds_df), 4)
        expect_equal(names(ds_df), c("mr_1", "mr_2", "mr_3", "v4"))
        expect_equal(ds_df$mr_1, as.vector(ds$MR$mr_1))
        expect_equal(ds_df$mr_2, as.vector(ds$MR$mr_2))
        expect_equal(ds_df$mr_3, as.vector(ds$MR$mr_3))
        expect_equal(ds_df$v4, as.vector(ds$v4))
    })

    test_that("CDF with assigned variables returns dataframe", {
        ds_df$new_local_var <- 4:1
        ds_df$new_local_var2 <- 1:4
        # and we can get a dataframe from this new one.
        true_df <- as.data.frame(ds_df)
        expect_equal(as.vector(ds_df$new_local_var), 4:1)
        expect_equal(as.vector(ds_df$new_local_var2), 1:4)
    })
})
