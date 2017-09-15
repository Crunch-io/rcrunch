context("CrunchDataFrame Methods")


with_mock_crunch({
    ds <- loadDataset("test ds")
    
    test_that("can manipulate the row order of a crunchDataFrame", {
        ds_df <- as.data.frame(ds)
        gndr <- as.vector(ds$gender)
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
    
    test_that("get_var_from_server works with variables, including different modes for factors", {
        ds_df <- as.data.frame(ds)
        true_df <- as.data.frame(ds, force = TRUE)
        
        expect_equal(get_var_from_server("textVar", ds_df),
                     true_df$textVar)
        expect_equal(get_var_from_server("gender", ds_df, mode = 'factor'),
                     true_df$gender)
        expect_equal(get_var_from_server("gender", ds_df, mode = 'id'),
                     ifelse(is.na(true_df$gender), -1,
                            ifelse(true_df$gender == "Female", 2, 1)))
        expect_equal(get_var_from_server("gender", ds_df, mode = 'numeric'),
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
    
    test_that("CrunchDataFrames respect order", {
        ds_df <- as.data.frame(ds[c("gender", "birthyr", "location")])
        expect_equal(names(ds_df), c("gender", "birthyr", "location"))
        expect_equal(names(ds_df[,c("location", "gender", "birthyr")]),
                     c("location", "gender", "birthyr"))
        
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
    
    test_that("get_CDF_var input validation", {
        ds_df <- as.data.frame(ds)
        
        expect_error(set_CDF_var(col_name = "new_local_var",
                                 cdf = data.frame(textVar = c(1,2))),
                     paste("The cdf argument must be a CrunchDataFrame, got",
                           "data.frame instead."))
    })
})