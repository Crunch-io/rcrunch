context("Make a derived variable with a new type")

with_mock_crunch({
    ds <- loadDataset("test ds")

    test_that("haltIfArray", {
        expect_true(haltIfArray(ds$birthyr))
        expect_error(haltIfArray(ds$mymrset),
                     "Array-like variables can't be used.")
        
        expect_error(haltIfArray(ds$mymrset, "embed_func()"),
                     "Array-like variables can't be used with function `embed_func()`.",
                     fixed = TRUE)
    })

    test_that("as.* catches arrays", {
        # arrays are incompatible on the server, but the error message isn't helpful
        expect_error(as.Text(ds$mymrset),
                     "Array-like variables can't be used with function `as.Text()`.",
                     fixed = TRUE)
        expect_error(as.Numeric(ds$catarray),
                     "Array-like variables can't be used with function `as.Numeric()`.",
                     fixed = TRUE)        
    })    
    
    test_that("as.Datetime catches unknown variable types", {
        # fake the variable type because all types so far are covered, but new 
        # types won't be, so make sure we catch those.
        fake_var <- ds$birthyr
        class(fake_var) <- "CrunchVariable"
        fake_var@tuple@body$type <- "not a real type"
        expect_false(is.Numeric(fake_var))
        expect_error(as.Datetime(fake_var),
                     paste0("Can't derive Datetime variables from .*not a real",
                            " type.* variables."))
    })
    
    test_that("numeric to text", {
        var_def <- as.Text(ds$birthyr)
        expect_is(var_def, "CrunchExpr")
        expect_equal(var_def, zfuncExpr("cast", ds$birthyr, "text"))
        
        expect_equal(var_def, as.character(ds$birthyr))
    })
    
    test_that("text to numeric", {
        var_def <- as.Numeric(ds$textVar)
        expect_is(var_def, "CrunchExpr")
        expect_equal(var_def, zfuncExpr("cast", ds$textVar, "numeric"))
        
        expect_equal(var_def, as.numeric(ds$textVar))
    })
    
    test_that("text to categorical", {
        var_def <- as.Categorical(ds$textVar)
        expect_is(var_def, "CrunchExpr")
        expect_equal(var_def, zfuncExpr("cast", ds$textVar, "categorical"))
    })
    
    test_that("datetime to categorical", {
        var_def <- as.Categorical(ds$starttime)
        expect_is(var_def, "CrunchExpr")
        expect_equal(var_def,
                     zfuncExpr("cast",
                               zfunc("format_datetime", ds$starttime,
                                     list(value = "%Y-%m-%d %H:%M:%S")),
                               "categorical"))
    })
    
    test_that("datetime to numeric", {
        var_def <- as.Numeric(ds$starttime)
        expect_is(var_def, "CrunchExpr")
        expect_equal(var_def, zfuncExpr("datetime_to_numeric", ds$starttime))
    })
    
    test_that("datetime to text", {
        var_def <- as.Text(ds$starttime)
        expect_is(var_def, "CrunchExpr")
        expect_equal(var_def, zfuncExpr("format_datetime", ds$starttime, 
                                             list(value = "%Y-%m-%d %H:%M:%S")))
        
        var_def <- as.Text(ds$starttime, format = "%Y-%m-%d")
        expect_is(var_def, "CrunchExpr")
        expect_equal(var_def, zfuncExpr("format_datetime", ds$starttime, 
                                             list(value = "%Y-%m-%d")))
    })
    
    
    test_that("numeric to datetime", {
        var_def <- as.Datetime(ds$birthyr, resolution = "Y")
        expect_is(var_def, "CrunchExpr")
        expect_equal(var_def, zfuncExpr("numeric_to_datetime", ds$birthyr, 
                                        list(value = "Y")))
    })
    
    test_that("text to datetime", {
        var_def <- as.Datetime(ds$textVar)
        expect_is(var_def, "CrunchExpr")
        expect_equal(var_def, zfuncExpr("parse_datetime", ds$textVar, 
                                        list(value = "%Y-%m-%d %H:%M:%S")))
        
        var_def <- as.Datetime(ds$textVar, format = "%Y")
        expect_is(var_def, "CrunchExpr")
        expect_equal(var_def, zfuncExpr("parse_datetime", ds$textVar, 
                                        list(value = "%Y")))
    })
})

with_test_authentication({
    ds <- newDataset(df)
    # ds$v1 # numeric 
    # ds$v2 # text 
    # ds$v3 # numeric 
    # ds$v4 # categorical 
    # ds$v5 # datetime 
    # make a text variable with numbers
    ds$num_as_text <- as.character(c(1:20))
    
    test_that("numeric to text", {
        ds$v1_text <- as.Text(ds$v1)
        expect_true(is.derived(ds$v1_text))
        expect_true(is.Text(ds$v1_text))
        expect_equal(as.vector(ds$v1_text), 
                     as.character(as.vector(ds$v1)))
        
        # alias to R's function names
        expect_equal(as.character(ds$v1), as.Text(ds$v1))
    })
    
    test_that("text to numeric", {
        ds$num_as_text_num <- as.Numeric(ds$num_as_text)
        expect_true(is.derived(ds$num_as_text_num))
        expect_true(is.Numeric(ds$num_as_text_num))
        expect_equal(as.vector(ds$num_as_text_num), 
                     as.numeric(as.vector(ds$num_as_text)))
        
        # if there aren't numbers in the text, the result should be NA
        ds$v2_num <- as.Numeric(ds$v2)
        expect_true(is.derived(ds$v2_num))
        expect_true(is.Numeric(ds$v2_num))
        expect_equal(as.vector(ds$v2_num), 
                     expect_warning(as.numeric(as.vector(ds$v2)),
                     "NAs introduced by coercion"))
        
        # alias to R's function names
        expect_equal(as.numeric(ds$v1), as.Numeric(ds$v1))
    })   

    test_that("text to categorical", {
        ds$v2_cat <- as.Categorical(ds$v2)
        expect_true(is.derived(ds$v2_cat))
        expect_true(is.Categorical(ds$v2_cat))
        expect_equal(as.vector(ds$v2_cat), 
                     as.factor(as.vector(ds$v2)))
    })  
    
    test_that("datetime to numeric", {
        ds$v5_num <- as.Numeric(ds$v5)
        expect_true(is.derived(ds$v5_num))
        expect_true(is.Numeric(ds$v5_num))
        expect_equal(as.vector(ds$v5_num), 
                     as.numeric(as.vector(ds$v5)))
    }) 
    
    test_that("datetime to text", {
        ds$v5_text <- as.Text(ds$v5)
        expect_true(is.derived(ds$v5_text))
        expect_true(is.Text(ds$v5_text))
        expect_equal(as.vector(ds$v5_text), 
                     format(as.vector(ds$v5), format = "%Y-%m-%d %H:%M:%S"))
    }) 
    
    test_that("datetime to text", {
        ds$v5_cat <- as.Categorical(ds$v5)
        expect_true(is.derived(ds$v5_cat))
        expect_true(is.Categorical(ds$v5_cat))
        expect_equal(as.vector(ds$v5_cat), 
                     as.factor(
                         format(as.vector(ds$v5), format = "%Y-%m-%d %H:%M:%S")
                     ))
    }) 
    
    
    test_that("text to datetime", {
        ds$text_times <- format(as.vector(ds$v5), format = "%Y-%m-%d")
        ds$times_from_text <- as.Datetime(ds$text_times, format = "%Y-%m-%d")
        expect_true(is.derived(ds$times_from_text))
        expect_true(is.Datetime(ds$times_from_text))
        expect_equal(as.vector(ds$times_from_text), 
                     as.vector(ds$v5))
    }) 
    
    test_that("numeric to datetime", {
        ds$num_times <- as.numeric(as.vector(ds$v5))
        ds$times_from_num <- as.Datetime(ds$num_times, resolution = "D")
        expect_true(is.derived(ds$times_from_num))
        expect_true(is.Datetime(ds$times_from_num))
        expect_equal(as.vector(ds$times_from_num), 
                     as.vector(ds$v5))
 
        # different resolutions change the result
        ds$num_times_secs <- rep(365*24*60*60, 20)
        ds$times_from_secs <- as.Datetime(ds$num_times_secs, resolution = "s")
        expect_true(is.derived(ds$times_from_secs))
        expect_true(is.Datetime(ds$times_from_secs))
        expect_equal(as.vector(ds$times_from_secs), 
                     as.POSIXlt(rep("1971-01-01", 20), tz = "UTC"))  
        
        ds$num_times_years <- rep(1, 20)
        ds$times_from_years <- as.Datetime(ds$num_times_years, resolution = "Y")
        expect_true(is.derived(ds$times_from_years))
        expect_true(is.Datetime(ds$times_from_years))
        # need rollup() here because the package doesn't currently support other rollups
        expect_equal(as.vector(rollup(ds$times_from_years, "D")), 
                     as.Date(rep("1971-01-01", 20)))
    }) 
})
