context("Update a dataset")

## For functional transformations, distinguish between "do this update to fix this data once" and "do this to every batch I append too"?

if (run.integration.tests) {
    with(test.authentication, {
        with(test.dataset(df), {
            test_that("Can update numeric variable with values", {
                try(ds$v3 <- 9:28)
                test <- as.vector(ds$v3) - df$v3
                expect_true(all(test == 1))
            })
            
            test_that("Value recycling on insert is consistent with R", {
                try(ds$v3 <- 1)
                expect_true(all(as.vector(ds$v3) == 1))
            })
            
            test_that("Can update numeric variable with filter and values", {
                expect_error(ds$v3[1:10] <- 2, 
                    "Update with numeric index not yet supported")
                skip({
                    try(ds$v3[1:10] <- 2)
                    expect_equivalent(mean(ds$v3), 1.5)
                }, "How do I create filter with numeric indices?")
                ds$v3 <- c(rep(2, 10), rep(1, 10))
                expect_equivalent(mean(ds$v3), 1.5)
                try(ds$v3[ds$v3 == 1] <- 3)
                expect_equivalent(mean(ds$v3), 2.5)
                try(ds[ds$v3 == 2, "v3"] <- 4)
                expect_equivalent(mean(ds$v3), 3.5)
                try(ds$v3[] <- c(rep(5, 10), rep(7, 10)))
                expect_equivalent(mean(ds$v3), 6)
            })
            
            test_that("Can update numeric variable with expresssion", {
                try(ds$v3 <- ds$v3 + 2)
                expect_equivalent(as.vector(ds$v3), c(rep(7, 10), rep(9, 10)))
            })
            
            test_that("Can filter on is.na", {
                try(ds$v3[is.na(ds$v2)] <- 0)
                expect_equivalent(as.vector(ds$v3), 
                    c(rep(7, 10), rep(9, 5), rep(0, 5)))
            })
            
            test_that("Can update text", {
                try(ds$v2[is.na(ds$v1)] <- "z")
                expect_identical(as.vector(ds$v2)[1:8], 
                    c(rep("z", 5), "f", "g", "h"))
                try(ds[ds$v2 %in% "z", "v2"] <- "y")
                expect_identical(as.vector(ds$v2)[1:8], 
                    c(rep("y", 5), "f", "g", "h"))
            })
            
            test_that("Can update datetime", {
                newvals <- as.Date(0:12, origin="1985-10-26")
                try(ds$v5[ds$v5 >= as.Date("1955-11-12")] <- newvals)
                expect_identical(max(ds$v5), as.POSIXct("1985-11-07"))
            })
            
            date.before <- rep(c("2014-04-15", "2014-08-15"), 2)
            date.after <- c("2014-04-15", "2014-09-15", "2014-04-15",
                "2014-09-15")
            date.df <- data.frame(wave=as.Date(date.before))
            with(test.dataset(date.df, "date.ds"), {
                test_that("Another datetime update", {
                    expect_identical(as.vector(date.ds$wave),
                        as.POSIXct(date.before))
                    try(date.ds$wave[date.ds$wave == as.Date("2014-08-15")] <- as.Date("2014-09-15"))
                    expect_identical(as.vector(date.ds$wave),
                        as.POSIXct(date.after))
                })
            })
            
            ## Categorical
            try(ds$v4[is.na(ds$v2)] <- "B")
            test_that("Can update categorical variables with character", {
                expect_identical(table(ds$v4)["B"], c(B=13L))
            })
            try(ds$v4[is.na(ds$v2)] <- factor("C"))
            test_that("Can update categorical with factor", {
                expect_identical(table(ds$v4)["C"], c(C=12L))
            })
            try(ds$v4[is.na(ds$v2)] <- c(2,1,2,1,2))
            test_that("Can update categorical with numeric (ids)", {
                expect_equivalent(table(ds$v4), table(df$v4))
            })
            test_that("Validation on categorical update", {
                expect_error(ds$v4[is.na(ds$v2)] <- as.factor(LETTERS[1:5]),
                    "Input values A, D, and E are not present in the category names of variable")
            })
            
            test_that("Can update categorical that has missings with more missings", {
                
            })
            test_that("Updating categorical with new categories (incl. missing)", {
                ## Temporary:
                expect_error(ds$v4[is.na(ds$v2)] <- NA,
                    "Cannot update CategoricalVariable with type logical")
                skip({
                    expect_identical(length(categories(ds$v4)), 2L)
                    try(ds$v4[is.na(ds$v2)] <- NA)
                    expect_identical(length(categories(ds$v4)), 3L)
                    expect_identical(as.vector(ds$v4),
                        as.factor(c(rep(LETTERS[2:3], length=15), rep(NA, 5))))
                    try(ds$v1[is.na(ds$v4)] <- NA)
                    expect_identical(as.vector(ds$v1[is.na(ds$v4)]), 
                        rep(NA_integer_, 5))
                    expect_identical(sum(is.na(as.vector(ds$v1))), 10L)
                }, "No Data isn't added, and is.na(ds$v4) returns 0 rows")
            })
            
            test_that("reset test dataset", {
                ds$v3[] <- df$v3
                expect_equivalent(as.vector(ds$v3), df$v3)
                ds$v4[] <- rep(1:2, length=20)
                expect_equivalent(table(ds$v4), table(df$v4))
            })
            
            test_that("Can 'mark missing'", {
                expect_error(is.na(ds$v5) <- ds$v4 == "B",
                    "is.na<- not yet supported")
                skip({
                    try(is.na(ds$v3) <- ds$v3 >= 10 & ds$v3 < 13)
                    expect_equivalent(as.vector(ds$v3),
                        c(8, 9, rep(NA, 3), 13:27))
                    try(is.na(ds$v5) <- ds$v4 == "B")
                    expect_identical(sum(is.na(as.vector(ds$v5))), 10L)
                    try(is.na(ds$v3) <- ds$v4 %in% "C")
                    print(as.vector(ds$v3))
                    expect_identical(sum(is.na(as.vector(ds$v3))), 10L)
                }, "mark missing as function seems not to work")
            })
        })
        
        with(test.dataset(mrdf), {
            ds <- mrdf.setup(ds)
            test_that("Subvariable values before trying to update", {
                expect_equivalent(as.vector(ds$CA$mr_1), 
                    as.factor(c("1.0", "0.0", "1.0", NA)))
                expect_equivalent(as.vector(ds$CA$mr_2), 
                    as.factor(c("0.0", "0.0", "1.0", NA)))
                expect_equivalent(as.vector(ds$CA$mr_3), 
                    as.factor(c("0.0", "0.0", "1.0", NA)))
            })
            try(ds$CA[ds$v4 == "B"] <- c("1.0"))
            test_that("Can update array subvariables", {
                expect_equivalent(as.vector(ds$CA$mr_1), 
                    as.factor(c("1.0", "0.0", "1.0", NA)))
                expect_equivalent(as.vector(ds$CA$mr_2), 
                    as.factor(c("1.0", "0.0", "1.0", NA)))
                expect_equivalent(as.vector(ds$CA$mr_3), 
                    as.factor(c("1.0", "0.0", "1.0", NA)))
            })
        })
        
        with(test.dataset(df), {
            len <- try(length(as.vector(ds$v4[ds$v4 == "B"])))
            test_that("setup for update with wrong number of values", {
                expect_identical(len, 10L)
            })
            test_that("Trying to update with too many values fails", {
                expect_error(ds$v4[ds$v4 == "B"] <- rep(1, len + 5), 
                    "expected 10 values, got 15")
            })
            test_that("Trying to update with too few values fails", {
                expect_error(ds$v4[ds$v4 == "B"] <- rep(1, len - 3), 
                    "expected 10 values, got 7")
            })
            test_that("Trying to update with different filters fails", {
                expect_error(ds$v4[ds$v4 == "B"] <- ds$v4[ds$v4 == "C"], 
                    "Cannot update a variable with a value that has a different filter")
                expect_equivalent(as.numeric(table(ds$v4)), c(10, 10))
                try(ds$v4[ds$v4 == "B"] <- as.vector(ds$v4[ds$v4 == "C"]))
                expect_true(all(as.vector(ds$v4) == "C"))
            })
            
            test_that("Trying to update with the wrong data type fails", {
                expect_error(ds$v3[is.na(ds$v1)] <- letters[3:7],
                    "Cannot update NumericVariable with type character")
            })
        })
    })
}