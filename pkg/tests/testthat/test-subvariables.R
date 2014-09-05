context("Subvariables")

with(fake.HTTP, {
    test.ds <- loadDataset("test ds")
    mr <- test.ds$mymrset
    
    test_that("setup", {
        expect_true(is.Multiple(mr))
    })
    
    test_that("subvariables are what we think", {
        expect_true(inherits(subvariables(mr), "Subvariables"))
        expect_identical(names(subvariables(mr)), c("First", "Second", "Last"))
    })
    
    test_that("subvariable name setter error checking", {
        expect_error(names(subvariables(mr)) <- 1:3)
        expect_error(names(subvariables(mr)) <- c("First", "Second"))
        expect_error(names(subvariables(mr)) <- c("First", "First", "First"))
    })
    
    test_that("[.Subvariables", {
        expect_true(inherits(subvariables(mr)[1:2], "Subvariables"))
        expect_true(inherits(subvariables(mr)[c("First", "Last")],
            "Subvariables"))
        expect_error(subvariables(mr)[c("First", "Other")],
            "Undefined subvariables selected")
    })
    
    test_that("subvariable setter validation", {
        expect_error(subvariables(mr) <- Subvariables(), 
            "Can only reorder, not change, subvariables")
        expect_error(subvariables(mr) <- subvariables(mr)[1:2], 
            "Can only reorder, not change, subvariables")
    })
    
    test_that("can extract a subvariable as a Variable", {
        expect_true(inherits(subvariables(mr)[[1]], "CrunchVariable"))
        expect_true(is.Categorical(subvariables(mr)[[1]]))
        expect_true(inherits(subvariables(mr)[["Second"]], "CrunchVariable"))
        expect_true(is.Categorical(subvariables(mr)[["Second"]]))
        expect_true(inherits(subvariables(mr)$Second, "CrunchVariable"))
        expect_true(is.Categorical(subvariables(mr)$Second))
        expect_true(is.null(subvariables(mr)$Other))
    })
    
    test_that("can extract directly from array variable", {
        expect_true(inherits(mr[[1]], "CrunchVariable"))
        expect_true(is.Categorical(mr[[1]]))
        expect_true(inherits(mr[["Second"]], "CrunchVariable"))
        expect_true(is.Categorical(mr[["Second"]]))
        expect_true(inherits(mr$Second, "CrunchVariable"))
        expect_true(is.Categorical(mr$Second))
        expect_true(is.null(mr$Other))
        
        expect_true(inherits(mr[1:2], "Subvariables"))
        expect_true(inherits(mr[c("First", "Last")],
            "Subvariables"))
        expect_error(mr[c("First", "Other")],
            "Undefined subvariables selected")
    })
    
    test_that("show method for Subvariables", {
        mr <- refresh(mr)
        expect_identical(showSubvariables(subvariables(mr)), 
            "\n Subvariables: \n   $`First`\n   $`Second`\n   $`Last`\n\n")
    })
})

if (run.integration.tests) {
    with(test.authentication, {
        with(test.dataset(mrdf), {
            ds <- mrdf.setup(ds, selections="1.0")
            var <- ds$test1
            test_that("setup test case 2", {
                expect_true(is.Multiple(var))
                expect_identical(names(subvariables(var)),
                    c("mr_1", "mr_2", "mr_3"))
            })
            
            test_that("can rename subvariables", {
                try(names(subvariables(var))[2] <- "M.R. Two")
                expect_identical(names(subvariables(var)),
                    c("mr_1", "M.R. Two", "mr_3"))
            })
            
            test_that("can rename one subvariable", {
                try(name(subvariables(var)[[2]]) <- "Due")
                expect_identical(names(subvariables(var)),
                    c("mr_1", "Due", "mr_3"))
                sv <- subvariables(var)
                try(name(sv[[2]]) <- "M.R. Two")
                expect_identical(names(sv),
                    c("mr_1", "M.R. Two", "mr_3"))
                expect_error(name(sv[[4]]) <- "Four",
                    "subscript out of bounds")
                expect_error(sv[[2]] <- ds$v4,
                    "Cannot add or remove subvariables")
                expect_error(sv[[2]] <- NULL,
                    "Cannot add or remove subvariables")
                expect_error(sv[[2]] <- "not a variable", 
                    "Can only assign Variables into an object of class Subvariables")
            })
            test_that("can rename some subvariables", {
                try(names(subvariables(var)[2:3]) <- c("Dois", "Três"))
                expect_identical(names(subvariables(var)),
                    c("mr_1", "Dois", "Três"))
                sv <- subvariables(var)
                try(names(sv[2:3]) <- c("M.R. Two", "mr_3"))
                expect_identical(names(sv),
                    c("mr_1", "M.R. Two", "mr_3"))
                expect_error(names(sv[3:4]) <- c("3", "4"),
                    "Subscript out of bounds: 4")
                expect_error(sv[2:3] <- c("not a variable", "nor this"), 
                    "Can only assign Variables into an object of class Subvariables")
            })
            
            test_that("can reorder subvariables", {
                try(subvariables(var) <- subvariables(var)[c(3,1,2)])
                expect_identical(names(subvariables(var)),
                    c("mr_3", "mr_1", "M.R. Two"))
                try(subvariables(var)[1:2] <- subvariables(var)[c(2,1)])
                expect_identical(names(subvariables(var)),
                    c("mr_1", "mr_3", "M.R. Two"))
            })
            test_that("can't (yet) otherwise modify subvariables", {
                expect_error(subvariables(var) <- NULL,
                    "Can only assign an object of class Subvariables")
                with(test.dataset(df, "other.ds"), {
                    fake <- Subvariables(other.ds@variables[1:3])
                    expect_error(subvariables(var) <- fake,
                        "Can only reorder, not change, subvariables")
                    expect_error(subvariables(var)[1:2] <- fake[1:2],
                        "Cannot add or remove subvariables")
                })
            })
        })
    })
}