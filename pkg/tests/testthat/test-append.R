context("Append datasets")

c1 <- list()
c2 <- list(var1=list(
    conflicts=list(list(
        message="No good",
        resolution="But I fixed it already"
    )),
    metadata=list(
        name="First"
    )
))
c3 <- list(
    var2=list(
        conflicts=list(list(
            message="No good",
            resolution="But I fixed it already"
        )),
        metadata=list(
            name="Second"
        )
    ),
    var1=list(
        conflicts=list(
            list(
                message="No good",
                resolution="But I fixed it already"
            ), 
            list(
                message="Oh, and there was another problem",
                resolution="But it's also cool"
            )
        ),
        metadata=list(
            name="First"
        )
    )
)

test_that("Simple conflict messages are formatted correctly", {
    expect_equivalent(flattenConflicts(c3), 
        data.frame(
            message=c("No good", "No good", "Oh, and there was another problem"),
            resolution=c("But I fixed it already", "But I fixed it already", "But it's also cool"),
            url=c("var2", "var1", "var1"),
            name=c("Second", "First", "First"),
            stringsAsFactors=FALSE))
    
    expect_identical(formatConflicts(c1), "No conflicts.")
    expect_identical(formatConflicts(c2), 
        paste("Conflict: No good; Resolution: But I fixed it already; 1 variable:", dQuote("First")))
    expect_identical(formatConflicts(c3), 
        c(paste("Conflict: No good; Resolution: But I fixed it already; 2 variables:", dQuote("Second"), "and", dQuote("First")),
        paste("Conflict: Oh, and there was another problem; Resolution: But it's also cool; 1 variable:", dQuote("First"))))
})

source("conflicts.R")
test_that("Complex conflicts are formatted", {
    expect_identical(formatConflicts(mock.conflicts), 
        c(paste("Conflict: Only in existing dataset; Resolution: Additional rows will be marked missing.; 1 variable:", dQuote("mr_1")),
        paste("Conflict: Only in new dataset; Resolution: Variable will be added with existing rows marked missing.; 1 variable:", dQuote("mr_3")),
        paste("Conflict: Subvariables didn't match; Resolution: Union of subvariables will be used; 1 variable:", dQuote("MR"))))
})

test_that("crunchTimeout", {
    opt <- getOption("crunch.timeout")
        options(crunch.timeout=7)
        expect_identical(crunchTimeout(), 7)
        options(crunch.timeout=NULL)
        expect_identical(crunchTimeout(), 60)
        options(crunch.timeout=list())
        expect_identical(crunchTimeout(), 60)        
    options(crunch.timeout=opt)
})

test_that("askForPermission says no if not interactive", {
    expect_false(askForPermission())
})

if (run.integration.tests) {
    with(test.authentication, {
        with(test.dataset(df, "part1"), {
            cats <- categories(part1$v4)
            with(test.dataset(df, "part2"), {
                p1.batches <- batches(part1)
                test_that("Batches before appending are right", {
                    expect_true(inherits(p1.batches, "ShojiCatalog"))
                    expect_identical(length(p1.batches), 1L)
                    expect_identical(length(batches(part2)), 1L)
                })
                out <- suppressMessages(try(addBatchToDataset(part1, part2)))
                test_that("can add batches to dataset", {
                    expect_true(is.character(out))
                    expect_true(grepl("/batches/", out))
                    expect_identical(length(batches(part1)), 2L)
                    expect_true(out %in% urls(batches(part1)))
                })
            })
        })
        with(test.dataset(df, "part1"), {
            cats <- categories(part1$v4)
            with(test.dataset(df, "part2"), {
                v3.1 <- as.vector(part1$v3)
                v3.2 <- as.vector(part2$v3)
                test_that("Setup for appending identical datasets", {
                    expect_true(is.numeric(v3.1))
                    expect_true(is.numeric(v3.2))
                    expect_equivalent(v3.1, df$v3)
                    expect_equivalent(v3.2, df$v3)
                    expect_identical(dim(part1), dim(part2))
                    expect_identical(dim(part1), dim(df))
                    expect_identical(length(batches(part1)), 1L)
                    expect_identical(length(batches(part2)), 1L)
                })
                out <- suppressMessages(try(appendDataset(part1, part2)))
                test_that("append handles two identical Datasets", {
                    expect_false(is.error(out))
                    expect_true(is.dataset(out))
                    expect_identical(self(out), self(part1))
                    expect_identical(length(batches(out)), 2L)
                    expect_identical(dim(out), c(nrow(df)*2L, ncol(df)))
                    expect_identical(getNrow(out), nrow(df)*2L)
                    expect_identical(nrow(out), length(as.vector(out$v3)))
                    expect_identical(categories(out$v4)[1:2], cats)
                    expect_equivalent(as.vector(out$v3), rep(df$v3, 2))
                    expect_identical(as.vector(out$v3), c(v3.1, v3.2))
                })
            })
        })

        try({
            file1 <- newDatasetFromFile(testfile.csv, name=now())
            file2 <- newDatasetFromFile(testfile.csv, name=now())
            v3.1 <- as.vector(file1$V3)
            v3.2 <- as.vector(file2$V3)
            
            test_that("our assumptions about these two datasets from file", {
                expect_true(is.numeric(v3.1))
                expect_true(is.numeric(v3.2))
                expect_equivalent(v3.1, testfile.df$V3)
                expect_equivalent(v3.2, testfile.df$V3)
                expect_identical(length(batches(file1)), 1L)
                expect_identical(length(batches(file2)), 1L)
            })
            
            test_that("append handles two identical Datasets from file", {
                out <- try(appendDataset(file1, file2))
                expect_false(is.error(out))
                expect_true(is.dataset(out))
                expect_identical(self(out), self(file1))
                expect_identical(length(batches(out)), 2L)
                expect_identical(dim(out),
                    c(nrow(testfile.df)*2L, ncol(testfile.df)))
                expect_identical(getNrow(out), nrow(testfile.df)*2L)
                expect_identical(nrow(out), length(as.vector(out$V3)))
                expect_equivalent(as.vector(out$V3), rep(testfile.df$V3, 2))
                expect_identical(as.vector(out$V3), c(v3.1, v3.2))  
            })

            delete(file2)
            delete(file1)
        })
        
        with(test.dataset(df[,2:5], "part1"), {
            cats <- categories(part1$v4)
            with(test.dataset(df[,1:3], "part2"), {
                p1.batches <- batches(part1)
                test_that("if I insist on confirmation, it fails if there are conflicts", {
                    expect_true(inherits(p1.batches, "ShojiCatalog"))
                    expect_identical(length(p1.batches), 1L)
                    expect_error(appendDataset(part1, part2, confirm=TRUE))
                    expect_identical(length(batches(part1)), 1L)
                })
                test_that("append handles missing variables from each", {
                    out <- try(appendDataset(part1, part2))
                    expect_false(is.error(out))
                    expect_true(is.dataset(out))
                    expect_identical(length(refresh(p1.batches)), 2L)
                    expect_identical(ncol(out), 5L)
                    expect_identical(ncol(out), length(allVariables(out)))
                    expect_true(setequal(names(out), paste0("v", 1:5)))
                    expect_identical(nrow(out), nrow(df) * 2L)
                    expect_identical(categories(out$v4)[1:2], cats)
                    expect_equivalent(as.vector(out$v3), rep(df$v3, 2))
                    expect_equivalent(as.vector(out$v1), 
                        c(rep(NA, nrow(df)), df$v1))
                    expect_identical(length(as.vector(out$v5)), 40L)
                    expect_identical(length(as.vector(out$v4)), 40L)
                    expect_equivalent(as.vector(out$v4)[1:20], df$v4)
                    expect_equivalent(as.vector(out$v4), 
                        factor(levels(df$v4)[c(df$v4, 
                            factor(rep(NA_character_, nrow(df))))]))
                    expect_equivalent(as.Date(as.vector(out$v5))[1:20], df$v5)
                    expect_equivalent(as.Date(as.vector(out$v5)), 
                        c(df$v5, rep(NA, nrow(df))))
                })
            })
        })
        
        with(test.dataset(df[,1:3], "part1"), {
            with(test.dataset(df[,2:5], "part2"), {
                cats <- categories(part2$v4)
                
                test_that("append with missing variables the other way", {
                    p1.batches <- batches(part1)
                    out <- try(appendDataset(part1, part2))
                    expect_false(is.error(out))
                    expect_true(is.dataset(out))
                    expect_identical(length(refresh(p1.batches)), 2L)
                    expect_identical(ncol(out), 5L)
                    expect_identical(ncol(out), length(allVariables(out)))
                    expect_true(setequal(names(out), paste0("v", 1:5)))
                    expect_identical(nrow(out), nrow(df) * 2L)
                    expect_identical(categories(out$v4)[1:2], cats)
                    expect_equivalent(as.vector(out$v3), rep(df$v3, 2))
                    expect_equivalent(as.vector(out$v1), 
                        c(df$v1, rep(NA, nrow(df))))
                    expect_identical(length(as.vector(out$v5)), 40L)
                    expect_identical(length(as.vector(out$v4)), 40L)
                    expect_equivalent(as.vector(out$v4)[21:40], df$v4)
                    expect_equivalent(as.vector(out$v4), 
                        factor(levels(df$v4)[c(
                            factor(rep(NA_character_, nrow(df))), df$v4)]))
                    expect_equivalent(as.Date(as.vector(out$v5))[21:40], df$v5)
                    expect_equivalent(as.numeric(as.Date(as.vector(out$v5))), 
                        c(rep(NA, nrow(df)), df$v5))
                })
            })
        })
        
        with(test.dataset(df[,2:5], "part1"), {
            d2 <- df
            d2$v2 <- d2$v3 ## v2 was text, now is numeric
            with(test.dataset(d2, "part2"), {
                test_that("append fails on type mismatch", {
                    p1.batches <- batches(part1)
                    expect_true(inherits(p1.batches, "ShojiCatalog"))
                    expect_identical(length(p1.batches), 1L)
                    expect_error(appendDataset(part1, part2))
                    expect_identical(length(batches(part1)), 1L)
                })
            })
        })
        
        with(test.dataset(mrdf, "part1"), {
            part1 <- mrdf.setup(part1, selections="1.0")
            with(test.dataset(mrdf, "part2"), {
                part2 <- mrdf.setup(part2, selections="1.0")
                test_that("set up MR for appending", {
                    expect_true(is.Multiple(part1$MR))
                    expect_true(is.Multiple(part2$MR))
                    expect_identical(length(batches(part1)), 1L)
                    expect_identical(length(batches(part2)), 1L)
                })
                test_that("identical datasets with arrays can append", {
                    out <- try(appendDataset(part1, part2))
                    expect_false(is.error(out))
                    expect_true(is.dataset(out))
                    expect_identical(length(batches(out)), 2L)
                    expect_identical(dim(out), c(nrow(mrdf)*2L, 2L))
                    expect_true(is.Multiple(out$MR))
                })
            })
        })
        
        with(test.dataset(mrdf, "part1"), {
            part1 <- mrdf.setup(part1, selections="1.0")
            mr_cats <- categories(part1$MR)
            subvar_cats <- categories(part1$MR$mr_1)
            dichotomized_cats <- Categories(list(
                list(id=1, missing=FALSE, name="0.0", numeric_value=0, selected=FALSE), 
                list(id=2, missing=FALSE, name="1.0", numeric_value=1, selected=TRUE),
                list(id=-1, missing=TRUE, name="No Data", numeric_value=NULL, selected=FALSE)))
            with(test.dataset(mrdf, "part2"), {
                ## Dichotomize this way so that categories get aligned
                ## (via supertype)              
                part2 <- mrdf.setup(part2)
                unbind(part2$CA)
                part2 <- refresh(part2)
                undichotomized_cats <- Categories(list(
                    list(id=1, missing=FALSE, name="0.0", numeric_value=0),
                    list(id=2, missing=FALSE, name="1.0", numeric_value=1), 
                    list(id=-1, missing=TRUE, name="No Data", numeric_value=NULL)))
                test_that("set up MR for appending", {
                    expect_true(is.Multiple(part1$MR))
                    expect_true(is.null(part2$MR))
                    expect_identical(mr_cats, subvar_cats)
                    expect_identical(mr_cats, dichotomized_cats)
                    expect_identical(categories(part2$mr_1), 
                        undichotomized_cats)
                    expect_false(identical(dichotomized_cats,
                        undichotomized_cats)) ## Just being clear about that
                    expect_identical(as.vector(part1$MR$mr_1),
                        as.vector(part2$mr_1))
                    expect_identical(as.vector(part1$MR$mr_2),
                        as.vector(part2$mr_2))
                    expect_identical(as.vector(part1$MR$mr_3),
                        as.vector(part2$mr_3))
                })
                out <- suppressMessages(try(appendDataset(part1, part2)))
                test_that("unbound subvariables get lined up", {
                    expect_false(is.error(out))
                    expect_true(is.dataset(out))
                    expect_identical(length(batches(out)), 2L)
                    expect_identical(dim(out), c(nrow(mrdf)*2L, 2L))
                    expect_true(is.variable(out$MR))
                    # print(str(categories(out$MR)))
                    expect_identical(categories(out$MR), dichotomized_cats)
                    expect_identical(categories(out$MR$mr_1), dichotomized_cats)
                    expect_false(identical(categories(out$MR),
                        undichotomized_cats)) ## Looks like we're taking the wrong cats
                    expect_identical(as.vector(out$MR$mr_1), 
                        rep(as.vector(part2$mr_1), 2))
                    expect_true(is.Multiple(out$MR))
                    expect_identical(names(subvariables(out$MR)),
                        c("mr_1", "mr_2", "mr_3"))
                })
            })
        })
        with(test.dataset(mrdf, "part1"), {
            part1 <- mrdf.setup(part1, selections="1.0")
            mr_cats <- categories(part1$MR)
            subvar_cats <- categories(part1$MR$mr_1)
            dichotomized_cats <- Categories(list(
                list(id=1, missing=FALSE, name="0.0", numeric_value=0, selected=FALSE), 
                list(id=2, missing=FALSE, name="1.0", numeric_value=1, selected=TRUE),
                list(id=-1, missing=TRUE, name="No Data", numeric_value=NULL, selected=FALSE)))
            with(test.dataset(mrdf, "part2"), {                
                cast.these <- grep("mr_", names(part2))
                part2[cast.these] <- lapply(part2[cast.these],
                    castVariable, "categorical")
                undichotomized_cats <- Categories(list(
                    list(id=2, missing=FALSE, name="0.0", numeric_value=0),
                    list(id=1, missing=FALSE, name="1.0", numeric_value=1), 
                    list(id=-1, missing=TRUE, name="No Data", numeric_value=NULL)))
                test_that("set up MR for appending", {
                    expect_true(is.Multiple(part1$MR))
                    expect_true(is.null(part2$MR))
                    expect_identical(mr_cats, subvar_cats)
                    expect_identical(mr_cats, dichotomized_cats)
                    expect_identical(categories(part2$mr_1), 
                        undichotomized_cats)
                    expect_false(identical(dichotomized_cats,
                        undichotomized_cats)) ## Just being clear about that
                    expect_identical(as.vector(part1$MR$mr_1),
                        as.vector(part2$mr_1))
                    expect_identical(as.vector(part1$MR$mr_2),
                        as.vector(part2$mr_2))
                    expect_identical(as.vector(part1$MR$mr_3),
                        as.vector(part2$mr_3))
                })
                out <- suppressMessages(try(appendDataset(part1, part2)))
                test_that("unbound subvars with not identical cats", {
                    expect_false(is.error(out))
                    expect_true(is.dataset(out))
                    expect_identical(length(batches(out)), 2L)
                    expect_identical(dim(out), c(nrow(mrdf)*2L, 2L))
                    expect_true(is.variable(out$MR))
                    # print(str(categories(out$MR)))
                    expect_identical(categories(out$MR), dichotomized_cats)
                    expect_identical(categories(out$MR$mr_1), dichotomized_cats)
                    expect_false(identical(categories(out$MR),
                        undichotomized_cats)) ## To be clear about the problem
                    ## Not handling categories with different ids but same names
                    expect_identical(as.vector(out$MR$mr_1), 
                        rep(as.vector(part2$mr_1), 2))
                    expect_true(is.Multiple(out$MR))
                    expect_identical(names(subvariables(out$MR)),
                        c("mr_1", "mr_2", "mr_3"))
                })
            })
        })
        
        with(test.dataset(mrdf[-3], "part1"), {
            part1 <- mrdf.setup(part1, selections="1.0")
            with(test.dataset(mrdf[-1], "part2"), {
                part2 <- mrdf.setup(part2, selections="1.0")
                test_that("set up MR for appending", {
                    expect_true(is.Multiple(part1$MR))
                    expect_identical(names(subvariables(part1$MR)),
                        c("mr_1", "mr_2"))
                    expect_true(is.Multiple(part2$MR))
                    expect_identical(names(subvariables(part2$MR)),
                        c("mr_2", "mr_3"))
                })
                test_that("arrays with different subvariables can append", {
                    out <- try(appendDataset(part1, part2))
                    expect_false(is.error(out))
                    expect_true(is.dataset(out))
                    expect_identical(length(batches(out)), 2L)
                    expect_identical(dim(out), c(nrow(mrdf)*2L, 2L))
                    expect_true(is.variable(out$MR))
                    expect_true(is.Multiple(out$MR))
                    expect_identical(names(subvariables(out$MR)),
                        c("mr_1", "mr_2", "mr_3"))
                })
            })
        })
    })
}
