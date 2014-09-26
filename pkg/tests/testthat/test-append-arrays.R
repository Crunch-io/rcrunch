context("Appending datasets with arrays")

if (run.integration.tests) {
    with(test.authentication, {
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
                out <- suppressMessages(try(appendDataset(part1, part2)))
                test_that("identical datasets with arrays can append", {
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
                out <- suppressMessages(try(appendDataset(part1, part2)))
                test_that("arrays with different subvariables can append", {
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
        
        sparsemrdf1 <- data.frame(v4=factor(rep(c("a", "b"), 500)))
        sparsemrdf2 <- data.frame(mr_1=c(1,0,1,1,0, rep(NA, 995)),
                           mr_2=c(rep(NA, 995), 0, 1, 1, 1, 0),
                           v4=as.factor(LETTERS[2:3]))
        with(test.dataset(sparsemrdf1, "part1"), {
            with(test.dataset(sparsemrdf2, "part2"), {
                part2 <- mrdf.setup(part2)
                out <- suppressMessages(try(appendDataset(part1, part2)))
                test_that("Sparse append with array", {
                    expect_identical(nrow(out), 2000L)
                    expect_identical(as.vector(out$CA$mr_2),
                        factor(c(rep(NA, 1995), "0.0", "1.0", "1.0", "1.0", "0.0")))
                })
            })
        })
    })
}