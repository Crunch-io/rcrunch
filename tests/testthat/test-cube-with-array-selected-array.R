context("Cubes with categorical array and multiple response (deprecated selected_array)")
# Duplicate tests and expectations from test-cube-with-array (as well one 
# test_that block (the first one) from test-cube-mr, and the last block from 
# test-multitables), to ensure that uses seeing the option 
# crunch.mr.selection="selected_array" get the results they expect.


with_mock_crunch({
    with(temp.options(crunch.mr.selection="selected_array"), {
        ds <- loadDataset("test ds")
        
        test_that("users can change the crunch.mr.selection option", {
            expect_GET(crtabs(~mymrset, data = ds),
                       "https://app.crunch.io/api/datasets/1/cube/?query=%7B%22dimensions%22%3A%5B%7B%22each%22%3A%22https%3A%2F%2Fapp.crunch.io%2Fapi%2Fdatasets%2F1%2Fvariables%2Fmymrset%2F%22%7D%2C%7B%22function%22%3A%22selected_array%22%2C%22args%22%3A%5B%7B%22variable%22%3A%22https%3A%2F%2Fapp.crunch.io%2Fapi%2Fdatasets%2F1%2Fvariables%2Fmymrset%2F%22%7D%5D%7D%5D%2C%22measures%22%3A%7B%22count%22%3A%7B%22function%22%3A%22cube_count%22%2C%22args%22%3A%5B%5D%7D%7D%2C%22weight%22%3Anull%7D&filter=%7B%7D")
        with(temp.options(crunch.mr.selection="not_a_selection_method"), {
            expect_error(crtabs(~mymrset, data = ds),
                         paste0("The option ", dQuote("crunch.mr.selection"),
                                " must be either ", dQuote("as_selected"),
                                " (the default) or ", dQuote("selected_array")),
                         fixed = TRUE)
            })
        })
    })
})

with_test_authentication({
    with(temp.options(crunch.mr.selection="selected_array"), {
        cubemrdf <- mrdf
        cubemrdf$v5 <- as.factor(c("A", "A", "B", "B"))
        mrds <- mrdf.setup(newDataset(cubemrdf), selections="1.0")

        test_that("univariate multiple response cube", {
            kube <- crtabs(~ MR, data=mrds)
            expect_is(kube, "CrunchCube")
            expect_equivalent(as.array(kube),
                              array(c(2, 1, 1), dim=c(3L),
                                    dimnames=list(MR=c("mr_1", "mr_2", "mr_3"))))
        })

        test_that("bivariate cube with MR", {
            kube <- crtabs(~ MR + v4, data=mrds)
            expect_is(kube, "CrunchCube")
            expect_equivalent(as.array(kube),
                              array(c(2, 1, 1, 0, 0, 0), dim=c(3L, 2L),
                                    dimnames=list(MR=c("mr_1", "mr_2", "mr_3"),
                                                  v4=c("B", "C"))))

            kube <- crtabs(~ v4 + MR, data=mrds)
            expect_is(kube, "CrunchCube")
            expect_equivalent(as.array(kube),
                              array(c(2, 0, 1, 0, 1, 0), dim=c(2L, 3L),
                                    dimnames=list(v4=c("B", "C"),
                                                  MR=c("mr_1", "mr_2", "mr_3"))))

            kube <- crtabs(~ v4 + MR, data=mrds, useNA="ifany")
            expect_is(kube, "CrunchCube")
            expect_equivalent(as.array(kube),
                              array(c(2, 0, 1, 0, 1, 0, 0, 1), dim=c(2L, 4L),
                                    dimnames=list(v4=c("B", "C"),
                                                  MR=c("mr_1", "mr_2", "mr_3", "<NA>"))))

            kube@useNA <- "always"
            expect_equivalent(as.array(kube),
                              array(c(2, 0, 0,
                                      1, 0, 0,
                                      1, 0, 0,
                                      0, 1, 0), dim=c(3L, 4L),
                                    dimnames=list(v4=c("B", "C", "No Data"),
                                                  MR=c("mr_1", "mr_2", "mr_3", "<NA>"))))
        })

        c1 <- crtabs(~ MR, data=mrds)
        test_that("prop.table on univariate MR without NAs", {
            expect_equivalent(prop.table(c1),
                              array(c(2/3, 1/3, 1/3), dim=c(3L),
                                    dimnames=list(MR=c("mr_1", "mr_2", "mr_3"))))
        })
        test_that("prop.table on univariate MR, useNA=always", {
            c2 <- c1
            c2@useNA <- "always"
            expect_equivalent(prop.table(c2),
                              array(c(.5, .25, .25, .25), dim=c(4L),
                                    dimnames=list(MR=c("mr_1", "mr_2", "mr_3", "<NA>"))))
        })

        c1 <- crtabs(~ v5 + MR, data=mrds)
        #    MR
        # v5  mr_1 mr_2 mr_3
        #   A    1    0    0
        #   B    1    1    1
        test_that("prop.table on bivariate with MR, no NAs", {
            expect_equivalent(as.array(c1),
                              array(c(1, 1, 0, 1, 0, 1), dim=c(2L, 3L),
                                    dimnames=list(v5=c("A", "B"),
                                                  MR=c("mr_1", "mr_2", "mr_3"))))
            expect_equivalent(margin.table(c1), 3)
            expect_equivalent(prop.table(c1),
                              array(c(1, 1, 0, 1, 0, 1)/3, dim=c(2L, 3L),
                                    dimnames=list(v5=c("A", "B"),
                                                  MR=c("mr_1", "mr_2", "mr_3"))))
            expect_equivalent(margin.table(c1, 1),
                              as.array(c(2, 1)))
            expect_equivalent(prop.table(c1, margin=1),
                              array(c(.5, 1, 0, 1, 0, 1), dim=c(2L, 3L),
                                    dimnames=list(v5=c("A", "B"),
                                                  MR=c("mr_1", "mr_2", "mr_3"))))
            expect_equivalent(margin.table(c1, margin=2),
                              as.array(c(2, 1, 1)))
            expect_equivalent(prop.table(c1, margin=2),
                              array(c(.5, .5, 0, 1, 0, 1), dim=c(2L, 3L),
                                    dimnames=list(v5=c("A", "B"),
                                                  MR=c("mr_1", "mr_2", "mr_3"))))
        })
        c2 <- c1
        c2@useNA <- "ifany"
        #    MR
        # v5  mr_1 mr_2 mr_3 <NA>
        #   A    1    0    0    0
        #   B    1    1    1    1
        test_that("prop.table on bivariate with MR, margin=NULL, useNA=ifany", {
            expect_equivalent(as.array(c2),
                              array(c(1, 1, 0, 1, 0, 1, 0, 1), dim=c(2L, 4L),
                                    dimnames=list(v5=c("A", "B"),
                                                  MR=c("mr_1", "mr_2", "mr_3", "<NA>"))))

            ## Sweep the whole table
            expect_equivalent(margin.table(c2), 4)
            expect_equivalent(prop.table(c2),
                              array(c(1, 1, 0, 1, 0, 1, 0, 1)/4, dim=c(2L, 4L),
                                    dimnames=list(v5=c("A", "B"),
                                                  MR=c("mr_1", "mr_2", "mr_3", "<NA>"))))
        })
        test_that("prop.table on bivariate with MR, margin=1, useNA=ifany", {
            expect_equivalent(margin.table(c2, 1),
                              as.array(c(2, 2)))
            expect_equivalent(prop.table(c2, margin=1),
                              array(c(.5, .5, 0, .5, 0, .5, 0, .5), dim=c(2L, 4L),
                                    dimnames=list(v5=c("A", "B"),
                                                  MR=c("mr_1", "mr_2", "mr_3", "<NA>"))))
        })
        test_that("prop.table on bivariate with MR, margin=2, useNA=ifany", {
            expect_equivalent(margin.table(c2, 2),
                              as.array(c(2, 1, 1, 1)))
            expect_equivalent(prop.table(c2, margin=2),
                              array(c(.5, .5, 0, 1, 0, 1, 0, 1), dim=c(2L, 4L),
                                    dimnames=list(v5=c("A", "B"),
                                                  MR=c("mr_1", "mr_2", "mr_3", "<NA>"))))
        })

        cube.as.CA <- array(c(1, 2, 2, 2, 1, 1, 1, 1, 1),
                            dim=c(3L, 3L),
                            dimnames=list(CA=c("mr_1", "mr_2", "mr_3"),
                                          CA=c("0.0", "1.0", "<NA>")))
        test_that("Cube of MR as_array", {
            kube <- crtabs(~ as_array(MR), data=mrds, useNA="ifany")
            expect_is(kube, "CrunchCube")
            expect_equivalent(as.array(kube), cube.as.CA)
        })
        mrds$MR <- undichotomize(mrds$MR)
        alias(mrds$MR) <- "CA"
        name(mrds$CA) <- "Cat array"
        test_that("'univariate' categorical array cube", {
            kube <- crtabs(~ CA, data=mrds, useNA="ifany")
            expect_is(kube, "CrunchCube")
            expect_equivalent(as.array(kube), cube.as.CA)
        })

        test_that("accessing array subvariables", {
            kube <- crtabs(~ CA$mr_1 + CA$mr_2, data=mrds, useNA="ifany")
            expect_equivalent(as.array(kube),
                              array(c(1, 1, 0, 0, 1, 0, 0, 0, 1),
                                    dim=c(3L, 3L),
                                    dimnames=list(mr_1=c("0.0", "1.0", "No Data"),
                                                  mr_2=c("0.0", "1.0", "No Data"))))
        })
        
        
        test_that("We can get an json tab book", {
            ds <- newDatasetFromFixture("apidocs")
            expect_error(m <- newMultitable(~ allpets + q1, data=ds), paste0(
                "Multitables do not support specifications that use deprecated",
                " multiple response functions. Do you have `options(crunch.mr.",
                "selection = \"selected_array\")` set? If so, please change it",
                " to `options(crunch.mr.selection = \"as_selected\")` and try ",
                "to create the multitable over again."
            ), fixed = TRUE)
            
            skip_locally("Vagrant host doesn't serve files correctly")
            # fake the multitable to have selected_array (which shouldn't exist, but just in case!)
            m_hacked <- newMultitable(~ petloc + q1, data=ds)
            m_hacked@body$template[[1]]$query[[2]] <- zfunc("selected_array", ds$allpets)
            expect_error(book <- tabBook(m_hacked, data=ds), paste0(
                "`tabBook` does not support multitables that use deprecated ",
                "multiple response functions. Do you have `options(crunch.mr.",
                "selection = \"selected_array\")` set? If so, please change it",
                " to `options(crunch.mr.selection = \"as_selected\")` and try ",
                "to create the multitable and then tab book over again."
            ), fixed = TRUE)

            # but non-MR multitables work fine.
            m <- newMultitable(~ petloc + q1, data=ds)
            book <- tabBook(m, data=ds)
            expect_is(book, "TabBookResult")
            expect_identical(dim(book), c(ncol(ds), 3L))
            expect_identical(names(book), names(variables(ds)))  
            })

    })
})
