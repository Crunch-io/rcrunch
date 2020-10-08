context("tabBook")
with_mock_crunch({
    ds <- loadDataset("test ds")
    ds2 <- loadDataset("ECON.sav")

    with_POST("https://app.crunch.io/api/datasets/1/filters/filter1/", {
        ## Mock the return of that creation
        f1 <- newFilter("A filter", ds$gender == "Male", catalog = filters(ds))
    })

    test_that("tabBook sets the right request header", {
        expect_header(
            expect_POST(
                tabBook(multitables(ds)[[1]], data = ds, output_format = "xlsx"),
                "https://app.crunch.io/api/datasets/1/multitables/ed30c4/export/"
            ),
            "Accept: application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
        )
        expect_header(
            expect_POST(
                tabBook(multitables(ds)[[1]], data = ds, output_format = "json"),
                "https://app.crunch.io/api/datasets/1/multitables/ed30c4/export/"
            ),
            "Accept: application/json"
        )
    })
    filts <- filters(ds)
    test_that("tabBook with no filter of any kind", {
        expect_equivalent(
            as.character(toJSON(standardize_tabbook_filter(ds, NULL))),
            "{}"
        )
    })
    test_that("tabBook filter argument with chr name", {
        expect_equivalent(
            as.character(toJSON(standardize_tabbook_filter(ds, "Public filter"))),
            '[{"filter":"https://app.crunch.io/api/datasets/1/filters/filter2/"}]'
        )
    })

    test_that("tabBook filter argument with filter expression", {
        expect_equivalent(
            as.character(toJSON(standardize_tabbook_filter(ds[ds$gender == "Male",], NULL))),
            paste0(
                '[{\"function\":\"==\",\"args\":',
                '[{\"variable\":\"https://app.crunch.io/api/datasets/1/variables/gender/\"},',
                '{\"value\":1}],\"name\":\"gender == \\\"Male\\\"\"}]'
            )
        )
    })

    test_that("tabBook filter argument with filter object", {
        expect_equivalent(
            as.character(toJSON(standardize_tabbook_filter(ds, f1))), #f1 mock created at top
            '[{"filter":"https://app.crunch.io/api/datasets/1/filters/filter1/"}]'
        )
    })

    test_that("tabBook filter argument with two chr filter names", {
        expect_equivalent(
            as.character(toJSON(standardize_tabbook_filter(
                ds,
                c("Occasional Political Interest", "Public filter")
            ))),
            paste0(
                '[{"filter":"https://app.crunch.io/api/datasets/1/filters/filter1/"},',
                '{"filter":"https://app.crunch.io/api/datasets/1/filters/filter2/"}]'
            )
        )
    })

    test_that("tabBook filter argument with chr and filter expression", {
        expect_equivalent(
            as.character(toJSON(standardize_tabbook_filter(
                ds[ds$gender == "Male",], "Public filter"
            ))),
            paste0(
                '[{\"filter\":\"https://app.crunch.io/api/datasets/1/filters/filter2/\"},',
                '{\"function\":\"==\",\"args\":[{\"variable\":',
                '\"https://app.crunch.io/api/datasets/1/variables/gender/\"},',
                '{\"value\":1}],\"name\":\"gender == \\\"Male\\\"\"}]'
            )
        )
    })

    test_that("tabBook filter argument with filter object and filter expression", {
        expect_equal(
            as.character(toJSON(standardize_tabbook_filter(ds[ds$gender == "Male",], f1))),
            paste0(
                '[{\"filter\":\"https://app.crunch.io/api/datasets/1/filters/filter1/\"},',
                '{\"function\":\"==\",\"args\":[{\"variable\":',
                '\"https://app.crunch.io/api/datasets/1/variables/gender/\"},{\"value\":1}],',
                '\"name\":\"gender == \\\"Male\\\"\"}]'
            )
        )
    })

    test_that("tabBook can return a subset of variables", {
        expect_equivalent(weight(ds2), ds[["birthyr"]])
        m <- multitables(ds2)[[1]]
        expect_header(
            expect_POST(
                tabBook(m, data = ds2[c("gender", "starttime")]),
                "https://app.crunch.io/api/datasets/3/multitables/ed30c4/export/",
                '{\"filter\":null,',
                '\"weight\":"https://app.crunch.io/api/datasets/3/variables/birthyr/",',
                '\"options\":[],"where":{"function":"select","args":[{"map":',
                '{"66ae9881e3524f7db84970d556c34552":',
                '{"variable":"https://app.crunch.io/api/datasets/3/variables/gender/"},',
                '"d7c21314ca9e453c93069168681a285c"',
                ':{"variable":"https://app.crunch.io/api/datasets/3/variables/starttime/"}}}]}'
            ),
            "Accept: application/json"
        )
    })

    test_that("tabBook with options", {
        expect_POST(
            tabBook(multitables(ds)[[1]],
                    data = ds, output_format = "json", format = list(pval_colors = TRUE))
            ,
            "https://app.crunch.io/api/datasets/1/multitables/ed30c4/export/",
            '{\"filter\":null,\"weight\":null,\"options\":{"format":{"pval_colors":true}}}'
        )
    })

    test_that("tabBook warning when using format argument", {
        expect_warning(
            expect_POST(
                tabBook(multitables(ds)[[1]],
                        data = ds, format = "json")
                ,
                "https://app.crunch.io/api/datasets/1/multitables/ed30c4/export/",
                '{\"filter\":null,\"weight\":null,\"options\":{}}'
            ),
            "Use `output_format`"
        )
    })

    test_that("tabBook warning when using legacy endpoint", {
        with(temp.option(use.legacy.tabbook.endpoint = TRUE), {
            expect_warning(
                expect_POST(
                    tabBook(multitables(ds)[[1]],
                            data = ds, output_format = "json")
                    ,
                    "https://app.crunch.io/api/datasets/1/multitables/ed30c4/tabbook/",
                    '{\"filter\":null,\"weight\":null,\"options\":[]}'
                ),
                "The legacy tabbook endpoint has been deprecated and will be removed in the future."
            )
        })
    })

    with_POST("https://app.crunch.io/api/datasets/1/multitables/apidocs-tabbook/", {
        book <- tabBook(multitables(ds)[[1]], data = ds, output_format = "json")
        test_that("tabBook JSON returns TabBookResult", {
            expect_is(book, "TabBookResult")
        })
        test_that("TabBookResult and MultitableResult size/extract methods", {
            expect_length(book, 9)
            expect_is(book[[1]], "MultitableResult")
            expect_length(book[[1]], 3)
            expect_identical(dim(book), c(9L, 3L))
            expect_is(book[[1]][[1]], "CrunchCube")
        })
        test_that("tab book print methods", {
            ## Print method for MultitableResult cbinds together the Cubes
            out <- cubify(
                4, 4, 0, 1, 0, 1, 1,
                5, 0, 5, 3, 2, 1, 1,
                5, 1, 3, 5, 2, 1, 1,
                dims = list(
                    c("Cat", "Dog", "Bird"),
                    c("", "Cat", "Dog", "Bird", "Cat", "Dog", "Bird")
                )
            )
            expect_prints(print(book[[1]]), get_output(out))
            ## TODO: print method for TabBookResult
        })

        test_that("The first result in a MultitableResult has 2 dimensions", {
            expect_identical(dim(book[[1]][[1]]), c(3L, 1L))
        })
        test_that("prop.table methods", {
            ## prop.table on a TabBookResult returns a list of lists of prop.tables
            expect_identical(
                prop.table(book)[[2]][[2]],
                prop.table(book[[2]][[2]])
            )
            expect_identical(
                prop.table(book, 1)[[2]][[2]],
                prop.table(book[[2]][[2]], 1)
            )
            expect_identical(
                prop.table(book, 2)[[2]][[2]],
                prop.table(book[[2]][[2]], 2)
            )
        })
        ## TODO: something more with variable metadata? For cubes more generally?
        ## --> are descriptions coming from backend if they exist?

        with_POST("https://app.crunch.io/api/datasets/1/multitables/apidocs-mr-ca-tabbook/", {
            ## This mock was taken from the integration test below
            book <- tabBook(multitables(ds)[[1]], data = ds, output_format = "json")
            test_that("tabBook JSON returns TabBookResult", {
                expect_is(book, "TabBookResult")
            })
            test_that("TabBookResult and MultitableResult size/extract methods", {
                expect_length(book, 1)
                expect_is(book[[1]], "MultitableResult")
                expect_length(book[[1]], 2)
                expect_identical(dim(book), c(1L, 2L))
                expect_is(book[[1]][[1]], "CrunchCube")
            })
            test_that("tab book print methods", {
                ## TODO: print method for TabBookResult
            })

            test_that("The first result in a MultitableResult has 3 dimensions", {
                expect_identical(dim(showMissing(book[[1]][[1]])), c(5L, 1L, 2L))
            })
            test_that("dim names", {
                expect_identical(
                    names(book[[1]][[1]]),
                    c("Pets by location", "Total", "Pets by location")
                )
                expect_identical(
                    names(book[[1]][[2]]),
                    c(
                        "Pets by location", "All pets owned",
                        "Pets by location"
                    )
                )
            })
        })

        with_POST("https://app.crunch.io/api/datasets/1/multitables/apidocs-ca-mr-tabbook/", {
            ## This mock was taken from the integration test below
            book <- tabBook(multitables(ds)[[1]], data = ds, output_format = "json")
            test_that("tabBook JSON returns TabBookResult", {
                expect_is(book, "TabBookResult")
            })
            test_that("TabBookResult and MultitableResult size/extract methods", {
                expect_length(book, 1)
                expect_is(book[[1]], "MultitableResult")
                expect_length(book[[1]], 2)
                expect_identical(dim(book), c(1L, 2L))
                expect_is(book[[1]][[1]], "CrunchCube")
            })
            test_that("tab book print methods", {
                ## TODO: print method for TabBookResult
            })
            test_that("The first result in a MultitableResult has 3 dimensions", {
                expect_identical(dim(showMissing(book[[1]][[1]])), c(3L, 1L))
            })
            test_that("dim names", {
                expect_identical(
                    names(book[[1]][[1]]),
                    c("All pets owned", "Total")
                )
                expect_identical(
                    names(book[[1]][[2]]),
                    c(
                        "Pets by location", "Pets by location",
                        "All pets owned"
                    )
                )
            })
        })
    })

    with_POST("https://app.crunch.io/api/datasets/1/multitables/apidocs-tabbook/", {
        ## This mock was taken from the integration test below
        book <- tabBook(multitables(ds)[[1]], data = ds)
        test_that("tabBook from apidocs dataset (mock)", {
            expect_is(book, "TabBookResult")
            expect_identical(dim(book), c(9L, 3L))
            expect_identical(
                prop.table(book, 2)[[2]][[2]],
                prop.table(book[[2]][[2]], 2)
            )
        })
        test_that("tab book names", {
            expect_identical(
                names(book)[1:4],
                c("All pets owned", "Pet", "Pets by location", "Number of dogs")
            )
            expect_is(book[["Pet"]], "MultitableResult")
            expect_null(book[["NOTVALID"]])
        })
        test_that("tabBook JSON with arrays returns TabBookResult", {
            expect_is(book, "TabBookResult")
            expect_identical(
                prop.table(book, 2)[[3]][[2]],
                prop.table(book[[3]][[2]], 2)
            )
        })
    })
})


with_test_authentication({
    ds <- newDatasetFromFixture("apidocs")
    mult <- newMultitable(~ allpets + q1, data = ds)

    test_that("We can get an xlsx tab book", {
        skip_on_local_backend("Vagrant host doesn't serve files correctly")
        f <- tempfile()
        out <- tabBook(mult, data = ds, output_format = "xlsx", file = f)
        expect_true(file.exists(out))
    })

    test_that("We can get an json tab book", {
        skip("multitables and multiple response need more work.")
        skip_on_local_backend("Vagrant host doesn't serve files correctly")
        book <- tabBook(mult, data = ds)
        expect_is(book, "TabBookResult")
        expect_identical(dim(book), c(ncol(ds), 3L))
        expect_identical(names(book), names(variables(ds)))
    })
})