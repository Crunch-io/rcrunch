context("tabBook")
with_mock_crunch({
    ds <- loadDataset("test ds") ## No weight set on dataset
    ds2 <- loadDataset("ECON.sav") ## Has weight set on dataset
    ds3 <- loadDataset("multiweight tabbook ds")

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

    test_that("tabBookWeightSpec works on dataset when appending default true weight", {
        expect_equivalent(
            tabBookWeightSpec(
                ds2[c("gender", "starttime")],
                list(wt1 = "gender", wt2 = "starttime")
            ),
            data.frame(
                alias =  c("gender", "gender", "starttime", "starttime"),
                weight = c("birthyr", "wt1", "birthyr", "wt2"),
                stringsAsFactors = FALSE
            )
        )
    })

    test_that("tabBookWeightSpec works on dataset when appending default unweighted", {
        expect_equivalent(
            tabBookWeightSpec(
                ds[c("gender", "mymrset", "location")],
                list(wt1 = "gender", wt2 = "location")
            ),
            data.frame(
                alias =  c("gender", "gender", "mymrset", "location", "location"),
                weight = c("", "wt1", "", "", "wt2"),
                stringsAsFactors = FALSE
            )
        )
    })

    test_that("tabBookWeightSpec works on dataset when not appending default", {
        expect_equivalent(
            tabBookWeightSpec(
                ds[c("gender", "mymrset", "location")],
                list(wt1 = "gender", wt2 = "location"),
                append_default_wt = FALSE
            ),
            data.frame(
                alias =  c("gender",  "location"),
                weight = c("wt1", "wt2"),
                stringsAsFactors = FALSE
            )
        )
    })

    test_that("tabBookWeightSpec works on dataset when appending and complicated list input", {
        expect_equivalent(
            tabBookWeightSpec(
                ds2[c("gender", "starttime")],
                list(wt1 = "gender", wt2 = "starttime", "gender")
            ),
            data.frame(
                alias =  c("gender", "gender", "gender", "starttime", "starttime"),
                weight = c("birthyr", "wt1", "", "birthyr", "wt2"),
                stringsAsFactors = FALSE
            )
        )
    })

    test_that("tabBookWeight warns and drops duplicate items", {
        expect_warning(
            weight_spec <- tabBookWeightSpec(
                ds2[c("gender", "starttime")],
                list(wt1 = "gender", wt2 = "starttime", birthyr = "gender") # birthyr default too
            ),
            "Dropping duplicated alias & weight combinations"
        )
        expect_equivalent(
            weight_spec,
            data.frame(
                alias =  c("gender", "gender","starttime", "starttime"),
                weight = c("birthyr", "wt1", "birthyr", "wt2"),
                stringsAsFactors = FALSE
            )
        )
    })

    w <- list(weight1 = c("allpets", "q1"), weight2 = "q1")
    w_df <- tabBookWeightSpec(ds3, w)
    multitable <- multitables(ds3)[[1]]

    # Used for constructing the paths to the multitable returns
    ds3_id <- ds3@body$id
    mt_id <- multitable@body$id
    mt_paths <- c(
        paste0("https://app.crunch.io/api/datasets/", ds3_id, "/multitables/", mt_id, "/tabbook-unweighted/"), #nolint
        paste0("https://app.crunch.io/api/datasets/", ds3_id, "/multitables/", mt_id, "/tabbook-weight1/"), #nolint
        paste0("https://app.crunch.io/api/datasets/", ds3_id, "/multitables/", mt_id, "/tabbook-weight2/") #nolint
    )

    test_that("Can load a multiweight tabbook", {
        with_multi_POST(mt_paths, {
            r <- tabBook(multitable, ds3, weight = w_df)
        })

        # Right number of pages
        expect_equal(length(r$meta$analyses), nrow(w_df))
        expect_equal(length(r$sheets), nrow(w_df))

        # Each analysis has right weight
        expect_equal(
            unname(vapply(r$meta$analyses, function(x) x$weight %||% "", "")),
            w_df$weight
        )

        # Each analysis has right name
        expect_equal(
            unname(vapply(r$meta$analyses, function(x) x$name, "")),
            unname(vapply(w_df$alias, function(x) name(ds3[[x]]), ""))
        )


        # TODO: numeric variables don't have alias, but total instead.
        # See if this is bug with backend or intentional
        # ref: https://github.com/Crunch-io/rcrunch/issues/509
        aliases <- aliases(r)
        expect_equal(
           aliases[aliases != "total"],
           w_df$alias[aliases != "total"]
        )

        # Check that we can also specify a list
        with_multi_POST(mt_paths, {
            r_from_list <- tabBook(multitable, ds3, weight = w)
        })

        expect_equal(r, r_from_list)
    })

    test_that("Fails on empty list", {
        expect_error(
            tabBook(multitable, ds3, weight = list()),
            "Empty list not allowed as a weight spec, use NULL to indicate no weights"
        )
    })

    test_that("Fails when multi weight and excel", {
        expect_error(
            tabBook(multitable, ds3, weight = w, output_format = "xlsx"),
            "Complex weights only supported for json tabBooks"
        )
    })

    test_that("Fails when duplicated weights", {
        weights <- data.frame(
            weight = c("wt1", "wt1"),
            alias = c("x", "x"),
            stringsAsFactors = FALSE
        )
        expect_error(
            tabBook(multitable, ds3, weight = weights),
            "Found duplicate weight and alias combinations in weight_spec"
        )
    })

    test_that("Fails when wrong columns are given in weight data.frame argument", {
        weights <- data.frame(a = 1, stringsAsFactors = FALSE)
        expect_error(
            tabBook(multitable, ds3, weight = weights),
            "if weight_spec is a data.frame it must have exactly two columns: 'weight' & 'alias'"
        )

        weights <- data.frame(weight = "wt1", alias = "q1", order = 1, stringsAsFactors = FALSE)
        expect_error(
            tabBook(multitable, ds3, weight = weights),
            "if weight_spec is a data.frame it must have exactly two columns: 'weight' & 'alias'"
        )
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