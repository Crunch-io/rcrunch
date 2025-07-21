context("Multitables")

# Skip tests on windows (because they're slow and CRAN complains)
if (tolower(Sys.info()[["sysname"]]) != "windows") {
    test_that("default name for formula", {
        expect_identical(RHS_string(a + b ~ c + d + rollup(e)), "c + d + rollup(e)")
        expect_identical(RHS_string("a+b~c+d+rollup(e)"), "c+d+rollup(e)")
    })

    with_mock_crunch({
        ds <- cachedLoadDataset("test ds") ## Has 2 multitables
        ds2 <- cachedLoadDataset("ECON.sav") ## Has no multitables
        ds_veg <- cachedLoadDataset("Vegetables example") ## Has valid tabbook
        with_POST("https://app.crunch.io/api/datasets/1/filters/filter1/", {
            ## Mock the return of that creation
            f1 <- newFilter("A filter", ds$gender == "Male", catalog = filters(ds))
            expect_is(f1, "CrunchFilter")
        })
        with_POST("https://app.crunch.io/api/datasets/1/filters/filter2/", {
            ## Mock the return of that creation
            f2 <- newFilter("A filter", ds$gender == "Male", catalog = filters(ds))
            expect_is(f2, "CrunchFilter")
        })
        test_that("multitables() getter", {
            expect_is(multitables(ds), "MultitableCatalog")
            expect_is(multitables(ds2), "MultitableCatalog")
            expect_length(multitables(ds), 3)
            expect_length(multitables(ds2), 1)
        })

        test_that("Extract multitable", {
            expect_is(multitables(ds)[[2]], "Multitable")
            expect_is(multitables(ds)[["Shared multitable"]], "Multitable")
            expect_error(multitables(ds)[[99]], "subscript out of bounds: 99")
            expect_null(multitables(ds)[["NOTVALID"]])
        })

        mults <- multitables(ds)
        test_that("Multitable catalog names", {
            expect_identical(
                names(mults),
                c("My banner", "My team multitable", "Shared multitable")
            )
            ## Note that this PATCHes the entity, not the catalog
            expect_PATCH(
                names(mults)[3] <- "New name",
                "https://app.crunch.io/api/datasets/1/multitables/4de322/",
                '{"name":"New name"}'
            )
        })
        test_that("Multitable catalog is.public", {
            expect_identical(is.public(mults), c(FALSE, FALSE, TRUE))
            expect_identical(is.public(mults[[1]]), FALSE)
            expect_identical(is.public(mults[[2]]), FALSE)
            expect_identical(is.public(mults[[3]]), TRUE)
            ## Note that this PATCHes the entity, not the catalog
            expect_PATCH(
                is.public(mults)[3] <- FALSE,
                "https://app.crunch.io/api/datasets/1/multitables/4de322/",
                '{"is_public":false}'
            )
            with_PATCH(
                NULL,
                is.public(mults)[3] <- FALSE
            )
            expect_no_request(is.public(mults)[3] <- TRUE)
        })

        test_that("Multitable delete requires consent", {
            expect_error(
                delete(mults[["Shared multitable"]]),
                "Must confirm deleting multitable"
            )
            expect_error(
                mults[["Shared multitable"]] <- NULL,
                "Must confirm deleting multitable"
            )
        })

        with(consent(), {
            test_that("Multitable delete", {
                expect_DELETE(
                    delete(mults[["Shared multitable"]]),
                    "https://app.crunch.io/api/datasets/1/multitables/4de322/"
                )
                expect_DELETE(
                    multitables(ds)[["Shared multitable"]] <- NULL,
                    "https://app.crunch.io/api/datasets/1/multitables/4de322/"
                )
                expect_DELETE(
                    multitables(ds)[[3]] <- NULL,
                    "https://app.crunch.io/api/datasets/1/multitables/4de322/"
                )
                expect_silent(multitables(ds)[[999]] <- NULL)
                expect_error(multitables(ds)[[list(1)]] <- NULL, "invalid subscript type 'list'")
                expect_error(multitables(ds)[[2i]] <- NULL, "invalid subscript type 'complex'")
            })
        })

        test_that("Multitable object methods", {
            expect_identical(name(mults[[1]]), "My banner")
            expect_PATCH(
                name(mults[[1]]) <- "Another name",
                "https://app.crunch.io/api/datasets/1/multitables/ed30c4/",
                '{"name":"Another name"}'
            )
            expect_PATCH(
                is.public(mults[[1]]) <- TRUE,
                "https://app.crunch.io/api/datasets/1/multitables/ed30c4/",
                '{"is_public":true}'
            )
            expect_no_request(is.public(mults[[1]]) <- FALSE)
        })

        test_that("newMultitable", {
            expect_POST(
                newMultitable(~ gender + mymrset,
                              data = ds,
                              name = "New multitable",
                              is_public = TRUE
                ),
                "https://app.crunch.io/api/datasets/1/multitables/",
                '{"element":"shoji:entity","body":{',
                '"template":[{"query":[{"variable":"https://app.crunch.io/api/',
                'datasets/1/variables/gender/"}]},',
                '{"query":[{"function":"dimension","args":[{"function":"as_selected",',
                '"args":[{"variable":"https://app.crunch.io/api/datasets/1/variables/mymrset/"}]},',
                '{"value":"subvariables"}]},{"function":"as_selected","args":[{"variable":',
                '"https://app.crunch.io/api/datasets/1/variables/mymrset/"}]}]}]',
                ',"name":"New multitable","is_public":true}}'
            )
            with_POST("https://app.crunch.io/api/datasets/1/multitables/4de322/", {
                mtable <- newMultitable(~ gender + mymrset,
                                        data = ds,
                                        name = "New multitable"
                )
                expect_is(mtable, "Multitable")
            })
        })

        test_that("newMultitable provides a default name based on the formula", {
            expect_POST(
                newMultitable(~ gender + mymrset, data = ds),
                "https://app.crunch.io/api/datasets/1/multitables/",
                '{"element":"shoji:entity","body":{',
                '"template":[{"query":[{"variable":"https://app.crunch.io/api/datasets',
                '/1/variables/gender/"}]},', # nolint
                '{"query":[{"function":"dimension","args":[{"function":"as_selected",',
                '"args":[{"variable":"https://app.crunch.io/api/datasets/1/variables/mymrset/"}]},',
                '{"value":"subvariables"}]},{"function":"as_selected","args":[{"variable":',
                '"https://app.crunch.io/api/datasets/1/variables/mymrset/"}]}]}]',
                ',"name":"gender + mymrset"}}'
            )
            with_POST("https://app.crunch.io/api/datasets/1/multitables/4de322/", {
                mtable <- newMultitable(~ gender + mymrset,
                                        data = ds,
                                        name = "New multitable"
                )
                expect_is(mtable, "Multitable")
            })
        })

        test_that("multitable show method", {
            with_POST("https://app.crunch.io/api/datasets/1/multitables/4de322/", {
                mtable <- newMultitable(~ gender + mymrset,
                                        data = ds,
                                        name = "Shared multitable"
                )
                expect_is(mtable, "Multitable")
            })
            expect_prints(
                mtable,
                paste(paste0("Multitable ", dQuote("Shared multitable")),
                      "Column variables:",
                      "  gender",
                      "  mymrset",
                      sep = "\n"
                )
            )
        })

        test_that("multitable list methods", {
            expect_POST(
                multitables(ds)[["mt again"]] <- ~ gender + birthyr,
                "https://app.crunch.io/api/datasets/1/multitables/",
                '{"element":"shoji:entity","body":',
                '{"template":[{"query":[{"variable":',
                '"https://app.crunch.io/api/datasets/1/variables/gender/"}]},',
                '{"query":[{"variable":"https://app.crunch.io/api/datasets/1/variables/birthyr/"}]}]', # nolint
                ',"name":"mt again"}}'
            )
            expect_PATCH(
                multitables(ds)[["Shared multitable"]] <- ~ gender + birthyr,
                "https://app.crunch.io/api/datasets/1/multitables/4de322/",
                '{"element":"shoji:entity","body":',
                '{"template":[{"query":[{"variable":',
                '"https://app.crunch.io/api/datasets/1/variables/gender/"}]},',
                '{"query":[{"variable":"https://app.crunch.io/api/datasets/1/variables/birthyr/"}]}]', # nolint
                "}}"
            )
            expect_PATCH(
                multitables(ds)[[3]] <- ~ gender + birthyr,
                "https://app.crunch.io/api/datasets/1/multitables/4de322/",
                '{"element":"shoji:entity","body":',
                '{"template":[{"query":[{"variable":',
                '"https://app.crunch.io/api/datasets/1/variables/gender/"}]},',
                '{"query":[{"variable":"https://app.crunch.io/api/datasets/1/variables/birthyr/"}]}]', # nolint
                "}}"
            )
            expect_error(
                multitables(ds)[[999]] <- ~ gender + birthyr,
                "subscript out of bounds: 999"
            )
        })

        test_that("newMultitable validation", {
            expect_error(newMultitable(), "Must provide a formula")
            expect_error(
                newMultitable(.),
                paste(dQuote("data"), "must be a Dataset")
            )
            expect_error(
                newMultitable(., data = ""),
                paste(dQuote("data"), "must be a Dataset")
            )
        })

        test_that("can get and set the team for multitables", {
            team_mult <- multitables(ds)[["My team multitable"]]
            private_mult <- multitables(ds)[["My banner"]]
            expect_identical(team(team_mult), getTeams()[["Alpha Team"]])
            expect_no_request(team(team_mult) <- getTeams()[["Alpha Team"]])

            expect_PATCH(
                team(team_mult) <- NULL,
                "https://app.crunch.io/api/datasets/1/multitables/f33123/",
                '{"team":null}'
            )

            expect_null(team(private_mult))

            expect_PATCH(
                team(private_mult) <- getTeams()[["Alpha Team"]],
                "https://app.crunch.io/api/datasets/1/multitables/ed30c4/",
                '{"team":"https://app.crunch.io/api/teams/team1/"}'
            )

            # can also just use a url
            expect_PATCH(
                team(private_mult) <- "https://app.crunch.io/api/teams/team1/",
                "https://app.crunch.io/api/datasets/1/multitables/ed30c4/",
                '{"team":"https://app.crunch.io/api/teams/team1/"}'
            )
        })

        test_that("cache priming (so that requests don't cloud tests below)", {
            expect_null(weight(ds))
        })
        test_that("tabBook sets the right request header", {
            expect_header(
                expect_POST(
                    tabBook(mults[[1]], data = ds, output_format = "xlsx"),
                    "https://app.crunch.io/api/datasets/1/multitables/ed30c4/export/"
                ),
                "Accept: application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
            )
            expect_header(
                expect_POST(
                    tabBook(mults[[1]], data = ds, output_format = "json"),
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

        test_that("tabBook filter argument with list of expressions", {
            expect_equal(
                as.character(toJSON(standardize_tabbook_filter(
                    ds, list(ds$gender == "Male", ds$gender == "Female"))
                )),
                paste0(
                    '[{\"function\":\"==\",\"args\":[{\"variable\":',
                    '\"https://app.crunch.io/api/datasets/1/variables/gender/\"},{\"value\":1}]},',
                    '{\"function\":\"==\",\"args\":[{\"variable\":',
                    '\"https://app.crunch.io/api/datasets/1/variables/gender/\"},{\"value\":2}]}]'
                )
            )
        })

        test_that("tabBook filter argument with mixed list", {
            expect_equal(
                as.character(toJSON(standardize_tabbook_filter(
                    ds, list(ds$gender == "Male", filters(ds)[1]))
                )),
                paste0(
                    '[{\"function\":\"==\",\"args\":[{\"variable\":',
                    '\"https://app.crunch.io/api/datasets/1/variables/gender/\"},{\"value\":1}]},',
                    '{\"filter\":\"https://app.crunch.io/api/datasets/1/filters/filter1/\"}]'
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
                    '\"options\":[],"where":{"function":"frame_subset","args":[{"frame":',
                    '"primary"},{"value":["66ae9881e3524f7db84970d556c34552",',
                    '"d7c21314ca9e453c93069168681a285c"]},{"value":null}]}}'
                ),
                "Accept: application/json"
            )
        })

        test_that("tabBook with options", {
            expect_POST(
                tabBook(mults[[1]],
                        data = ds, output_format = "json", format = list(pval_colors = TRUE))
                ,
                "https://app.crunch.io/api/datasets/1/multitables/ed30c4/export/",
                '{\"filter\":null,\"weight\":null,\"options\":{"format":{"pval_colors":true}}}'
            )
        })

        test_that("tabBook warning when using format argument", {
            expect_warning(
                expect_POST(
                    tabBook(mults[[1]],
                            data = ds, format = "json")
                    ,
                    "https://app.crunch.io/api/datasets/1/multitables/ed30c4/export/",
                    '{\"filter\":null,\"weight\":null,\"options\":{}}'
                ),
                "Use `output_format`"
            )
        })

        test_that("tabBook warning when using legacy endpoint", {
            with(temp.option(crunch = list(use.legacy.tabbook.endpoint = TRUE)), {
                expect_warning(
                    expect_POST(
                        tabBook(mults[[1]],
                                data = ds, output_format = "json")
                        ,
                        "https://app.crunch.io/api/datasets/1/multitables/ed30c4/tabbook/",
                        '{\"filter\":null,\"weight\":null,\"options\":[]}'
                    ),
                    "The legacy tabbook endpoint has been deprecated and will be removed in the future." # nolint
                )
            })
        })

        with_POST(
            "https://app.crunch.io/api/datasets/veg/multitables/mt_01/cat-mr-tabbook/", {
                book <- tabBook(mults[[1]], data = ds, output_format = "json")
                test_that("tabBook JSON returns TabBookResult", {
                    expect_is(book, "TabBookResult")
                })
                test_that("TabBookResult and MultitableResult size/extract methods", {
                    expect_length(book, 5)
                    expect_is(book[[1]], "MultitableResult")
                    expect_length(book[[1]], 3)
                    expect_identical(dim(book), c(5L, 3L))
                    expect_is(book[[1]][[1]], "CrunchCube")
                })

                test_that("MultitableResult print methods - mr+cat template x cat page", {
                    ## Print method for MultitableResult binds together the Cubes
                    out <- cubify(
                        85,  42, 56, 80, 85,   0,
                        120, 41, 68, 88,  0, 120,
                        dims = list(
                            c("No", "Yes"),
                            c("", "Savory", "Spicy", "Sweet", "No", "Yes")
                        )
                    )
                    expect_prints(print(book[[2]]), get_output(out))
                })

                test_that("MultitableResult print methods - mr+cat template x catarray page", {
                    ## Print method for MultitableResult binds together the Cubes
                    ## And rearranges 3d cubes from Cat array
                    out <- structure(
                        c(44, 28, 13, 90, 18, 19, 12, 6, 33, 6, 32, 17, 6,
                          57, 7, 42, 21, 10, 73, 14, 26, 11, 4, 33, 2, 16, 16, 9, 55, 16,
                          73, 21, 21, 55, 33, 30, 6, 10, 24, 13, 48, 13, 15, 32, 16, 57,
                          19, 20, 44, 25, 38, 9, 10, 18, 8, 34, 11, 10, 36, 25, 19, 0,
                          122, 16, 45, 7, 0, 49, 7, 19, 12, 0, 67, 10, 34, 17, 0, 103,
                          14, 30, 15, 0, 58, 8, 4, 4, 0, 62, 8, 38, 20, 17, 45, 101, 15,
                          8, 4, 18, 43, 7, 14, 14, 20, 67, 7, 16, 16, 36, 83, 10, 10, 9,
                          16, 41, 5, 10, 8, 28, 56, 10),
                        .Dim = c(5L, 6L, 4L),
                        .Dimnames = list(
                            c("Strongly Disagree", "Disagree", "Neither", "Agree", "Strongly Agree"), # nolint
                            c("", "Savory", "Spicy", "Sweet", "No", "Yes"),
                            c("Healthy", "Tasty", "Filling", "Environmental")
                        )
                    )
                    expect_prints(print(book[[4]]), get_output(out))
                })

                test_that("MultitableResult print methods - mr+cat template x mr page", {
                    ## Print method for MultitableResult binds together the Cubes
                    out <- cubify(
                        86,  86,  52,  65, 42, 41,
                        128, 52, 128, 110, 56, 68,
                        171, 65, 110, 171, 80, 88,
                        dims = list(
                            c("Savory", "Spicy", "Sweet"),
                            c("", "Savory", "Spicy", "Sweet", "No", "Yes")
                        )
                    )
                    expect_prints(print(book[[3]]), get_output(out))
                })

                test_that("MultitableResult print methods - mr+cat template x numeric page", {
                    ## Print method for MultitableResult binds together the Cubes
                    out <- cubify(
                        41.83920, 37.21250, 44.41667, 42.09375, 41.64103, 42.18803,
                        dims = list(
                            c("", "Savory", "Spicy", "Sweet", "No", "Yes")
                        )
                    )
                    expect_prints(print(book[[1]]), get_output(out))
                })

                test_that("MultitableResult print methods - mr+cat template x numeric array page", {
                    ## Print method for MultitableResult binds together the Cubes
                    out <- structure(
                        c(72.312195122, 62.3970588235, 75.8719211823, 72.54,
                          68.0251256281, 86.6734693878, 72.8823529412, 63.9642857143, 75.4634146341,
                          73.313253012, 70.0253164557, 87.2133333333, 72.192, 61.918699187,
                          76.0483870968, 74.9024390244, 67.5409836066, 87.1610169492, 72.3076923077,
                          62.3090909091, 76.2771084337, 71.8086419753, 67.8695652174, 86.8679245283,
                          71.6235294118, 62.6666666667, 78.6172839506, 69.3780487805, 67.4268292683,
                          86.95, 72.7913043478, 62.1271186441, 74.2905982906, 74.4513274336,
                          68.5178571429, 86.4910714286),
                        .Dim = c(6L, 6L),
                        .Dimnames = list(
                            c("Avocado", "Brussel Sprout", "Carrot", "Daikon", "Eggplant", "Fennel"), # nolint
                            c("", "Savory", "Spicy", "Sweet", "No", "Yes")
                        )
                    )
                    expect_prints(print(book[[5]]), get_output(out))
                })

                test_that("The first result in a MultitableResult has 2 dimensions", {
                    expect_identical(dim(book[["Healthy Eater"]][[1]]), c(2L, 1L))
                })
                test_that("prop.table methods", {
                    ## prop.table on a TabBookResult returns a list of lists of prop.tables
                    full_prop_table <- prop.table(book)

                    expect_identical(
                        full_prop_table[[2]][[2]],
                        prop.table(book[[2]][[2]])
                    )

                    ## And non-count measures get NULL
                    expect_identical(
                        full_prop_table[["Age"]],
                        NULL
                    )
                })

                test_that("tab book names", {
                    expect_identical(
                        names(book)[1:2],
                        c("Age", "Healthy Eater")
                    )
                    expect_is(book[["Healthy Eater"]], "MultitableResult")
                    expect_null(book[["NOTVALID"]])
                })

            }
        )
        ## TODO: something more with variable metadata? For cubes more generally?
        ## --> are descriptions coming from backend if they exist?

    })

    with_test_authentication({
        ds <- newDatasetFromFixture("apidocs")
        ds2 <- newDatasetFromFixture("apidocs")
        test_that("Multitable catalog", {
            expect_is(multitables(ds), "MultitableCatalog")
            expect_length(multitables(ds), 0)
        })

        test_that("Can make a multitable", {
            m <- newMultitable(~ allpets + q1, data = ds)
            expect_identical(name(m), "allpets + q1")
            expect_identical(getShowContent(m), c(
                paste0("Multitable ", dQuote("allpets + q1")),
                "Column variables:",
                "  allpets",
                "  q1"
            ))
        })

        test_that("Can make a multitable perserving zcl functions", {
            m <- newMultitable(~ rollup(wave, "M") + q1, data = ds)
            expect_identical(name(m), "rollup(wave, \"M\") + q1")
            expect_identical(getShowContent(m), c(
                paste0("Multitable ", dQuote("rollup(wave, \"M\") + q1")),
                "Column variables:",
                "  rollup(wave, \"M\")",
                "  q1"
            ))
            # cleanup
            with_consent(delete(m))
        })

        test_that("Can make a multitable with a cat array", {
            m <- newMultitable(~ petloc + q1, data = ds)
            expect_identical(name(m), "petloc + q1")
            expect_identical(getShowContent(m), c(
                paste0("Multitable ", dQuote("petloc + q1")),
                "Column variables:",
                "  petloc",
                "  q1"
            ))
            # cleanup
            with_consent(delete(m))
        })

        test_that("Can make a multitable with list methods", {
            multitables(ds)[["new mt"]] <- ~ country + q3
            expect_identical(
                getShowContent(multitables(ds)[["new mt"]]),
                c(
                    paste0("Multitable ", dQuote("new mt")),
                    "Column variables:",
                    "  country",
                    "  q3"
                )
            )
            multitables(ds)[["new mt"]] <- ~ country + q1
            expect_identical(
                getShowContent(multitables(ds)[["new mt"]]),
                c(
                    paste0("Multitable ", dQuote("new mt")),
                    "Column variables:",
                    "  country",
                    "  q1"
                )
            )
            with_consent(multitables(ds)[["new mt"]] <- NULL)
            expect_false("new mt" %in% names(multitables(refresh(ds))))
        })

        mult <- multitables(ds)[["allpets + q1"]]

        test_that("Can make the multitable entity public/personal", {
            expect_false(is.public(mult))
            is.public(mult) <- TRUE
            expect_true(is.public(refresh(mult)))
            is.public(mult) <- FALSE
            expect_false(is.public(refresh(mult)))
        })

        test_that("Can make the multitable public/personal on the catalog", {
            expect_false(is.public(multitables(ds))[1])
            is.public(multitables(ds))[1] <- TRUE
            expect_true(is.public(refresh(multitables(ds)))[1])
            is.public(multitables(ds))[1] <- FALSE
            expect_false(is.public(refresh(multitables(ds)))[1])
        })

        test_that("Can edit the multitable name", {
            expect_identical(name(mult), "allpets + q1")
            name(mult) <- "A new name"
            expect_identical(name(mult), "A new name")
            expect_identical(name(refresh(mult)), "A new name")
            expect_identical(names(refresh(multitables(ds))), "A new name")
            names(multitables(ds)) <- "Yet another name"
            expect_identical(names(multitables(ds)), "Yet another name")
            expect_identical(names(refresh(multitables(ds))), "Yet another name")
        })


        multitables(ds2)[["new mt"]] <- ~ country + q1
        multitables(ds2)[["Yet another name"]] <- ~ country + q3
        mults <- multitables(ds2)
        with(consent(), {
            test_that("Multitable delete", {
                delete(mults[["Yet another name"]])
                expect_equal(length(multitables(refresh(ds2))), 1)
                expect_true(!"Yet another name" %in% names(multitables(ds2)))
                expect_equal(names(multitables(ds2)), "new mt")
            })
        })

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

        test_that("team-sharing of multitables", {
            multitables(ds)[["team multitable"]] <- ~ allpets + q1
            team_multitab <- multitables(ds)[["team multitable"]]
            expect_null(team(team_multitab))

            # set teams to use
            teams <- getTeams()
            if (!"A new team for filters" %in% names(teams)) {
                teams[["A new team for filters"]] <- list()
            }
            if (!"A different team for filters" %in% names(teams)) {
                teams[["A different team for filters"]] <- list()
            }

            # can set a team
            team(team_multitab) <- getTeams()[["A new team for filters"]]
            expect_identical(
                team(team_multitab),
                getTeams()[["A new team for filters"]]
            )

            # can change a team (with a URL this time)
            team_url <- self(getTeams()[["A different team for filters"]])
            team(team_multitab) <- team_url
            expect_identical(
                team(team_multitab),
                getTeams()[["A different team for filters"]]
            )

            # can remove the team
            team(team_multitab) <- NULL
            expect_null(team(team_multitab))
        })
    })
}
