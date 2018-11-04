context("Multitables")

test_that("default name for formula", {
    expect_identical(RHS_string(a + b ~ c + d + rollup(e)), "c + d + rollup(e)")
    expect_identical(RHS_string("a+b~c+d+rollup(e)"), "c+d+rollup(e)")
})

with_mock_crunch({
    ds <- loadDataset("test ds") ## Has 2 multitables
    ds2 <- loadDataset("ECON.sav") ## Has no multitables

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

    m <- mults[[1]]
    test_that("Multitable object methods", {
        expect_identical(name(m), "My banner")
        expect_PATCH(
            name(m) <- "Another name",
            "https://app.crunch.io/api/datasets/1/multitables/ed30c4/",
            '{"name":"Another name"}'
        )
        expect_PATCH(
            is.public(m) <- TRUE,
            "https://app.crunch.io/api/datasets/1/multitables/ed30c4/",
            '{"is_public":true}'
        )
        expect_no_request(is.public(m) <- FALSE)
    })

    test_that("newMultitable", {
        expect_POST(
            newMultitable(~gender + mymrset,
                data = ds,
                name = "New multitable"
            ),
            "https://app.crunch.io/api/datasets/1/multitables/",
            '{"element":"shoji:entity","body":{',
            '"template":[{"query":[{"variable":"https://app.crunch.io/api/datasets/1/variables/gender/"}]},',
            '{"query":[{"each":"https://app.crunch.io/api/datasets/1/variables/mymrset/"},',
            '{"function":"as_selected","args":[{"variable":"https://app.crunch.io/api/datasets/1/variables/mymrset/"}]}',
            "]}]",
            ',"name":"New multitable"}}'
        )
        with_POST("https://app.crunch.io/api/datasets/1/multitables/4de322/", {
            mtable <- newMultitable(~gender + mymrset,
                data = ds,
                name = "New multitable"
            )
            expect_is(mtable, "Multitable")
        })
    })

    test_that("newMultitable provides a default name based on the formula", {
        expect_POST(
            newMultitable(~gender + mymrset, data = ds),
            "https://app.crunch.io/api/datasets/1/multitables/",
            '{"element":"shoji:entity","body":{',
            '"template":[{"query":[{"variable":"https://app.crunch.io/api/datasets/1/variables/gender/"}]},',
            '{"query":[{"each":"https://app.crunch.io/api/datasets/1/variables/mymrset/"},',
            '{"function":"as_selected","args":[{"variable":"https://app.crunch.io/api/datasets/1/variables/mymrset/"}]}',
            "]}]",
            ',"name":"gender + mymrset"}}'
        )
        with_POST("https://app.crunch.io/api/datasets/1/multitables/4de322/", {
            mtable <- newMultitable(~gender + mymrset,
                data = ds,
                name = "New multitable"
            )
            expect_is(mtable, "Multitable")
        })
    })

    test_that("importMultitable", {
        with_POST("https://app.crunch.io/api/datasets/1/multitables/4de322/", {
            mtable <- newMultitable(~gender + mymrset,
                data = ds,
                name = "New multitable"
            )
            expect_is(mtable, "Multitable")
        })
        expect_POST(
            importMultitable(ds, mtable, name = "copied_multitable"),
            "https://app.crunch.io/api/datasets/1/multitables/",
            '{"element":"shoji:entity","body":{',
            '"multitable":"https://app.crunch.io/api/datasets/1/multitables/4de322/",',
            '"name":"copied_multitable"}}'
        )
    })

    test_that("multitable show method", {
        with_POST("https://app.crunch.io/api/datasets/1/multitables/4de322/", {
            mtable <- newMultitable(~gender + mymrset,
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
            multitables(ds)[["mt again"]] <- ~gender + birthyr,
            "https://app.crunch.io/api/datasets/1/multitables/",
            '{"element":"shoji:entity","body":',
            '{"template":[{"query":[{"variable":"https://app.crunch.io/api/datasets/1/variables/gender/"}]},',
            '{"query":[{"variable":"https://app.crunch.io/api/datasets/1/variables/birthyr/"}]}]',
            ',"name":"mt again"}}'
        )
        expect_PATCH(
            multitables(ds)[["Shared multitable"]] <- ~gender + birthyr,
            "https://app.crunch.io/api/datasets/1/multitables/4de322/",
            '{"element":"shoji:entity","body":',
            '{"template":[{"query":[{"variable":"https://app.crunch.io/api/datasets/1/variables/gender/"}]},',
            '{"query":[{"variable":"https://app.crunch.io/api/datasets/1/variables/birthyr/"}]}]',
            "}}"
        )
        expect_PATCH(
            multitables(ds)[[3]] <- ~gender + birthyr,
            "https://app.crunch.io/api/datasets/1/multitables/4de322/",
            '{"element":"shoji:entity","body":',
            '{"template":[{"query":[{"variable":"https://app.crunch.io/api/datasets/1/variables/gender/"}]},',
            '{"query":[{"variable":"https://app.crunch.io/api/datasets/1/variables/birthyr/"}]}]',
            "}}"
        )
        expect_error(multitables(ds)[[999]] <- ~gender + birthyr, "subscript out of bounds: 999")
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
                tabBook(m, data = ds, format = "xlsx"),
                "https://app.crunch.io/api/datasets/1/multitables/ed30c4/tabbook/"
            ),
            "Accept: application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
        )
        expect_header(
            expect_POST(
                tabBook(m, data = ds, format = "json"),
                "https://app.crunch.io/api/datasets/1/multitables/ed30c4/tabbook/"
            ),
            "Accept: application/json"
        )
    })

    test_that("tabBook can return a subset of variables", {
        expect_equivalent(weight(ds2), ds[["birthyr"]])
        m <- multitables(ds2)[[1]]
        expect_header(
            expect_POST(
                tabBook(m, data = ds2[c("gender", "starttime")]),
                "https://app.crunch.io/api/datasets/3/multitables/ed30c4/tabbook/"
            ),
            "Accept: application/json"
        )
    })
    
    ## TODO: test the query shape

    with_POST("https://app.crunch.io/api/datasets/1/multitables/apidocs-tabbook/", {
        book <- tabBook(m, data = ds, format = "json")
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
            book <- tabBook(m, data = ds, format = "json")
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
            book <- tabBook(m, data = ds, format = "json")
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
        book <- tabBook(m, data = ds)
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
    ds2 <- newDatasetFromFixture("apidocs")
    test_that("Multitable catalog", {
        expect_is(multitables(ds), "MultitableCatalog")
        expect_length(multitables(ds), 0)
    })

    test_that("Can make a multitable", {
        m <- newMultitable(~allpets + q1, data = ds)
        expect_identical(name(m), "allpets + q1")
        expect_identical(getShowContent(m), c(
            paste0("Multitable ", dQuote("allpets + q1")),
            "Column variables:",
            "  allpets",
            "  q1"
        ))
    })

    test_that("Can make a multitable perserving zcl functions", {
        m <- newMultitable(~rollup(wave, "M") + q1, data = ds)
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
        m <- newMultitable(~petloc + q1, data = ds)
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
        multitables(ds)[["new mt"]] <- ~country + q3
        expect_identical(
            getShowContent(multitables(ds)[["new mt"]]),
            c(
                paste0("Multitable ", dQuote("new mt")),
                "Column variables:",
                "  country",
                "  q3"
            )
        )
        multitables(ds)[["new mt"]] <- ~country + q1
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

    test_that("Can copy the multitable to a new multitable", {
        m <- importMultitable(ds2, mult, name = "copied_multitable")
        expect_identical(name(m), "copied_multitable")
        is.public(multitables(ds2))[1] <- TRUE
        expect_true(is.public(refresh(m)))
        expect_identical(getShowContent(m), c(
            paste0("Multitable ", dQuote("copied_multitable")),
            "Column variables:",
            "  allpets",
            "  q1"
        ))
    })

    test_that("importMultitable works without a name", {
        m <- importMultitable(ds2, mult)
        expect_identical(name(m), "Yet another name")
    })

    mults <- multitables(ds2)
    with(consent(), {
        test_that("Multitable delete", {
            delete(mults[["Yet another name"]])
            expect_equal(length(multitables(refresh(ds2))), 1)
            expect_true(!"Yet another name" %in% names(multitables(ds2)))
            expect_equal(names(multitables(ds2)), "copied_multitable")
        })
    })

    test_that("We can get an xlsx tab book", {
        skip_on_local_backend("Vagrant host doesn't serve files correctly")
        f <- tempfile()
        out <- tabBook(mult, data = ds, format = "xlsx", file = f)
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
        multitables(ds)[["team multitable"]] <- ~allpets + q1
        team_multitab <- multitables(ds)[["team multitable"]]
        expect_null(team(team_multitab))
        
        # can set a team
        team(team_multitab) <- getTeams()[["New team"]]
        expect_identical(team(team_multitab), getTeams()[["New team"]])
        
        # can change a team (with a URL this time)
        team(team_multitab) <- self(getTeams()[["a really new one"]])
        expect_identical(team(team_multitab), getTeams()[["a really new one"]])
        
        # can remove the team
        team(team_multitab) <- NULL
        expect_null(team(team_multitab))
    })
})
