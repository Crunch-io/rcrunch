context("Multitables")

test_that("default name for formula", {
    expect_identical(RHS_string(a + b ~ c + d + rollup(e)), "c + d + rollup(e)")
    expect_identical(RHS_string("a+b~c+d+rollup(e)"), "c+d+rollup(e)")
})

with_mock_crunch({
    ds <- loadDataset("test ds") ## Has 2 multitables
    ds2 <- loadDataset("ECON.sav") ## Has no multitables
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
            '{"query":[{"each":"https://app.crunch.io/api/datasets/1/variables/mymrset/"},',
            '{"function":"as_selected","args":[{"variable":"https://app.crunch.io/',
            'api/datasets/1/variables/mymrset/"}]}',
            "]}]",
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
            '{"query":[{"each":"https://app.crunch.io/api/datasets/1/variables/mymrset/"},',
            '{"function":"as_selected","args":[{"variable":"https://app.crunch.io',
            '/api/datasets/1/variables/mymrset/"}]}', # nolint
            "]}]",
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
            '{"query":[{"variable":"https://app.crunch.io/api/datasets/1/variables/birthyr/"}]}]',
            ',"name":"mt again"}}'
        )
        expect_PATCH(
            multitables(ds)[["Shared multitable"]] <- ~ gender + birthyr,
            "https://app.crunch.io/api/datasets/1/multitables/4de322/",
            '{"element":"shoji:entity","body":',
            '{"template":[{"query":[{"variable":',
            '"https://app.crunch.io/api/datasets/1/variables/gender/"}]},',
            '{"query":[{"variable":"https://app.crunch.io/api/datasets/1/variables/birthyr/"}]}]',
            "}}"
        )
        expect_PATCH(
            multitables(ds)[[3]] <- ~ gender + birthyr,
            "https://app.crunch.io/api/datasets/1/multitables/4de322/",
            '{"element":"shoji:entity","body":',
            '{"template":[{"query":[{"variable":',
            '"https://app.crunch.io/api/datasets/1/variables/gender/"}]},',
            '{"query":[{"variable":"https://app.crunch.io/api/datasets/1/variables/birthyr/"}]}]',
            "}}"
        )
        expect_error(multitables(ds)[[999]] <- ~ gender + birthyr, "subscript out of bounds: 999")
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

    test_that("team-sharing of multitables", {
        multitables(ds)[["team multitable"]] <- ~ allpets + q1
        team_multitab <- multitables(ds)[["team multitable"]]
        expect_null(team(team_multitab))

        # set teams to use
        teams <- getTeams()
        teams[["A new team for filters"]] <- list()
        teams[["A different team for filters"]] <- list()

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
