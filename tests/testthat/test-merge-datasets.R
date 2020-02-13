context("Merge/extend dataset")

with_mock_crunch({
    ds1 <- loadDataset("test ds")
    ds2 <- loadDataset("ECON.sav")

    testPayloadNoFilterArg <- paste0(
        '{"function":"adapt",',
        '"args":[{"dataset":"https://app.crunch.io/api/datasets/3/"},',
        '{"variable":"https://app.crunch.io/api/datasets/3/variables/birthyr/"},',
        '{"variable":"https://app.crunch.io/api/datasets/1/variables/birthyr/"}]'
    )
    testPayload <- paste0(testPayloadNoFilterArg, "}")
    genderFilter <- paste0(
        '{"function":"==","args":[',
        '{"variable":"https://app.crunch.io/api/datasets/3/variables/gender/"},{"value":1}]}'
    )
    testPayloadWithFilter <- paste0(
        testPayloadNoFilterArg, ',"filter":',
        genderFilter, "}"
    )
    testSubsetPayloadPart1 <- paste0(
        '{"function":"select","args":[{"map":{',
        '"66ae9881e3524f7db84970d556c34552":',
        '{"variable":"https://app.crunch.io/api/datasets/3/variables/gender/"},',
        '"f78ca47313144b57adfb495893968e70":',
        '{"variable":"https://app.crunch.io/api/datasets/3/variables/birthyr/"}}}],',
        '"frame":'
    )
    testSubsetPayload <- paste0(testSubsetPayloadPart1, testPayload, "}")
    testSubsetPayloadWithFilter <- paste0(
        testSubsetPayloadPart1, testPayload,
        ',"filter":', genderFilter, "}"
    )

    test_that("Correct payload without filtering", {
        expect_warning(
            expect_POST(
                merge(ds1, ds2, by.x = ds1$birthyr, ds2$birthyr),
                "https://app.crunch.io/api/datasets/1/variables/",
                testPayload
            ),
            "Variable birthyr is hidden"
        )
    })

    test_that("Can reference variables by alias", {
        expect_warning(
            expect_POST(
                merge(ds1, ds2, by.x = "birthyr", by.y = "birthyr"),
                "https://app.crunch.io/api/datasets/1/variables/",
                testPayload
            ),
            "Variable birthyr is hidden"
        )
        expect_warning(
            expect_POST(
                merge(ds1, ds2, by = "birthyr"),
                "https://app.crunch.io/api/datasets/1/variables/",
                testPayload
            ),
            "Variable birthyr is hidden"
        )
    })
    test_that("joinDatasets with default copy=TRUE redirects here", {
        expect_warning(
            expect_POST(
                joinDatasets(ds1, ds2, by.x = ds1$birthyr, ds2$birthyr),
                "https://app.crunch.io/api/datasets/1/variables/",
                testPayload
            ),
            "Variable birthyr is hidden"
        )
    })
    test_that("joinDatasets with copy=FALSE is (no longer/not yet) valid", {
        expect_error(
            joinDatasets(ds1, ds2, by.x = ds1$birthyr, ds2$birthyr, copy = FALSE),
            "Virtual joins are not yet supported."
        )
    })

    test_that("merge a subset of variables", {
        expect_warning(
            expect_POST(
                merge(ds1, ds2[c("gender", "birthyr")], by = "birthyr"),
                "https://app.crunch.io/api/datasets/1/variables/",
                testSubsetPayload
            ),
            "Variable birthyr is hidden"
        )
    })

    test_that("filter rows in merge", {
        expect_warning(
            expect_POST(
                merge(ds1, ds2[ds2$gender == "Male", ], by = "birthyr"),
                "https://app.crunch.io/api/datasets/1/variables/",
                testPayloadWithFilter
            ),
            "Variable birthyr is hidden"
        )
    })

    test_that("filter rows and variables in merge", {
        expect_warning(
            expect_POST(
                merge(ds1, ds2[ds2$gender == "Male", c("gender", "birthyr")],
                    by = "birthyr"
                ),
                "https://app.crunch.io/api/datasets/1/variables/",
                testSubsetPayloadWithFilter
            ),
            "Variable birthyr is hidden"
        )
    })

    test_that("Input validation for merge/extend (plus method dispatch)", {
        expect_error(
            extendDataset(1),
            "x must be a Crunch Dataset"
        )
        expect_error(
            merge(ds1, 1, by.x = ds1[[1]]),
            "y must be a Crunch Dataset"
        )
        expect_error(
            merge(ds1, ds2, by.x = 1),
            "by.x must be a Crunch Variable"
        )
        expect_error(
            merge(ds1, ds2, by.x = ds2[[1]]),
            "by.x must be a variable in x"
        )
        expect_error(
            merge(ds1, ds2, by.x = ds1[[1]], by.y = 1),
            "by.y must be a Crunch Variable"
        )
        expect_error(
            merge(ds1, ds2, by.x = ds1[[1]], by.y = ds1[[1]]),
            "by.y must be a variable in y"
        )
        expect_warning(
            expect_error(
                merge(ds1, ds2, by.x = ds1$birthyr, by.y = ds2$birthyr, all = TRUE),
                'Option "all" not supported.'
            ),
            "Variable birthyr is hidden"
        ) ## In ds2
        expect_warning(
            expect_error(
                merge(ds1, ds2, by.x = ds1$birthyr, by.y = ds2$birthyr, all.x = FALSE),
                'Option "all.x=FALSE" not supported.'
            ),
            "Variable birthyr is hidden"
        ) ## In ds2
        expect_warning(
            expect_error(
                merge(ds1, ds2, by.x = ds1$birthyr, by.y = ds2$birthyr, all.y = TRUE),
                'Option "all.y" not supported.'
            ),
            "Variable birthyr is hidden"
        ) ## In ds2
    })

    test_that("Categorical and array variables can't be used as keys", {
        expect_error(
            merge(ds1, ds2, by.x = ds1$gender, by.y = ds2$birthyr),
            "by.x must be type numeric or text"
        )
        expect_error(
            merge(ds1, ds2, by.x = ds1$birthyr, by.y = ds2$gender),
            "by.y must be type numeric or text"
        )
    })

    test_that("Providing != 1 alias gives useful error message", {
        expect_error(
            merge(ds1, ds2),
            "by.x must reference one and only one variable"
        )
        ## Default "by" is intersection of names
        expect_error(
            merge(ds1, ds2, by.x = ds1$birthyr),
            "by.y must reference one and only one variable"
        )
    })

    test_that("An invalid alias gives a useful error message", {
        expect_error(
            merge(ds1, ds2, by.x = "NOTAVARIABLE"),
            "NOTAVARIABLE does not reference a variable in x"
        )
        expect_error(
            merge(ds1, ds2, by.x = ds1$birthyr, by.y = "NOTAVARIABLE"),
            "NOTAVARIABLE does not reference a variable in y"
        )
    })
})

printed_order_apidocs2 <- c(
    "[+] Key Pet Indicators",
    "    All pets owned",
    "    Pet",
    "    Pets by location",
    "[+] Dog Metrics",
    "    Number of dogs",
    "    [+] Number of dogs by type",
    "        Number of dogs -- With papers",
    "        Number of dogs -- Mutts",
    "[+] Details",
    "    Pet name",
    "[+] Dimensions",
    "    Country",
    "    Wave",
    "Person ID",
    "[+] __hidden__",
    "    Weight",
    "    Case ID"
)

printed_order_apidocs2_merge_by_stringid <- c( # nolint
    "    [+] Key Pet Indicators",
    "        All pets owned",
    "        Pet",
    "        Pets by location",
    "    [+] Dog Metrics",
    "        Number of dogs",
    "        [+] Number of dogs by type",
    "            Number of dogs -- With papers",
    "            Number of dogs -- Mutts",
    "    [+] Details",
    "        Pet name",
    "    [+] Dimensions",
    "        Country",
    "        Wave",
    "[+] __hidden__",
    "    [+] Example dataset",
    "        Weight",
    "        Case ID"
)

printed_order_apidocs2_merge_by_caseid <- c( # nolint
    "    [+] Key Pet Indicators",
    "        All pets owned",
    "        Pet",
    "        Pets by location",
    "    [+] Dog Metrics",
    "        Number of dogs",
    "        [+] Number of dogs by type",
    "            Number of dogs -- With papers",
    "            Number of dogs -- Mutts",
    "    [+] Details",
    "        Pet name",
    "    [+] Dimensions",
    "        Country",
    "        Wave",
    "    Person ID",
    "[+] __hidden__",
    "    [+] Example dataset",
    "        Weight"
)

with_test_authentication({
    ds1 <- newDatasetFromFixture("join-apidocs2-to-me")
    with_consent(ds1$allpets_1 <- NULL)
    ds2 <- newDatasetFromFixture("apidocs2")
    test_that("Shape of apidocs2", {
        expect_prints(ordering(ds2),
            paste(printed_order_apidocs2, collapse = "\n"),
            fixed = TRUE
        )
        expect_identical(names(ds2), c(
            "allpets", "q1", "petloc", "ndogs",
            "ndogs_a", "ndogs_b", "q3", "country", "wave", "stringid"
        ))
        expect_identical(dim(ds2), c(20L, 10L))
    })
    test_that("Shape of join-to-me", {
        expect_identical(names(ds1), c("id", "matches", "other_var"))
        expect_identical(dim(ds1), c(14L, 3L))
    })

    test_that("An uncomplicated merge on a text key", {
        ds1 <- merge(ds1, ds2, by.x = ds1$id, by.y = ds2$stringid)
        expect_is(ds1, "CrunchDataset")
        expect_identical(dim(ds1), c(14L, 12L))
        expect_prints(ordering(ds1),
            paste(c(
                "ID",
                "Join matches",
                "Another variable",
                paste0("[+] ", name(ds2)),
                paste0(printed_order_apidocs2_merge_by_stringid) # nolint
            ),
            collapse = "\n"
            ),
            fixed = TRUE
        )
        expect_identical(
            names(ds1),
            c(
                "id", "matches", "other_var", "allpets", "q1", "petloc", "ndogs",
                "ndogs_a", "ndogs_b", "q3", "country", "wave"
            )
        )
        expect_identical(hiddenVariables(ds1, "name"), c("Case ID", "Weight"))
    })

    test_that("Similarly uncomplicated merge, but numeric and hidden key", {
        # When merging datasets during jenkins run tests, the order of the
        # variables is *sometimes* not copied (usually Pet Name/q3 is missing),
        # which makes this test fail. The ordering should be fully copied
        # during the merge operation. Reproduction of the bug on production or
        # alpha did not yield errors with order. Repro. locally is possible
        ds1 <- newDatasetFromFixture("join-apidocs2-to-me")
        with_consent(ds1$allpets_1 <- NULL)
        type(ds1$id) <- "numeric"

        expect_warning(
            ds1 <- merge(ds1, ds2, by.x = ds1$id, by.y = ds2$caseid),
            "Variable caseid is hidden"
        )
        expect_is(ds1, "CrunchDataset")
        expect_identical(dim(ds1), c(14L, 13L))
        expect_prints(ordering(ds1),
            paste(c(
                "ID",
                "Join matches",
                "Another variable",
                paste0("[+] ", name(ds2)),
                paste0(printed_order_apidocs2_merge_by_caseid) # nolint
            ),
            collapse = "\n"
            ),
            fixed = TRUE
        )
        expect_identical(
            names(ds1),
            c(
                "id", "matches", "other_var", "allpets", "q1", "petloc", "ndogs",
                "ndogs_a", "ndogs_b", "q3", "country", "wave", "stringid"
            )
        )
        expect_identical(hiddenVariables(ds1, "name"), "Weight")
    })

    ## More tests to write:
    ## 1) check for handling of the conflicted alias
    ## 2) weight_variables?
    ## 3) apply exclusion filter on either dataset

    test_that("Can select variables to join", {
        ds1 <- newDatasetFromFixture("join-apidocs2-to-me")
        with_consent(ds1$allpets_1 <- NULL)
        ds1 <- merge(ds1, ds2[c("stringid", "q1", "petloc")],
            by.x = "id", by.y = "stringid"
        )
        expect_identical(
            names(ds1),
            c("id", "matches", "other_var", "q1", "petloc")
        )
    })

    test_that("Can select variables and rows to join", {
        ds1 <- newDatasetFromFixture("join-apidocs2-to-me")
        with_consent(ds1$allpets_1 <- NULL)
        ds1 <- merge(ds1, ds2[ds2$stringid == "43805958", c("stringid", "q1", "petloc")],
            by.x = "id", by.y = "stringid"
        )
        expect_prints(ordering(ds1),
            paste(c(
                "ID",
                "Join matches",
                "Another variable",
                paste0("[+] ", name(ds2)),
                paste("    ",
                    c(
                        "[+] Key Pet Indicators",
                        "    Pet",
                        "    Pets by location"
                    ),
                    sep = "", collapse = "\n"
                )
            ),
            collapse = "\n"
            ),
            fixed = TRUE
        )
        expect_identical(
            names(ds1),
            c("id", "matches", "other_var", "q1", "petloc")
        )
        expect_equal(sum(table(ds1$q1)), 1)
    })

    test_that("Can select rows to join", {
        # When merging datasets during jenkins run tests, the order of the
        # variables is *sometimes* not copied (usually Pet Name/q3 is missing),
        # which makes this test fail. The ordering should be fully copied
        # during the merge operation. Reproduction of the bug on production or
        # alpha did not yield errors with order. Repro. locally is possible
        ds1 <- newDatasetFromFixture("join-apidocs2-to-me")
        with_consent(ds1$allpets_1 <- NULL)
        ds1 <- merge(ds1, ds2[ds2$stringid == "43805958", ],
            by.x = "id", by.y = "stringid"
        )
        expect_prints(ordering(ds1),
            paste(c(
                "ID",
                "Join matches",
                "Another variable",
                paste0("[+] ", name(ds2)),
                paste0(printed_order_apidocs2_merge_by_stringid) # nolint
            ),
            collapse = "\n"
            ),
            fixed = TRUE
        )
        expect_identical(
            names(ds1),
            c(
                "id", "matches", "other_var", "allpets", "q1", "petloc", "ndogs",
                "ndogs_a", "ndogs_b", "q3", "country", "wave"
            )
        )
        expect_equal(sum(table(ds1$q1)), 1)
    })
})
