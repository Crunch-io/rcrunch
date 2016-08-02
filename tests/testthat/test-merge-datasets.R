context("Merge/extend dataset")

with_mock_HTTP({
    ds1 <- loadDataset("test ds")
    ds2 <- loadDataset("ECON.sav")

    test_that("Input validation for merge/extend (plus method dispatch)", {
        expect_error(extendDataset(1),
            "x must be a Crunch Dataset")
        expect_error(merge(ds1, 1),
            "y must be a Crunch Dataset")
        expect_error(merge(ds1, ds2, by.x=1),
            "by.x must be a Crunch Variable")
        expect_error(merge(ds1, ds2, by.x=ds2[[1]]),
            "by.x must be a variable in x")
        expect_error(merge(ds1, ds2, by.x=ds1[[1]], by.y=1),
            "by.y must be a Crunch Variable")
        expect_error(merge(ds1, ds2, by.x=ds1[[1]], by.y=ds1[[1]]),
            "by.y must be a variable in y")
        expect_error(merge(ds1, ds2, by.x=ds1[[1]], by.y=ds2[[1]], all=TRUE),
            'Option "all" not supported.')
        expect_error(merge(ds1, ds2, by.x=ds1[[1]], by.y=ds2[[1]], all.x=FALSE),
            'Option "all.x=FALSE" not supported.')
        expect_error(merge(ds1, ds2, by.x=ds1[[1]], by.y=ds2[[1]], all.y=TRUE),
            'Option "all.y" not supported.')
    })

    test_that("Correct payload without filtering", {
        expect_warning(
            expect_POST(merge(ds1, ds2, by.x=ds1$birthyr, ds2$birthyr),
                '/api/datasets/dataset1/variables/',
                '{"function":"adapt",',
                '"args":[{"dataset":"/api/datasets/dataset3/","filter":null},',
                '{"variable":"/api/datasets/dataset3/variables/birthyr/"},',
                '{"variable":"/api/datasets/dataset1/variables/birthyr/"}]}'),
            "Variable birthyr is hidden")
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
    "Weight",
    "Person ID"
)

with_test_authentication({
    ds1 <- newDatasetFromFixture("join-apidocs2-to-me")
    ds1$allpets_1 <- NULL
    ds2 <- newDatasetFromFixture("apidocs2")
    test_that("Shape of apidocs2", {
        expect_output(ordering(ds2),
            paste(printed_order_apidocs2, collapse="\n"), fixed=TRUE)
        expect_identical(names(ds2), c("allpets", "q1", "petloc", "ndogs",
            "ndogs_a", "ndogs_b", "q3", "country", "wave", "stringid"))
        expect_identical(dim(ds2), c(20L, 10L))
    })
    test_that("Shape of join-to-me", {
        expect_identical(names(ds1), c("id", "matches", "other_var"))
        expect_identical(dim(ds1), c(14L, 3L))
    })

    test_that("An uncomplicated merge on a text key", {
        ds1 <- merge(ds1, ds2, by.x=ds1$id, by.y=ds2$stringid)
        expect_is(ds1, "CrunchDataset")
        expect_identical(dim(ds1), c(14L, 12L))
        expect_output(ordering(ds1),
            paste(c(
                "ID",
                "Join matches",
                "Another variable",
                paste0("[+] ", name(ds2)),
                paste("    ",
                    printed_order_apidocs2[-length(printed_order_apidocs2)],
                    sep="", collapse="\n")),
                collapse="\n"),
            fixed=TRUE)
        expect_identical(names(ds1),
            c("id", "matches", "other_var", "allpets", "q1", "petloc", "ndogs",
            "ndogs_a", "ndogs_b", "q3", "country", "wave"))
        expect_identical(hiddenVariables(ds1, "name"), c("Case ID", "Weight"))
    })

    test_that("Similarly uncomplicated merge, but numeric and hidden key", {
        ds1 <- newDatasetFromFixture("join-apidocs2-to-me")
        ds1$allpets_1 <- NULL
        type(ds1$id) <- "numeric"

        expect_warning(ds1 <- merge(ds1, ds2, by.x=ds1$id, by.y=ds2$caseid),
            "Variable caseid is hidden")
        expect_is(ds1, "CrunchDataset")
        expect_identical(dim(ds1), c(14L, 13L))
        expect_output(ordering(ds1),
            paste(c(
                "ID",
                "Join matches",
                "Another variable",
                paste0("[+] ", name(ds2)),
                paste("    ",
                    printed_order_apidocs2,
                    sep="", collapse="\n")),
                collapse="\n"),
            fixed=TRUE)
        expect_identical(names(ds1),
            c("id", "matches", "other_var", "allpets", "q1", "petloc", "ndogs",
            "ndogs_a", "ndogs_b", "q3", "country", "wave", "stringid"))
        expect_identical(hiddenVariables(ds1, "name"), "Weight")
    })

    ## Tests to write:
    ## 1) check for handling of the conflicted alias
    ## 2) weight_variables?
    ## 3) apply exclusion filter on either dataset
    ## 4) Next: filter rows/cols
})
