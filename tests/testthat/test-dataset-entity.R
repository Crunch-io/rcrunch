context("Dataset object and methods")

with_mock_crunch({
    ds <- loadDataset("test ds")
    ds2 <- loadDataset("ECON.sav")
    ds3 <- loadDataset("an archived dataset", kind = "archived")

    today <- "2016-02-11"

    test_that("Dataset can be loaded", {
        expect_true(is.dataset(ds))
    })

    test_that("CrunchDataset class (re-)init preserves object state", {
        expect_identical(CrunchDataset(ds), ds)
        expect_identical(CrunchDataset(ds[, "gender"]), ds[, "gender"])
        expect_identical(
            CrunchDataset(ds[ds$gender == "Female", ]),
            ds[ds$gender == "Female", ]
        )
        # Now confirm the same with subclasses
        SubDataset <- setClass("SubDataset", contains = "CrunchDataset")
        expect_is(SubDataset(ds), "SubDataset")
        expect_identical(names(SubDataset(ds[, "gender"])), "gender")
    })

    test_that("Dataset attributes", {
        expect_identical(name(ds), "test ds")
        expect_identical(description(ds), "")
        expect_identical(id(ds), "1")
        expect_null(notes(ds))
    })

    test_that("Dataset attribute setting", {
        expect_PATCH(
            name(ds) <- "New name",
            "https://app.crunch.io/api/datasets/1/",
            '{"name":"New name"}'
        )
        expect_PATCH(
            notes(ds) <- "Ancillary information",
            "https://app.crunch.io/api/datasets/1/",
            '{"notes":"Ancillary information"}'
        )
    })

    test_that("Dataset attribute setters update in place too", {
        with_PATCH(NULL, {
            name(ds) <- "New name"
            expect_identical(name(ds), "New name")
            notes(ds) <- "Ancillary information"
            expect_identical(notes(ds), "Ancillary information")
        })
    })


    test_that("Population setting", {
        expect_equal(popSize(ds), 90000000)
        expect_equal(popMagnitude(ds), 3)
        expect_no_request(setPopulation(ds, popSize(ds), popMagnitude(ds)))
        expect_PATCH(
            setPopulation(ds, size = 6000),
            "https://app.crunch.io/api/datasets/1/settings/",
            '{"population":{"magnitude":3,"size":6000}}'
        )
        expect_PATCH(
            setPopulation(ds, magnitude = 6),
            "https://app.crunch.io/api/datasets/1/settings/",
            '{"population":{"magnitude":6,"size":90000000}}'
        )
        expect_PATCH(
            setPopulation(ds, size = 6000, magnitude = 6),
            "https://app.crunch.io/api/datasets/1/settings/",
            '{"population":{"magnitude":6,"size":6000}}'
        )
        expect_PATCH(
            setPopulation(ds, size = NULL, magnitude = 6),
            "https://app.crunch.io/api/datasets/1/settings/",
            '{"population":null}'
        )
        expect_PATCH(
            setPopulation(ds, size = NULL),
            "https://app.crunch.io/api/datasets/1/settings/",
            '{"population":null}'
        )
        expect_PATCH(
            popSize(ds) <- 6000,
            "https://app.crunch.io/api/datasets/1/settings/",
            '{"population":{"magnitude":3,"size":6000}}'
        )
        expect_PATCH(
            popMagnitude(ds) <- 6,
            "https://app.crunch.io/api/datasets/1/settings/",
            '{"population":{"magnitude":6,"size":90000000}}'
        )
    })
    test_that("setPopulation errors correctly", {
        expect_error(
            setPopulation(ds, magnitude = 1000),
            "Magnitude must be either 3, 6, or 9"
        )
        expect_error(popMagnitude(ds) <- NULL,
            paste0(
                "Magnitude cannot be set to `NULL`. Did you mean ",
                "to remove population size with `popSize(x) <- ",
                "NULL`?"
            ),
            fixed = TRUE
        )
        expect_error(setPopulation(ds, size = 1000, magnitude = NULL),
            paste0(
                "Magnitude cannot be set to `NULL`. Did you mean ",
                "to remove population size with `popSize(x) <- ",
                "NULL`?"
            ),
            fixed = TRUE
        )

        # setting a population to null that is already null does nothing.
        expect_no_request(popSize(ds2) <- NULL)

        expect_error(ds <- setPopulation(ds, size = 12345, magnitude = NULL),
            paste0(
                "Magnitude cannot be set to `NULL`. Did you mean ",
                "to remove population size with `popSize(x) <- ",
                "NULL`?"
            ),
            fixed = TRUE
        )
    })
    test_that("setPopulation handles datasets with no population values", {
        expect_error(
            setPopulation(ds2, magnitude = 3),
            "Dataset does not have a population, please set one before attempting to change magnitude"
        )
        expect_warning(
            expect_PATCH(
                popSize(ds2) <- 6000,
                "https://app.crunch.io/api/datasets/3/settings/",
                '{"population":{"magnitude":3,"size":6000}}'
            ),
            "Dataset magnitude not set, defaulting to thousands"
        )
    })



    test_that("Name setting validation", {
        expect_error(
            name(ds) <- 3.14,
            'Names must be of class "character"'
        )
        expect_error(
            name(ds) <- NULL,
            'Names must be of class "character"'
        )
    })

    test_that("archived", {
        expect_false(is.archived(ds))
        expect_false(is.archived(ds2))
        expect_true(is.archived(ds3))
    })

    test_that("archive setting", {
        expect_PATCH(
            is.archived(ds2) <- TRUE,
            "https://app.crunch.io/api/datasets/3/",
            '{"archived":true}'
        )
        expect_PATCH(
            archive(ds2),
            "https://app.crunch.io/api/datasets/3/",
            '{"archived":true}'
        )
    })

    test_that("draft/published", {
        expect_true(is.published(ds))
        expect_false(is.published(ds2))
        expect_false(is.draft(ds))
        expect_true(is.draft(ds2))
    })

    test_that("draft/publish setting", {
        expect_PATCH(
            is.published(ds2) <- TRUE,
            "https://app.crunch.io/api/datasets/3/",
            '{"is_published":true}'
        )
        expect_PATCH(
            is.published(ds) <- FALSE,
            "https://app.crunch.io/api/datasets/1/",
            '{"is_published":false}'
        )
        expect_PATCH(
            is.draft(ds2) <- FALSE,
            "https://app.crunch.io/api/datasets/3/",
            '{"is_published":true}'
        )
        expect_PATCH(
            is.draft(ds) <- TRUE,
            "https://app.crunch.io/api/datasets/1/",
            '{"is_published":false}'
        )
        expect_PATCH(
            publish(ds2),
            "https://app.crunch.io/api/datasets/3/",
            '{"is_published":true}'
        )
        expect_no_request(publish(ds))
        expect_no_request(is.draft(ds) <- FALSE)
        expect_no_request(is.published(ds) <- TRUE)
    })

    test_that("start/endDate", {
        expect_identical(startDate(ds), "2016-01-01")
        expect_identical(endDate(ds), "2016-01-01")
        expect_null(startDate(ds2))
        expect_null(endDate(ds2))
    })

    test_that("startDate<- makes correct request", {
        expect_PATCH(
            startDate(ds2) <- today,
            "https://app.crunch.io/api/datasets/3/",
            '{"start_date":"2016-02-11"}'
        )
        expect_PATCH(
            startDate(ds) <- NULL,
            "https://app.crunch.io/api/datasets/1/",
            '{"start_date":null}'
        )
    })
    test_that("endDate<- makes correct request", {
        expect_PATCH(
            endDate(ds2) <- today,
            "https://app.crunch.io/api/datasets/3/",
            '{"end_date":"2016-02-11"}'
        )
        expect_PATCH(
            endDate(ds) <- NULL,
            "https://app.crunch.io/api/datasets/1/",
            '{"end_date":null}'
        )
    })

    test_that("Dataset VariableCatalog index is ordered", {
        expect_identical(
            urls(variables(ds)),
            c(
                "https://app.crunch.io/api/datasets/1/variables/birthyr/",
                "https://app.crunch.io/api/datasets/1/variables/gender/",
                "https://app.crunch.io/api/datasets/1/variables/location/",
                "https://app.crunch.io/api/datasets/1/variables/mymrset/",
                "https://app.crunch.io/api/datasets/1/variables/textVar/",
                "https://app.crunch.io/api/datasets/1/variables/starttime/",
                "https://app.crunch.io/api/datasets/1/variables/catarray/"
            )
        )
        ## allVariables is ordered too
        expect_identical(
            urls(allVariables(ds)),
            c(
                "https://app.crunch.io/api/datasets/1/variables/birthyr/",
                "https://app.crunch.io/api/datasets/1/variables/gender/",
                "https://app.crunch.io/api/datasets/1/variables/location/",
                "https://app.crunch.io/api/datasets/1/variables/mymrset/",
                "https://app.crunch.io/api/datasets/1/variables/textVar/",
                "https://app.crunch.io/api/datasets/1/variables/starttime/",
                "https://app.crunch.io/api/datasets/1/variables/catarray/"
            )
        )
    })

    test_that("namekey function exists and affects names()", {
        expect_identical(getOption("crunch.namekey.dataset"), "alias")
        expect_identical(names(ds), aliases(variables(ds)))
        with(temp.option(crunch.namekey.dataset = "name"), {
            expect_identical(names(ds), names(variables(ds)))
        })
    })

    test_that("Dataset ncol doesn't make any requests", {
        with(temp.options(httpcache.log = ""), {
            logs <- capture.output(nc <- ncol(ds))
        })
        expect_identical(logs, character(0))
        expect_identical(nc, 7L)
        expect_identical(dim(ds), c(25L, 7L))
    })

    test_that("Dataset has names() and extract methods work", {
        expect_false(is.null(names(ds)))
        expect_identical(
            names(ds),
            c("birthyr", "gender", "location", "mymrset", "textVar", "starttime", "catarray")
        )
        expect_true(is.variable(ds[[1]]))
        expect_true("birthyr" %in% names(ds))
        expect_true(is.variable(ds$birthyr))
        expect_true(is.dataset(ds[2]))
        expect_identical(ds["gender"], ds[2])
        expect_identical(ds[, 2], ds[2])
        expect_identical(ds[names(ds) == "gender"], ds[2])
        expect_identical(names(ds[2]), c("gender"))
        expect_identical(dim(ds[2]), c(25L, 1L))
        expect_null(ds$not.a.var.name)
        expect_error(ds[[999]], "subscript out of bounds")
        expect_identical(ds[[self(ds$gender)]], ds$gender)
    })

    with(temp.option(crunch.namekey.dataset = "name"), {
        test_that("'namekey' feature (that should be deprecated) is respected", {
            expect_true(is.Numeric(ds$`Birth Year`))
            expect_null(ds$birthyr)
        })
    })

    test_that("Variables can be extracted by url", {
        url <- urls(variables(ds))[1]
        expect_identical(ds[[url]], ds[["birthyr"]])
        expect_identical(ds[url], ds["birthyr"])
    })

    test_that("Setting variable metadata on the dataset", {
        # See other tests in test-variable-catalog.R; this tests the [<- methods
        # on the dataset entity
        expect_PATCH(
            names(variables(ds[1])) <- "year of birth",
            "https://app.crunch.io/api/datasets/1/variables/", '{"element":"shoji:catalog","index":{',
            '"https://app.crunch.io/api/datasets/1/variables/birthyr/":{',
            '"name":"year of birth"}}}'
        )
        expect_no_request(names(variables(ds[1])) <- "Birth Year")
    })

    test_that("Dataset extract error handling", {
        expect_error(ds[[999]], "subscript out of bounds")
        expect_error(
            ds[c("gender", "NOTAVARIABLE")],
            "Undefined columns selected: NOTAVARIABLE"
        )
        expect_null(ds$name)
    })

    test_that("Dataset logical extract cases", {
        expect_null(activeFilter(ds[TRUE, ]))
        expect_error(
            ds[FALSE, ],
            "Invalid logical filter: FALSE"
        )
        expect_error(
            ds[NA, ],
            "Invalid logical filter: NA"
        )
        expect_null(activeFilter(ds[rep(TRUE, nrow(ds)), ]))
        expect_error(
            ds[c(TRUE, FALSE), ],
            "Logical filter vector is length 2, but dataset has 25 rows"
        )
        expect_prints(
            toJSON(activeFilter(ds[c(
                rep(FALSE, 4), TRUE,
                rep(FALSE, 20)
            ), ])),
            paste0(
                '{"function":"==","args":[{"function":"row",',
                '"args":[]},{"value":4}]}'
            )
        )
    })

    test_that("Extract from dataset by VariableOrder/Group", {
        ents <- c(
            "https://app.crunch.io/api/datasets/1/variables/gender/",
            "https://app.crunch.io/api/datasets/1/variables/mymrset/"
        )
        ord <- VariableOrder(VariableGroup("G1", entities = ents))
        expect_identical(ds[ord[[1]]], ds[c("gender", "mymrset")])
        expect_identical(ds[ord], ds[c("gender", "mymrset")])
    })

    test_that("show method", {
        expect_identical(
            getShowContent(ds),
            c(
                paste("Dataset", dQuote("test ds")),
                "",
                "Contains 25 rows of 7 variables:",
                "",
                "$birthyr: Birth Year (numeric)",
                "$gender: Gender (categorical)",
                "$location: Categorical Location (categorical)",
                "$mymrset: mymrset (multiple_response)",
                "$textVar: Text variable ftw (text)",
                "$starttime: starttime (datetime)",
                "$catarray: Cat Array (categorical_array)"
            )
        )
    })

    test_that("dataset can refresh", {
        expect_identical(ds, refresh(ds))
    })

    test_that("dataset refresh doesn't GET dataset catalog", {
        with(temp.option(httpcache.log = ""), {
            logs <- capture.output({
                d2 <- refresh(ds)
            })
        })
        in_logs <- function(str, loglines) {
            any(grepl(str, loglines, fixed = TRUE))
        }
        expect_false(in_logs("HTTP GET https://app.crunch.io/api/datasets/ 200", logs))
        expect_true(in_logs("CACHE DROP ^https://app[.]crunch[.]io/api/datasets/", logs))
    })

    test_that("Dataset settings", {
        expect_false(settings(ds)$viewers_can_export)
        expect_true(settings(ds)$viewers_can_change_weight)
        expect_identical(settings(ds)$weight, self(ds$birthyr))
        expect_identical(self(settings(ds)), "https://app.crunch.io/api/datasets/1/settings/")
    })

    test_that("Changing dataset settings", {
        expect_PATCH(
            settings(ds)$viewers_can_export <- TRUE,
            "https://app.crunch.io/api/datasets/1/settings/",
            '{"viewers_can_export":true}'
        )
    })
    test_that("No request made if not altering a setting", {
        expect_no_request(settings(ds)$viewers_can_export <- FALSE)
    })
    test_that("Can set a NULL setting", {
        expect_PATCH(
            settings(ds)$viewers_can_export <- NULL,
            "https://app.crunch.io/api/datasets/1/settings/",
            '{"viewers_can_export":null}'
        )
    })
    test_that("Can set a variable as weight", {
        expect_PATCH(
            settings(ds)$weight <- ds$gender, ## Silly; server would reject, but just checking request
            "https://app.crunch.io/api/datasets/1/settings/",
            '{"weight":"https://app.crunch.io/api/datasets/1/variables/gender/"}'
        )
    })
    test_that("Can't add a setting that doesn't exist", {
        expect_error(
            settings(ds)$NOTASETTING <- TRUE,
            "Invalid attribute: NOTASETTING"
        )
    })
    test_that("Dataset deleting", {
        expect_error(delete(ds), "Must confirm")
        with_consent(expect_DELETE(delete(ds), self(ds))) ## No warning
    })

    with_consent({
        test_that("deleteDataset by name", {
            expect_DELETE(deleteDataset("test ds"), self(ds))
        })
        test_that("deleteDataset by URL", {
            expect_DELETE(deleteDataset(self(ds)), self(ds))
        })
        test_that("deleteDataset by web URL", {
            expect_DELETE(deleteDataset(APIToWebURL(ds)), self(ds))
        })
        test_that("deleteDataset on Dataset object", {
            expect_DELETE(deleteDataset(ds), self(ds))
        })
    })

    test_that("deleteDataset error handling", {
        expect_error(deleteDataset(
            "this is totally not a dataset",
            paste(dQuote("this is totally not a dataset"), "not found")
        ))
        test_that("deleteDataset by index (is no longer supported)", {
            expect_error(
                deleteDataset(4),
                "deleteDataset requires either a Dataset, a unique dataset name, or a URL"
            )
        })
        expect_error(deleteDataset(ds), "Must confirm")
        expect_error(
            deleteDataset("duplicated dataset"),
            paste(
                dQuote("duplicated dataset"), "identifies 2 datasets.",
                "To delete, please identify the dataset uniquely by URL or path."
            )
        )
        expect_error(
            deleteDataset("http://app.crunch.io/api/"),
            "http://app.crunch.io/api/ is not a valid dataset URL"
        )
    })

    test_that("Dashboard URL", {
        expect_null(dashboard(ds))
        expect_PATCH(
            dashboard(ds) <- "https://shiny.crunch.io/example/",
            "https://app.crunch.io/api/datasets/1/",
            '{"app_settings":{"whaam":',
            '{"dashboardUrl":"https://shiny.crunch.io/example/"}}}'
        )
    })

    test_that("Primary key methods", {
        expect_null(pk(ds2))
        expect_identical(pk(ds), ds$birthyr)
        expect_POST(
            pk(ds) <- ds$textVar,
            "https://app.crunch.io/api/datasets/1/pk/",
            '{"pk":["https://app.crunch.io/api/datasets/1/variables/textVar/"]}'
        )
        expect_DELETE(pk(ds2) <- NULL, "https://app.crunch.io/api/datasets/3/pk/")
    })
})

with_test_authentication({
    whereas("When editing dataset metadata", {
        ds <- createDataset(name = now())
        test_that("Name and description setters push to server", {
            d2 <- ds
            name(ds) <- "Bond. James Bond."
            expect_identical(name(ds), "Bond. James Bond.")
            expect_identical(name(refresh(d2)), "Bond. James Bond.")
            description(ds) <- "007"
            expect_identical(description(ds), "007")
            expect_identical(description(refresh(d2)), "007")
            notes(ds) <- "On Her Majesty's Secret Service"
            expect_identical(notes(ds), "On Her Majesty's Secret Service")
            expect_identical(
                notes(refresh(d2)),
                "On Her Majesty's Secret Service"
            )
        })
        test_that("population setters push to server", {
            ds <- setPopulation(ds, 12345, 3)
            expect_equal(popSize(ds), 12345)
            expect_equal(popMagnitude(ds), 3)
            ds <- setPopulation(ds, 54321)
            expect_equal(popSize(ds), 54321)
            expect_equal(popMagnitude(ds), 3)
            ds <- setPopulation(ds, magnitude = 6)
            expect_equal(popSize(ds), 54321)
            expect_equal(popMagnitude(ds), 6)
            ds <- setPopulation(ds, size = NULL, magnitude = 6)
            expect_null(popSize(ds))
            expect_null(popMagnitude(ds))
        })

        test_that("Can unset notes and description", {
            ds <- refresh(ds)
            expect_identical(description(ds), "007")
            description(ds) <- NULL
            expect_null(description(ds))
            expect_identical(notes(ds), "On Her Majesty's Secret Service")
            notes(ds) <- NULL
            expect_null(notes(ds))
        })

        test_that("Can set (and unset) startDate", {
            startDate(ds) <- "1985-11-05"
            expect_identical(startDate(ds), "1985-11-05")
            expect_identical(startDate(refresh(ds)), "1985-11-05T00:00:00")
            startDate(ds) <- NULL
            expect_null(startDate(ds))
            expect_null(startDate(refresh(ds)))
        })
        test_that("Can set (and unset) endDate", {
            endDate(ds) <- "1985-11-05"
            expect_identical(endDate(ds), "1985-11-05")
            expect_identical(endDate(refresh(ds)), "1985-11-05T00:00:00")
            endDate(ds) <- NULL
            expect_null(endDate(ds))
            expect_null(endDate(refresh(ds)))
        })

        test_that("Can publish/unpublish a dataset", {
            expect_true(is.published(ds))
            expect_false(is.draft(ds))
            is.draft(ds) <- TRUE
            expect_false(is.published(ds))
            expect_true(is.draft(ds))
            ds <- refresh(ds)
            expect_false(is.published(ds))
            expect_true(is.draft(ds))
            is.published(ds) <- TRUE
            expect_true(is.published(ds))
            expect_false(is.draft(ds))
        })

        test_that("Can archive/unarchive", {
            expect_false(is.archived(ds))
            is.archived(ds) <- TRUE
            expect_true(is.archived(ds))
            ds <- refresh(ds)
            expect_true(is.archived(ds))
            is.archived(ds) <- FALSE
            expect_false(is.archived(ds))
        })

        test_that("Sending invalid dataset metadata errors usefully", {
            expect_error(
                endDate(ds) <- list(foo = 4),
                "must be a string"
            )
            expect_error(
                startDate(ds) <- 1985,
                "must be a string"
            )
            skip("Improve server-side validation")
            expect_error(
                startDate(ds) <- "a string",
                "Useful error message here"
            )
        })

        ds$name <- 1:2
        test_that("A variable named/aliased 'name' can be accessed", {
            expect_true("name" %in% aliases(variables(ds)))
            expect_true("name" %in% names(ds))
            expect_true(is.Numeric(ds$name))
        })

        test_that("PK methods work", {
            expect_null(pk(ds))
            expect_silent(pk(ds) <- ds$name)
            expect_equal(pk(ds), ds$name)
            expect_silent(pk(ds) <- NULL)
            expect_null(pk(ds))
        })

        test_that("Dataset settings (defaults)", {
            # expect_true(settings(ds)$viewers_can_export) ## Isn't it?
            expect_true(settings(ds)$viewers_can_change_weight)
            expect_null(settings(ds)$weight)
        })

        test_that("Setting and unsetting dataset settings", {
            settings(ds)$viewers_can_change_weight <- FALSE
            expect_false(settings(ds)$viewers_can_change_weight)
            skip("Can't set this setting to NULL to reset it to default")
            settings(ds)$viewers_can_change_weight <- NULL
            ## Go back to default
            expect_true(settings(ds)$viewers_can_change_weight)
        })
        test_that("Setting default weight", {
            settings(ds)$weight <- self(ds$name)
            expect_identical(settings(ds)$weight, self(ds$name))
            ## And it should now be in the weight variables order too
            expect_identical(urls(ShojiOrder(crGET(shojiURL(
                variables(ds),
                "orders", "weights"
            )))), self(ds$name))
            ## Can also remove the setting
            settings(ds)$weight <- NULL
            expect_null(settings(ds)$weight)
        })
        test_that("Junk input validation for settings", {
            expect_error(settings(ds)$weight <- self(ds))
            expect_error(settings(ds)$viewers_can_export <- list(foo = "bar"))
        })
    })

    with(test.dataset(df), {
        test_that("dataset dim", {
            expect_identical(dim(ds), dim(df))
            expect_identical(nrow(ds), nrow(df))
            expect_identical(ncol(ds), ncol(df))
        })

        test_that("Dataset [[<-", {
            v1 <- ds$v1
            name(v1) <- "Variable One"
            ds$v1 <- v1
            expect_identical(names(variables(ds))[1], "Variable One")
            expect_error(
                ds$v2 <- v1,
                "Cannot overwrite one Variable"
            )
        })
    })

    with(test.dataset(mrdf), {
        cast.these <- grep("mr_", names(ds))
        test_that("Dataset [<-", {
            expect_true(all(vapply(
                variables(ds)[cast.these],
                function(x) x$type == "numeric", logical(1)
            )))
            expect_true(all(vapply(
                ds[cast.these],
                function(x) is.Numeric(x), logical(1)
            )))
            ds[cast.these] <- lapply(
                ds[cast.these],
                castVariable, "categorical"
            )
            expect_true(all(vapply(
                variables(ds)[cast.these],
                function(x) x$type == "categorical", logical(1)
            )))
            expect_true(all(vapply(
                ds[cast.these],
                function(x) is.Categorical(x), logical(1)
            )))
        })
        test_that("Dataset [[<- on new array variable", {
            ds$arrayVar <- makeArray(ds[cast.these], name = "Array variable")
            expect_true(is.CA(ds$arrayVar))
            expect_identical(name(ds$arrayVar), "Array variable")
        })
    })
})
