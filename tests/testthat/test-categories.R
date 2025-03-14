context("Categories")

with_mock_crunch({
    ds <- cachedLoadDataset("test ds")
    cats <- categories(ds$gender)

    test_that("category init", {
        expect_is(cats[[1]], "Category")
        expect_true(is.category(cats[[1]]))
        expect_is(cats, "Categories")
        expect_true(is.categories(cats))
        expect_length(cats, 3)
    })

    test_that("Categories print method", {
        expect_prints(
            cats[[1]],
            get_output(data.frame(id = 1, name = "Male", value = 1, missing = FALSE))
        )
        expect_prints(
            cats,
            get_output(
                data.frame(
                    id = c(1, 2, -1), name = c("Male", "Female", "No Data"),
                    value = c(1, 2, NA), missing = c(FALSE, FALSE, TRUE)
                )
            )
        )
    })

    test_that("Categories validation", {
        expect_error(Categories(
            list(id = -1L, name = "B", numeric_value = 1L, missing = FALSE),
            list(id = 2L, name = "C", numeric_value = 2L, missing = FALSE),
            list(id = -1L, name = "No Data", numeric_value = NULL, missing = TRUE)
        ), "Invalid category ids: must be unique")
        expect_error(Categories(
            list(id = 1L, name = "Name 1", numeric_value = 1L, missing = FALSE),
            list(id = 2L, name = "Name 1", numeric_value = 2L, missing = FALSE),
            list(id = -1L, name = "No Data", numeric_value = NULL, missing = TRUE)
        ), "Invalid category names: must be unique")
    })

    test_that("fill in category id when missing", {
        expect_equal(
            Categories(list(name = "A"), list(name = "B")),
            Categories(list(id = 1L, name = "A"), list(id = 2L, name = "B"))
        )

        expect_equal(
            Categories(list(name = "A", id = 2L), list(name = "B")),
            Categories(list(id = 2L, name = "A"), list(id = 1L, name = "B"))
        )
    })

    test_that("category slicers", {
        expect_true(is.categories(cats[1]))
        expect_equal(cats[c("Female", "Male")], cats[c(2, 1)])
        expect_error(
            cats[c("Female", "Male", "not a category")],
            "subscript out of bounds: not a category"
        )
        expect_error(
            cats[c(1, 2, 5)],
            "subscript out of bounds: 5"
        )
        expect_error(
            cats[c(1, 2, 98, 99)],
            "subscript out of bounds: 98 and 99"
        )
    })

    test_that("can use negative subscripts on Categories", {
        expect_true(is.categories(cats[-1]))
        expect_error(
            cats[c(1, -1)],
            "only 0's may be mixed with negative subscripts"
        )
    })

    test_that("categories to/fromJSON", {
        ## cereal serializes to JSON and then deserializes
        expect_identical(cats, Categories(data = cereal(cats)))
        expect_identical(cats[1], Categories(data = cereal(cats[1])))
        expect_identical(cats[[1]], Category(data = cereal(cats[[1]])))
    })

    test_that("lapply categories", {
        expect_identical(lapply(cats, function(x) x), cats)
    })

    test_that("category getters", {
        male <- cats[[1]]
        expect_identical(name(male), "Male")
        expect_identical(value(male), 1)
        expect_identical(id(male), 1L)
        expect_identical(names(cats), c("Male", "Female", "No Data"))
        expect_identical(values(cats), c(1, 2, NA))
        expect_identical(ids(cats), c(1L, 2L, -1L))
    })

    test_that("category setters", {
        male <- cats[[1]]
        name(male) <- "uomo"
        expect_identical(name(male), "uomo")
        expect_true(is.category(male))
        value(male) <- 42
        expect_identical(value(male), 42)
        expect_error(id(male) <- 4)
        expect_error(value(male) <- "foo")
        expect_true(is.category(male))
    })

    test_that("categories setters", {
        new_names <- c("masculino", "femenino", "No Data")
        names(cats) <- new_names
        expect_equal(names(cats), new_names)
        names(cats)[2] <- "donne"
        expect_equal(names(cats)[1:2], c("masculino", "donne"))
        values(cats)[c(1, 2)] <- c(42, 24)
        expect_identical(values(cats), c(42, 24, NA))
        expect_true(is.categories(cats))
    })

    test_that("validation on category setting", {
        expect_error(
            cats[1] <- "new name",
            "Invalid categories: 1 element is not a Crunch category object"
        )
        expect_error(
            name(cats[[1]]) <- NULL,
            'Names must be of class "character"'
        )
    })

    test_that("names(categories)<- input validation", {
        expect_identical(
            names(categories(ds$gender)),
            c("Male", "Female", "No Data")
        )
        expect_error(
            names(categories(ds$gender))[2] <- NULL,
            "replacement has length zero"
        ) ## R default, good enough
        expect_error(
            names(categories(ds$gender))[2] <- list(foo = "1"),
            'Names must be of class "character"'
        )
        expect_error(
            names(categories(ds$gender))[23] <- "cat",
            "Invalid names: supplied 23 names for 3 categories"
        )
        ## Not ideal error message, but best we can do here
        expect_error(
            names(categories(ds$gender))[1] <- NA_character_,
            "Category names must be non-missing"
        )
        ## Also for "No Data"
        expect_error(
            names(categories(ds$gender))[3] <- NA_character_,
            "Category names must be non-missing"
        )
    })

    test_that("categories<- with invalid input gives helpful message", {
        expect_error(categories(ds$gender) <- 1:3,
            paste(
                "`categories(x) <- value` only accepts Categories,",
                "not numeric. Did you mean",
                "`values(categories(x)) <- value`?"
            ),
            fixed = TRUE
        )
        expect_error(categories(ds$gender) <- c("A", "B", "C"),
            paste(
                "`categories(x) <- value` only accepts Categories,",
                "not character. Did you mean",
                "`names(categories(x)) <- value`?"
            ),
            fixed = TRUE
        )
        expect_error(categories(ds$gender)[1] <- c("A", "B", "C"),
            paste(
                "Invalid categories: 3 elements are not Crunch",
                "category objects"
            ),
            fixed = TRUE
        )
        expect_error(categories(ds$gender)[1] <- list("A", "B", "C"),
            paste(
                "Invalid categories: 3 elements are not Crunch",
                "category objects"
            ),
            fixed = TRUE
        )
        expect_error(categories(ds$gender) <- list(),
            "`categories(x) <- value` only accepts Categories, not list.",
            fixed = TRUE
        )
        expect_error(
            categories(ds$birthyr) <- 1:3,
            "category assignment not defined for NumericVariable"
        )
        expect_error(
            categories(ds$gender) <- categories(ds$gender)[c(1, 2, 5)],
            "subscript out of bounds: 5"
        )
        expect_error(categories(ds$catarray) <- 1:3,
            paste(
                "`categories(x) <- value` only accepts Categories,",
                "not numeric. Did you mean",
                "`values(categories(x)) <- value`?"
            ),
            fixed = TRUE
        )
        expect_error(categories(ds$catarray) <- c("A", "B", "C"),
            paste(
                "`categories(x) <- value` only accepts Categories,",
                "not character. Did you mean",
                "`names(categories(x)) <- value`?"
            ),
            fixed = TRUE
        )
        expect_error(categories(ds$catarray) <- list(),
            "`categories(x) <- value` only accepts Categories, not list.",
            fixed = TRUE
        )
    })

    test_that("names(categories())<- doesn't have warning message", {
        expect_warning(
            expect_PATCH(
                names(categories(ds$gender)) <- c("a", "b", "No Data"),
                "https://app.crunch.io/api/datasets/1/variables/gender/",
                '{"categories":[{"id":1,"missing":false,"name":"a","numeric_value":1},',
                '{"id":2,"missing":false,"name":"b","numeric_value":2},{"id":-1,"missing":true,',
                '"name":"No Data","numeric_value":null}]}'
            ),
            NA # NA means no warning
        )
    })

    test_that("categories ids cannot be set", {
        expect_error(
            ids(cats) <- rev(ids(cats)),
            "Cannot modify category ids"
        )
    })

    test_that("dichotomize", {
        male <- cats[[1]]
        expect_false(is.selected(male))
        male$selected <- TRUE
        expect_true(is.selected(male))
        expect_equal(name(male), "Male")

        expect_false(is.dichotomized(cats))
        dcats <- dichotomize(cats, 1)
        expect_true(is.dichotomized(dcats))
        expect_true(is.selected(dcats[[1]]))
        expect_false(is.selected(dcats[[2]]))
        expect_equal(name(dcats[[1]]), "Male")
        expect_equal(names(dcats), c("Male", "Female", "No Data"))

        dcats2 <- dichotomize(cats, "Female")
        expect_true(is.dichotomized(dcats2))
        expect_false(is.selected(dcats2[[1]]))
        expect_true(is.selected(dcats2[[2]]))

        expect_error(dichotomize(cats, "Cat!"))

        cats2 <- undichotomize(dcats)
        expect_false(is.dichotomized(cats2))
        expect_false(is.selected(cats2[[1]]))
    })

    test_that("is.selected works on Categories", {
        expect_identical(
            is.selected(categories(ds$mymrset)),
            structure(c(FALSE, TRUE, FALSE), .Names = c("0.0", "1.0", "No Data"))
        )
    })
    test_that("is.selected assignment methods", {
        true_body <- paste0(
            '{"categories":[{"id":1,"missing":false,"name":"0.0","numeric_value":',
            '0,"selected":true},{"id":2,"missing":false,"name":"1.0",',
            '"numeric_value":1,"selected":true},{"id":-1,"missing":true,',
            '"name":"No Data","numeric_value":null,"selected":true}]}'
        )

        expect_PATCH(
            is.selected(categories(ds$mymrset)) <- c(TRUE, TRUE, TRUE),
            "https://app.crunch.io/api/datasets/1/variables/mymrset/",
            true_body
        )
        expect_PATCH(
            is.selected(categories(ds$mymrset)) <- TRUE,
            "https://app.crunch.io/api/datasets/1/variables/mymrset/",
            true_body
        )
        expect_PATCH(
            is.selected(categories(ds$mymrset)[2]) <- TRUE,
            "https://app.crunch.io/api/datasets/1/variables/mymrset/",
            paste0(
                '{"categories":[{"id":1,"missing":false,"name":"0.0",',
                '"numeric_value":0,"selected":false},{"id":2,"missing":false,',
                '"name":"1.0","numeric_value":1,"selected":true},{"id":-1,',
                '"missing":true,"name":"No Data","numeric_value":null,',
                '"selected":false}]}'
            )
        )
    })
    test_that("is.selected assignment errors correctly", {
        expect_error(
            is.selected(categories(ds$mymrset)[2]) <- "banana",
            "Value must be either TRUE or FALSE."
        )
        expect_error(
            is.selected(categories(ds$mymrset)) <- c(TRUE, FALSE, TRUE, FALSE),
            paste0("You supplied ", 4, " logical values for ", 3, " Categories.")
        )
    })

    test_that("is.na", {
        expect_identical(is.na(cats), structure(c(FALSE, FALSE, TRUE),
            .Names = c("Male", "Female", "No Data")
        ))
        expect_true(is.na(cats[[3]]))
        expect_false(is.na(cats[[1]]))
    })

    test_that("is.na<- by name", {
        cats <- cats
        try(is.na(cats) <- "Female")
        expect_true(is.categories(cats))
        expect_identical(is.na(cats), structure(c(FALSE, TRUE, TRUE),
            .Names = c("Male", "Female", "No Data")
        ))
        expect_error(
            is.na(cats) <- c("Male", "Prefer not to say"),
            paste0("Category not found: ", dQuote("Prefer not to say"))
        )
        expect_identical(is.na(cats), structure(c(FALSE, TRUE, TRUE),
            .Names = c("Male", "Female", "No Data")
        ))
    })
    test_that("is.na<- by logical", {
        cats <- cats
        try(is.na(cats) <- c(TRUE, FALSE, FALSE))
        expect_true(is.categories(cats))
        expect_identical(is.na(cats), structure(c(TRUE, FALSE, FALSE),
            .Names = c("Male", "Female", "No Data")
        ))
    })

    test_that("na.omit", {
        expect_length(cats, 3)
        expect_length(na.omit(cats), 2)
        expect_true(is.categories(na.omit(cats)))
        expect_true(all(vapply(na.omit(cats), is.category, logical(1))))
    })

    newcat <- Category(name = "Other", id = 4)
    newcat2 <- Category(name = "Something else", id = 5)
    cats2 <- Categories(newcat, newcat2)
    test_that("Category constructor with missing attributes", {
        expect_false(is.na(newcat))
        expect_true(is.na(value(newcat)))
    })
    test_that("c() method for Categories, setup", {
        expect_true(is.categories(cats))
        expect_true(is.categories(cats2))
        expect_true(is.category(newcat))
    })
    test_that("c(Categories, Category)", {
        expect_true(is.categories(c(cats, newcat)))
    })
    test_that("c(Category, Categories)", {
        expect_true(is.categories(c(newcat, cats)))
    })
    test_that("c(Category, Category)", {
        expect_true(is.categories(c(newcat, newcat2)))
    })
    test_that("c(Categories, Categories)", {
        expect_true(is.categories(c(cats, cats2)))
    })


    ds_catdate <- loadDataset("b9d811", project = NULL)
    cats <- categories(ds_catdate$cat_set)

    test_that("catdates getters/setters", {
        expect_equal(dates(cats[[1]]), "2020-01")
        expect_equal(
            dates(cats),
            c("wave1" = "2020-01", "wave2" = "2020-02", "wave3" = "2020-03", "No Data" = NA)
        )

        dates(cats)[[1]] <- "2019-12"
        expect_equal(dates(cats[[1]]), "2019-12")

        dates(cats) <- c("2019-01", "2019-02", "2019-03", NA)
        expect_equal(
            dates(cats),
            c("wave1" = "2019-01", "wave2" = "2019-02", "wave3" = "2019-03", "No Data" = NA)
        )
    })
})


with_test_authentication({
    whereas("When editing categories", {
        ds <- newDataset(df[, 4, drop = FALSE])
        test_that("categories setters persist to the server", {
            expect_equal(names(categories(ds$v4)), c("B", "C", "No Data"))
            expect_equivalent(
                as.array(crtabs(~v4, data = ds)),
                array(c(10, 10), dim = 2L, dimnames = list(v4 = c("B", "C")))
            )

            names(categories(ds$v4))[1] <- "V"
            expect_equal(names(categories(ds$v4)), c("V", "C", "No Data"))
            expect_equivalent(
                as.array(crtabs(~v4, data = ds)),
                array(c(10, 10), dim = 2L, dimnames = list(v4 = c("V", "C")))
            )
            expect_identical(
                names(categories(ds$v4)),
                names(categories(refresh(ds)$v4))
            )

            categories(ds$v4)[1:2] <- categories(ds$v4)[2:1]
            expect_equal(names(categories(ds$v4)), c("C", "V", "No Data"))
        })
        test_that("Can add categories with c()", {
            ds$v4a <- df$v4 ## Another copy
            expect_identical(
                names(categories(ds$v4a)),
                c("B", "C", "No Data")
            )
            categories(ds$v4a) <- c(
                categories(ds$v4a),
                Category(name = "D", id = 4)
            )
            expect_identical(
                names(categories(ds$v4a)),
                c("B", "C", "No Data", "D")
            )
        })
        test_that("Can insert a category in the middle", {
            ds$v4b <- df$v4
            expect_identical(
                names(categories(ds$v4b)),
                c("B", "C", "No Data")
            )
            categories(ds$v4b) <- c(
                categories(ds$v4b)[1:2],
                Category(name = "D", id = 4), categories(ds$v4b)[3]
            )
            expect_identical(
                names(categories(ds$v4b)),
                c("B", "C", "D", "No Data")
            )
        })
        test_that("Can add one to the end", {
            ds$v4c <- df$v4
            expect_identical(
                names(categories(ds$v4c)),
                c("B", "C", "No Data")
            )
            categories(ds$v4c)[[4]] <- Category(name = "D", id = 4)
            expect_identical(
                names(categories(ds$v4c)),
                c("B", "C", "No Data", "D")
            )
        })
        test_that("Can't duplicate categories", {
            ds$v4d <- df$v4
            expect_identical(
                names(categories(ds$v4d)),
                c("B", "C", "No Data")
            )
            expect_error(categories(ds$v4) <- c(
                categories(ds$v4d),
                categories(ds$v4d)
            ))
        })
        test_that("Can delete a category that has no data", {
            ds$v4e <- df$v4
            categories(ds$v4e) <- c(
                categories(ds$v4e)[1:2],
                Category(name = "D", id = 4), categories(ds$v4e)[3]
            )
            expect_identical(
                names(categories(ds$v4e)),
                c("B", "C", "D", "No Data")
            )
            ## Reassign the data from C to D
            ds$v4e[ds$v4e == "C"] <- "D"
            expect_equivalent(
                as.array(crtabs(~v4e, data = ds)),
                array(c(10, 0, 10), dim = 3L, dimnames = list(v4 = c("B", "C", "D")))
            )
            ## Then delete B
            categories(ds$v4e) <- categories(ds$v4e)[-2]
            expect_identical(
                names(categories(ds$v4e)),
                c("B", "D", "No Data")
            )
            expect_equivalent(
                as.array(crtabs(~v4e, data = ds)),
                array(c(10, 10), dim = 2L, dimnames = list(v4 = c("B", "D")))
            )
        })
        test_that("Can't drop categories that have data", {
            ds <- refresh(ds)
            expect_identical(
                names(categories(ds$v4e)),
                c("B", "D", "No Data")
            )
            expect_equivalent(
                as.array(crtabs(~v4e, data = ds)),
                array(c(10, 10), dim = 2L, dimnames = list(v4 = c("B", "D")))
            )
            expect_error(
                categories(ds$v4e) <- categories(ds$v4e)[-2],
                "Cannot delete categories: 4"
            )
            exclusion(ds) <- ds$v4e == "D"
            expect_equivalent(
                as.array(crtabs(~v4e, data = ds)),
                array(c(10, 0), dim = 2L, dimnames = list(v4 = c("B", "D")))
            )
            expect_error(
                categories(ds$v4e) <- categories(ds$v4e)[-2],
                "Cannot delete categories: 4"
            )
            exclusion(ds) <- NULL
            expect_equivalent(
                as.array(crtabs(~v4e, data = ds)),
                array(c(10, 10), dim = 2L, dimnames = list(v4 = c("B", "D")))
            )
        })
    })

    whereas("When manipulating categories of array variables", {
        ds <- newDatasetFromFixture("apidocs")
        test_that("dichotomizing dichotomizes the subvariables", {
            expect_true(is.MR(ds$allpets))
            expect_true(is.dichotomized(categories(ds$allpets)))
            expect_true(is.dichotomized(categories(ds$allpets$allpets_2)))
            ds$allpets <- undichotomize(ds$allpets)
            expect_true(is.CA(ds$allpets))
            expect_false(is.dichotomized(categories(ds$allpets)))
            expect_false(is.dichotomized(categories(ds$allpets$allpets_2)))
        })
        test_that("Editing array categories affects the subvariables too", {
            expect_identical_temp_nodata(
                names(categories(ds$petloc)),
                c("Cat", "Dog", "Bird", "Skipped", "Not Asked", "No Data")
            )
            expect_identical_temp_nodata(
                names(categories(ds$petloc$petloc_home)),
                c("Cat", "Dog", "Bird", "Skipped", "Not Asked", "No Data")
            )
            expect_identical(
                names(table(ds$petloc$petloc_home)),
                c("Cat", "Dog", "Bird")
            )
            names(categories(ds$petloc))[2] <- "Canine"
            expect_identical_temp_nodata(
                names(categories(ds$petloc)),
                c("Cat", "Canine", "Bird", "Skipped", "Not Asked", "No Data")
            )
            expect_identical(
                names(table(ds$petloc$petloc_home)),
                c("Cat", "Canine", "Bird")
            )
            expect_identical_temp_nodata(
                names(categories(ds$petloc$petloc_home)),
                c("Cat", "Canine", "Bird", "Skipped", "Not Asked", "No Data")
            )
        })

        test_that("Reordering array categories", {
            expect_identical_temp_nodata(
                names(categories(ds$petloc)),
                c("Cat", "Canine", "Bird", "Skipped", "Not Asked", "No Data")
            )
            categories(ds$petloc) <- rev(categories(ds$petloc))
            expect_identical_temp_nodata(
                names(categories(ds$petloc)),
                c("No Data", "Not Asked", "Skipped", "Bird", "Canine", "Cat")
            )
            expect_identical_temp_nodata(
                names(categories(ds$petloc$petloc_home)),
                c("No Data", "Not Asked", "Skipped", "Bird", "Canine", "Cat")
            )
        })
        test_that("is.selected method gets and set selection value", {
            ds$mr_sub1 <- factor(sample(1:2, nrow(ds), replace = TRUE))
            ds$mr_sub2 <- factor(sample(1:2, nrow(ds), replace = TRUE))
            ds$mr <- makeMR(ds[, c("mr_sub1", "mr_sub2")], selections = "1", name = "mr")
            is.selected(categories(ds$mr)) <- c(TRUE, TRUE, TRUE)
            expect_true(all(is.selected(categories(ds$mr))))
            is.selected(categories(ds$mr)[2]) <- FALSE
            expect_identical(
                is.selected(categories(ds$mr)),
                structure(c(TRUE, FALSE, TRUE), .Names = c("1", "2", "No Data"))
            )
        })
    })
})
