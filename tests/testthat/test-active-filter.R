context("Filtering datasets and variables in the R session")

with_mock_crunch({
    ds <- loadDataset("test ds")
    ds2 <- ds[ds$gender == "Male", ]
    ds3 <- ds2[ds2$birthyr > 1981, ]

    test_that("A clean dataset has NULL activeFilter", {
        expect_null(activeFilter(ds))
    })
    test_that("Getting a variable from a clean dataset has NULL activeFilter", {
        expect_null(activeFilter(ds$gender))
    })
    test_that("A null filter becomes valid JSON", {
        expect_equal(unclass(toJSON(zcl(activeFilter(ds)))), "{}")
    })

    test_that("[ method on dataset adds an active filter", {
        expect_identical(activeFilter(ds2), ds$gender == "Male")
    })
    test_that("Active filter persists on refreshing dataset", {
        expect_identical(activeFilter(refresh(ds2)), ds$gender == "Male") # nolint
    })
    test_that("Further [ on a filtered dataset ands the filters together", {
        expect_identical(
            activeFilter(ds3),
            ds$gender == "Male" & ds$birthyr > 1981
        )
    })
    test_that("But further [ on filtered dataset must match the dataset's filtering", {
        expect_error(
            ds2b <- ds2[ds$birthyr > 1981, ],
            paste0(
                "In ds2[ds$birthyr > 1981, ], object and subsetting expression ",
                "have different filter expressions"
            ),
            fixed = TRUE
        )
        expect_error(
            ds2b <- ds2[ds3$birthyr > 1981, ],
            paste0(
                "In ds2[ds3$birthyr > 1981, ], object and subsetting ",
                "expression have different filter expressions"
            ),
            fixed = TRUE
        )
    })

    test_that("subset method for dataset does the same", {
        expect_identical(subset(ds, ds$gender == "Male"), ds2)
    })

    test_that("Variables extracted from a filtered dataset are also filtered", {
        expect_identical(activeFilter(ds2$birthyr), ds$gender == "Male")
        expect_identical(
            activeFilter(ds[ds$gender == "Male", "birthyr"]),
            ds$gender == "Male"
        )
    })

    test_that("Subvariables extracted from a filtered array are also filtered", {
        mr <- ds2$mymrset
        expect_true(is.Multiple(mr))
        expect_identical(activeFilter(mr), ds$gender == "Male")
        sv1 <- subvariables(mr)[[1]]
        expect_true(is.Categorical(sv1))
        expect_identical(activeFilter(sv1), ds$gender == "Male")
    })

    test_that("Active filter persists on refreshing variable", {
        expect_identical(
            activeFilter(refresh(ds2$birthyr)),
            ds$gender == "Male"
        )
    })

    test_that("Getting weight variable from filtered dataset is filtered", {
        otherds <- loadDataset("ECON.sav")
        expect_warning(
            expect_identical(weight(otherds), otherds$birthyr),
            "Variable birthyr is hidden"
        )
        otherds2 <- otherds[otherds$gender == "Male", ]
        expect_warning(
            expect_identical(weight(otherds2), otherds2$birthyr),
            "Variable birthyr is hidden"
        )
        expect_identical(activeFilter(weight(otherds2)), otherds$gender == "Male")
    })

    test_that("activeFilter from filtered CrunchVariable", {
        expect_null(activeFilter(ds$birthyr))
        expect_identical(
            activeFilter(ds2$birthyr),
            activeFilter(ds$birthyr[ds$gender == "Male"])
        )
        expect_identical(
            activeFilter(ds$birthyr[ds$gender == "Male"]),
            ds$gender == "Male"
        )
    })

    test_that("Further [ on a filtered variable ands the filters together", {
        expect_identical(
            activeFilter(ds2$gender[ds2$birthyr > 1981]),
            ds$gender == "Male" & ds$birthyr > 1981
        )
    })
    test_that("But further [ on filtered variable must match the variable's filtering", {
        expect_error(ds2$gender[ds$birthyr > 1981],
            paste0(
                "In ds2$gender[ds$birthyr > 1981], object and subsetting ",
                "expression have different filter expressions"
            ),
            fixed = TRUE
        )
        expect_error(ds2$gender[ds3$birthyr > 1981],
            paste0(
                "In ds2$gender[ds3$birthyr > 1981], object and subsetting ",
                "expression have different filter expressions"
            ),
            fixed = TRUE
        )
    })

    age <- ds$starttime - ds$birthyr
    test_that("activeFilter from CrunchExpr", {
        expect_identical(
            activeFilter(age[ds$gender == "Male"]),
            ds$gender == "Male"
        )
    })
    test_that("activeFilter passes across operations among vars/exprs", {
        expect_identical(
            activeFilter(ds$birthyr[ds$gender == "Male"] - ds$starttime[ds$gender == "Male"]),
            ds$gender == "Male"
        )
    })
    age2 <- ds2$starttime - ds2$birthyr
    test_that("activeFilter from CrunchExpr from filtered dataset", {
        expect_identical(activeFilter(age2), ds$gender == "Male") # nolint
    })
    test_that("Further [ on a filtered expression ands the filters together", {
        expect_identical(
            activeFilter(age2[ds2$birthyr > 1981]),
            ds$gender == "Male" & ds$birthyr > 1981
        )
    })
    test_that("But further [ on filtered expression must match the expression's filtering", {
        expect_error(age2[ds$birthyr > 1981],
            paste0(
                "In age2[ds$birthyr > 1981], object and subsetting expression ",
                "have different filter expressions"
            ),
            fixed = TRUE
        )
        expect_error(age2[ds3$birthyr > 1981],
            paste0(
                "In age2[ds3$birthyr > 1981], object and subsetting ",
                "expression have different filter expressions"
            ),
            fixed = TRUE
        )
    })

    test_that("Vars with different activeFilters can't combine", {
        expect_error(
            ds$birthyr[ds$gender == "Male"] - ds$starttime[ds$gender == "Female"],
            "Cannot combine expressions with different filters"
        )
        expect_error(
            ds$birthyr[ds$gender == "Male"] - ds3$starttime,
            "Cannot combine expressions with different filters"
        )
    })
    test_that("Exprs with different activeFilters can't combine", {
        expect_error(
            age[ds$gender == "Male"] - age[ds$gender == "Female"],
            "Cannot combine expressions with different filters"
        )
    })
    test_that("Vars/exprs together with different active filters can't combine", {
        expect_error(
            ds$birthyr[ds$gender == "Male"] - age[ds$gender == "Female"],
            "Cannot combine expressions with different filters"
        )
    })

    test_that("Can combine var with activeFilter with non-Crunch object", {
        expect_identical(
            activeFilter(ds$birthyr[ds$gender == "Male"] - 50),
            ds$gender == "Male"
        )
        expect_identical(
            activeFilter(2016 - ds$birthyr[ds$gender == "Male"]),
            ds$gender == "Male"
        )
    })

    test_that("Expression on filtered variable keeps its filter", {
        expect_identical(
            activeFilter(bin(ds2$birthyr)),
            ds$gender == "Male"
        )
        expect_identical(
            activeFilter(is.na(ds2$birthyr)),
            ds$gender == "Male"
        )
        expect_identical(
            activeFilter(!is.na(ds2$birthyr)),
            ds$gender == "Male"
        )
    })

    test_that("harmonizeFilters creates the expected filter", {
        expected_expr <- list(
            `function` = "and",
            args = list(list(
                `function` = "==",
                args = list(list(variable = "https://app.crunch.io/api/datasets/1/variables/gender/"),
                            list(value = 1L)
                )
            ), list(
                `function` = "in",
                args = list(
                    list(`function` = "row", args = list()),
                    list(column = c(6, 10, 12, 20, 21)
                    )
                )
            )
            )
        )
        ds_harmonized <- harmonizeFilters(ds2, activeFilter(ds2), 1:5)
        expect_is(ds_harmonized, "CrunchDataset")
        expect_identical(activeFilter(ds_harmonized)@expression,
                         expected_expr)
    })
})

with_test_authentication({
    ds <- newDataset(df)
    ds2 <- ds[ds$v4 == "C", ]
    ds2b <- ds[ds$v4 != "B", ]
    ds3 <- ds[ds$v3 > 11, ]
    ds4 <- ds[is.na(ds$v1), ]

    test_that("filtered dim", {
        expect_identical(dim(ds2), c(10L, 6L))
        expect_identical(dim(ds2b), c(10L, 6L))
        expect_identical(dim(ds3), c(16L, 6L))
        expect_identical(dim(ds4), c(5L, 6L))
    })

    test_that("activeFilter appears in print method for dataset", {
        expect_prints(ds3, "Filtered by v3 > 11")
        expect_false(any(grepl("Filtered by", get_output(ds))))
    })

    test_that("Filtered variables return filtered values from as.vector", {
        expect_identical(
            as.vector(ds2$v3),
            c(9, 11, 13, 15, 17, 19, 21, 23, 25, 27)
        )
        expect_identical(
            as.vector(ds2b$v3),
            c(9, 11, 13, 15, 17, 19, 21, 23, 25, 27)
        )
        expect_identical(as.vector(ds3$v3), as.numeric(12:27))
        expect_identical(as.vector(ds4$v3), as.numeric(8:12))
    })

    test_that("activeFilter appears in print method for variables", {
        expect_prints(ds3$v3, "Filtered by v3 > 11")
    })

    test_that("as.data.frame when filtered", {
        df2 <- as.data.frame(ds2)
        expect_identical(df2$v3, c(9, 11, 13, 15, 17, 19, 21, 23, 25, 27))
        df3 <- as.data.frame(ds3)
        expect_equivalent(df3$v3, 12:27)
        skip_on_local_backend("Vagrant host doesn't serve files correctly")
        expect_equivalent(
            as.data.frame(ds2[, c("v3", "v4")], force = TRUE),
            df[df$v4 == "C", c("v3", "v4")]
        )
    })

    test_that("filtered cubing", {
        expect_equivalent(
            as.array(crtabs(~v4, data = ds[ds$v4 == "C", ])),
            array(c(0, 10), dim = 2L, dimnames = list(v4 = c("B", "C")))
        )
        expect_equivalent(
            as.array(crtabs(~v4, data = ds4)),
            array(c(3, 2), dim = 2L, dimnames = list(v4 = c("B", "C")))
        )
    })

    test_that("filtered updating", {
        skip("TODO")
    })
})
