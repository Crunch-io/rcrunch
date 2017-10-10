context("Variable transformations")

insrts_list <- list(list(anchor = 6, name = "Low",
                         `function` = list(combine = c(1, 2))),
                    list(anchor = 7, name = "High",
                         `function` = list(combine = c(9, 10))))
insrts <- Insertions(data=insrts_list)

test_that("Can make a transforms object from Insertions or lists", {
    expect_true(is.insertions(insrts))
    expect_true(is.list(insrts_list) & !is.insertions(insrts_list))
    trans <- Transforms(insertions = insrts)

    expect_equal(trans, Transforms(insertions = insrts_list))
    expect_equal(trans[["insertions"]], insrts)
})

test_that("Transforms validation", {
    expect_error(Transforms(foo = 'bar'),
                 paste("Transforms must have at least one of",
                       serialPaste(dQuote(c("insertions", "categories", "elements")), "or")))
    expect_error(Transforms(insertions = "foo"),
                 paste0("invalid class ", dQuote("Insertions"),
                        " object: Invalid insertions: 1 element is not a Crunch insertion object\\."))
    expect_error(Transforms(categories = "foo"),
                 paste0("invalid class ", dQuote("Categories"),
                        " object: Invalid categories: 1 element is not a Crunch category object\\."))
})

with_mock_crunch({
    ds <- loadDataset("test ds")

    test_that("Can get and set transform", {
        expect_equivalent(transforms(ds$location),
                     Transforms(insertions = list(
                         list(anchor = "3", name = "London+Scotland",
                              `function` = list(combine = c("1", "2")))),
                                categories = NULL,
                                elements = NULL)
                     )
        loc_ary <- array(c(7, 10, 17),
                         dimnames = list("location" = c("London", "Scotland",
                                                        "London+Scotland")))
        class(loc_ary) <- "CategoricalVariableSummary"

        expect_equivalent(showTransforms(ds$location), loc_ary)

        expect_null(transforms(ds$gender))
        expect_PATCH(transforms(ds$gender) <- Transforms(insertions = list(
            list(anchor = "3", name = "Male+Female",
                 `function` = list(combine = c("1", "2"))))),
            'https://app.crunch.io/api/datasets/1/variables/gender/',
            '{"element":"shoji:entity","body":{"view":{"transform":{',
            '"insertions":[{"anchor":"3","name":"Male+Female","function"',
            ':{"combine":["1","2"]}}]}}}}')
    })
})

with_test_authentication({
    ds <- newDataset(df)

    test_that("Can get and set transforms", {
        trans <- Transforms(insertions = list(
            list(anchor = "3", name = "B+C",
                 `function` = list(combine = c("1", "2")))))
        expect_null(transforms(ds$v4))
        transforms(ds$v4) <- trans
        trans_resp <- trans
        trans_resp["categories"] <- list(NULL)
        trans_resp["elements"] <- list(NULL)
        expect_json_equivalent(transforms(ds$v4), trans_resp)

        v4_ary <- array(c(10, 10, 20), dimnames = list("v4" = c("B", "C", "B+C")))
        class(v4_ary) <- "CategoricalVariableSummary"
        expect_equivalent(showTransforms(ds$v4), v4_ary)
    })
})
