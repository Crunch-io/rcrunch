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

v7_trans <-  loadCube("cubes/univariate-categorical-with-trans.json")

test_that("calcTransform univariate cube", {
    expect_equal(
        calcTransform(v7_trans@arrays$count,
                      Transforms(data=index(variables(v7_trans))[[1]]$view$transform),
                      Categories(data=index(variables(v7_trans))[[1]]$categories)
        ),
                 cubify(10, 5, 5, 0, 15, 10,
                        dims=list(
                            v7=c("C", "D", "E", "No Data", "C, E", "D, E")
                        )))
})

v7 <- loadCube("cubes/univariate-categorical.json")

test_that("if there are no transforms, return the cube as-is", {
    expect_equal(as.array(showTransforms(v7)),
                 cubify(10, 5,
                        dims=list(
                            v7=c("C", "E")
                        )))
})

v7_always <- v7_ifany <- v7_trans
v7_ifany@useNA <- "ifany"
v7_always@useNA <- "always"

test_that("showTransform univariate cube", {
    skip("Cube transforms with categories as missing not implemented")
    showTransforms(v7_trans)
    expect_equal(as.array(showTransforms(v7_trans)),
                 cubify(10, 5, 15, 5,
                        dims=list(
                            v7=c("C", "E", "C, E", "E alone")
                        )))
    expect_equal(as.array(showTransforms(v7_ifany)),
                 cubify(10, 5, 5,
                        dims=list(
                            v7=c("C", "D", "E")
                        )))
    expect_equal(as.array(showTransforms(v7_always)),
                 cubify(10, 5, 5, 0,
                        dims=list(
                            v7=c(LETTERS[3:5], "No Data")
                        )))
})

v4_x_v7 <- v4_x_v7_ifany <- v4_x_v7_always <- loadCube("cubes/cat-x-cat.json")
v4_x_v7_ifany@useNA <- "ifany"
v4_x_v7_always@useNA <- "always"

test_that("useNA on bivariate cube", {
    expect_error(showTransforms(v4_x_v7),
                 paste0("Calculating varaible transforms is not implemented ",
                        "for dimensions greater than 1."))
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
    })
})
