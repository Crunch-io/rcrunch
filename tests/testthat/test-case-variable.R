context("Case variables")

with_mock_crunch({
    ds <- loadDataset("test ds")

    test_that("Case list validator", {
        case <- list(name="Dudes", expression=ds$gender == "Male")
        case_out <- list(id = NULL, name="Dudes",
                         expression=ds$gender == "Male",
                         numeric_value=NULL,
                         missing=FALSE)
        expect_equal(ensureValidCase(case), case_out)
        expect_error(ensureValidCase("case"), "A case must be a list")
        expect_error(ensureValidCase(list(expression=CrunchLogicalExpr())),
                     "a case's name must be a character")
        expect_error(ensureValidCase(list(name="name")),
                     "a case's expression must be a CrunchLogicalExpr")
        expect_error(ensureValidCase(list(name=c("name", "name2"),
                                          expression=CrunchLogicalExpr())),
                     "There is more than one attribute for name")
        expect_error(ensureValidCase(list(name="name",
                                          expression=c(CrunchLogicalExpr(),
                                                       CrunchLogicalExpr()))),
                     "There is more than one attribute for expression")
        expect_error(ensureValidCase(list(name="name",
                                          expression=CrunchLogicalExpr(),
                                          id=0.8)),
                     "a case's id must be an integer")
        expect_error(ensureValidCase(list(name="name",
                                          expression=CrunchLogicalExpr(),
                                          numeric_value="nope")),
                     "a case's numeric_value must be a numeric")
        expect_error(ensureValidCase(list(name="name",
                                          expression=CrunchLogicalExpr(),
                                          missing="nope")),
                     "a case's missing must be a logical")
        expect_error(ensureValidCase(list(not_right="not")),
                     paste("each case must have at most an id, name, expression,",
                     "numeric_value, and missing element. The errant",
                     "arguments were: not_right"))

        else_case <- list(name="Dudes", expression="else")
        else_case_out <- list(id = NULL, name="Dudes",
                              numeric_value=NULL, missing=FALSE)
        expect_equal(ensureValidCase(else_case), else_case_out)
    })

    case_output <- list(
        name="Super clever segmentation",
        derivation=list(
            `function`="case",
            args=list(
                list(column=I(1:2),
                     type=list(
                         value=list(
                             class="categorical",
                             categories=list(
                                 list(id=1, name="Dudes",
                                      numeric_value=NULL, missing=FALSE),
                                 list(id=2, name="Old women",
                                      numeric_value=NULL, missing=FALSE)
                             )
                         )
                     )
                ),
                list(
                    `function`="==",
                    args=list(
                        list(variable="https://app.crunch.io/api/datasets/1/variables/gender/"),
                        list(value=1)
                    )
                ), list(
                    `function`="<",
                    args=list(
                        list(variable="https://app.crunch.io/api/datasets/1/variables/birthyr/"),
                        list(value=1950)
                    )
                )
            )
        )
    )

    test_that("Case variable definition", {
        expect_json_equivalent(
            makeCaseVariable(
                cases = list(
                    list(expression=ds$gender == "Male", name="Dudes"),
                    list(expression=ds$birthyr < 1950, name="Old women")
                ),
                name="Super clever segmentation"),
            case_output)
    })
    test_that("makeCaseVariable works with ... specification", {
        expect_json_equivalent(
            makeCaseVariable(`Dudes`=ds$gender == "Male",
                             `Old women`=ds$birthyr < 1950,
                             name="Super clever segmentation"),
            case_output)
    })
    test_that("makeCaseVariable works with more than one CrunchLogicalExpr", {
        case_output$derivation$args[[3]] <- list(`function`="and",
                                                 "args"=list(
            list(
                `function`="<",
                args=list(
                    list(variable="https://app.crunch.io/api/datasets/1/variables/birthyr/"),
                    list(value=1950)
                )),
            list(
                `function`="==",
                args=list(
                    list(variable="https://app.crunch.io/api/datasets/1/variables/gender/"),
                    list(value=2)
                )
            )))
        expect_json_equivalent(
            makeCaseVariable(`Dudes`=ds$gender == "Male",
                             `Old women`=ds$birthyr < 1950 &
                                 ds$gender == "Female",
                             name="Super clever segmentation"),
            case_output)
    })
    test_that("makeCaseVariable works with ids pre-specified", {
        expect_json_equivalent(
            makeCaseVariable(
                cases = list(
                    list(id=1L, expression=ds$gender == "Male", name="Dudes"),
                    list(expression=ds$birthyr < 1950, name="Old women")
                ),
                name="Super clever segmentation"),
            case_output)

        case_output$derivation$args[[1]]$column <- I(10:11)
        case_output$derivation$args[[1]]$type$value$categories[[1]]$id <- 10L
        case_output$derivation$args[[1]]$type$value$categories[[1]]$numeric_value <- 0
        case_output$derivation$args[[1]]$type$value$categories[[2]]$id <- 11L
        case_output$derivation$args[[1]]$type$value$categories[[2]]$missing <- TRUE
        expect_json_equivalent(
            makeCaseVariable(
                cases = list(
                    list(id=10L, expression=ds$gender == "Male",
                         name="Dudes", numeric_value=0),
                    list(id=11L, expression=ds$birthyr < 1950,
                         name="Old women", missing=TRUE)
                ),
                name="Super clever segmentation"),
            case_output)
    })
    test_that("makeCaseVariable works with an else pre-specified", {
        case_output$derivation$args[[1]]$column[[3]] <- 5L
        case_output$derivation$args[[1]]$type$value$categories[[3]] <- list(
            id=5L, name="Other", numeric_value=NULL, missing=FALSE)
        expect_json_equivalent(
            makeCaseVariable(
                cases = list(
                    list(expression=ds$gender == "Male", name="Dudes"),
                    list(expression=ds$birthyr < 1950, name="Old women"),
                    list(expression="else", name="Other", id=5L)
                ),
                name="Super clever segmentation"),
            case_output)
    })

    test_that("makeCaseVariable has a number of ways to specify else", {
        case_output$derivation$args[[1]]$column[[3]] <- 3L
        case_output$derivation$args[[1]]$type$value$categories[[3]] <- list(
            id=3L, name="Other", numeric_value=NULL, missing=FALSE)
        expect_json_equivalent(
            makeCaseVariable(
                cases = list(
                    list(expression=ds$gender == "Male", name="Dudes"),
                    list(expression=ds$birthyr < 1950, name="Old women"),
                    list(expression="else", name="Other")
                ),
                name="Super clever segmentation"),
            case_output)
        expect_json_equivalent(
            makeCaseVariable(`Dudes`=ds$gender == "Male",
                             `Old women`=ds$birthyr < 1950,
                             `Other`="else",
                             name="Super clever segmentation"),
            case_output)
    })

    test_that("makeCaseVariable errors gracefully", {
        expect_error(makeCaseVariable(cases=list(
            list(expression=ds$gender == "Male", name="Dudes"))),
            'argument "name" is missing, with no default')
        expect_error(makeCaseVariable(`Old women`=ds$birthyr < 1950,
                                      cases=list(
            list(expression=ds$gender == "Male", name="Dudes")), name=""),
            paste("can't have case conditions both in", dQuote("..."),
                "as well as in the", dQuote("cases"),
                "argument, please use one or the other."))
        expect_error(makeCaseVariable(name="Dudes"),
            paste("must supply case conditions in either", dQuote("..."),
                "or the", dQuote("cases"),
                "argument, please use one or the other."))
        expect_error(makeCaseVariable(cases=list(), name="Dudes"),
                     paste("must supply case conditions in either", dQuote("..."),
                     "or the", dQuote("cases"),
                     "argument, please use one or the other."))
        expect_error(makeCaseVariable(cases=list(
            list(expression=ds$gender == "Male", name="Dudes"),
            list(expression="else", name="name", id=0.8)), name=""),
            "id must be an integer")
        expect_error(makeCaseVariable(cases=list(list(
            list(expression=ds$gender == "Male", name="Dudes"),
            list(expression=ds$gender == "Female", name="Not Dudes"),
            list(expression="else", name="else1"))), name=""),
            paste("could not find names for a case; this might be because the",
                "cases were embedded in too many lists."))
        expect_error(makeCaseVariable(cases=list(
            list(expression=ds$gender == "Male", name="Dudes"),
            list(expression='else', name='other'),
            list(expression='else', name='other2')),
            name=""),
            paste("you can only provide a single else case; you have more than",
                "one in either"))
        expect_error(makeCaseVariable(cases=list(
            list(expression=ds$gender == "Male", name="Dudes"),
            list(expression="else", name="else1"),
            list(expression=ds$gender == "Female", name="Not Dudes")), name=""),
            paste("The else case must be the last element of", dQuote("cases"),
                "or", dQuote("...")))
        expect_error(makeCaseVariable(cases=list(
            list(expression=ds$gender == "Male", name="Dudes"),
            list(expression="else", name="name", id=99999999)), name=""),
            paste("id must be less than 32,768, this might be a result of too",
                "many cases being used."))
        expect_error(makeCaseVariable(cases=list(
            list(expression=ds$gender == "Male", name="Dudes"),
            list(expression="else", name="name", id=-10)), name=""),
            "id must not be less than 1")
        expect_error(
            makeCaseVariable(
                cases = list(
                    list(id=1, expression=ds$gender == "Male", name="Dudes"),
                    list(id=1, expression=ds$birthyr < 1950, name="Old women")
                ),
                name="Super clever segmentation"),
            "there are duplicate ids provided: 1 and 1")
        expect_error(
            makeCaseVariable(
                cases = list(
                    list(expression=ds$gender == "Male", name="Dudes"),
                    list(expression=ds$birthyr < 1950, name="Dudes")
                ),
                name="Super clever segmentation"),
            "there are duplicate names provided: Dudes and Dudes")
        expect_error(
            makeCaseVariable(
                cases = list(
                    list(expression=ds$gender == "Male", name="Dudes"),
                    list(expression=ds$gender == "Male", name="Dudes again")
                ),
                name="Super clever segmentation"),
            paste('there are duplicate condition expressions provided:',
                'gender == "Male" and gender == "Male"'))
        expect_error(
            makeCaseVariable(
                cases = list(
                    list(expression=ds$gender == "Male",
                         name="Dudes", missing=NA)
                    ),
                name="Super clever segmentation"),
            "a case's missing must be a logical")
    })
})

with_test_authentication({
    ds <- newDatasetFromFixture("apidocs")
    test_that("makeCaseVariables", {
        ds$catdog <- makeCaseVariable(`Cats`=ds$q1 == "Cat",
                                      `Dogs`=ds$q1 == "Dog",
                                      name="Cats or Dogs",
                                      description = "Describe cats and dogs")
        expect_equal(as.vector(ds$catdog)[1:10],
                     factor(c(NA, "Cats", NA, "Dogs", "Dogs", NA, NA, NA,
                              "Cats", "Dogs"), levels=(c("Cats", "Dogs"))))
        expect_equal(name(ds$catdog), "Cats or Dogs")
        expect_equal(description(ds$catdog), "Describe cats and dogs")
        expect_equal(ids(categories(ds$catdog)), c(1,2,-1))
        expect_equal(names(categories(ds$catdog)), c("Cats", "Dogs", "No Data"))

        ds$catdog2 <- makeCaseVariable(cases=list(
            list(expression=ds$q1 == "Cat", name="Cats"),
            list(expression=ds$q1 == "Dog", name="Dogs"),
            list(expression="else", id=99L, name="Other", missing=FALSE)),
            name="Cats or Dogs2")
        expect_equal(as.vector(ds$catdog2)[1:10],
                     factor(c("Other", "Cats", "Other", "Dogs", "Dogs",
                              "Other", "Other", "Other", "Cats", "Dogs"),
                            levels=(c("Cats", "Dogs", "Other"))))
        expect_equal(ids(categories(ds$catdog2)), c(1,2,99))
        expect_equal(names(categories(ds$catdog2)), c("Cats", "Dogs", "Other"))

        # positive ids can be missing
        ds$catdog3 <- makeCaseVariable(cases=list(
            list(expression=ds$q1 == "Cat", name="Cats"),
            list(id=99L, expression=ds$q1 == "Dog", name="Dogs", missing=TRUE)),
            name="Cats or Dogs3",
            description = "Describe cats and dogs")
        expect_equal(as.vector(ds$catdog3)[1:10],
                     factor(c(NA, "Cats", NA, NA, NA, NA, NA, NA,
                              "Cats", NA), levels=(c("Cats"))))
        expect_equal(description(ds$catdog3), "Describe cats and dogs")
        expect_equal(ids(categories(ds$catdog3)), c(1,99,-1))
        expect_equal(names(categories(ds$catdog3)), c("Cats", "Dogs", "No Data"))
    })
})
