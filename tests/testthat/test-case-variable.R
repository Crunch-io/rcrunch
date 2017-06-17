context("Case variables")

with_mock_crunch({
    ds <- loadDataset("test ds")
    
    test_that("Case list validator", {
        case <- list(name="Dudes", expression=ds$gender == "Male")
        case_out <- list(name="Dudes", expression=ds$gender == "Male", numeric_value=NULL, missing=FALSE)
        expect_equal(ensureValidCase(case), case_out)
        expect_error(ensureValidCase("case"), "A case must be a list")
        expect_error(ensureValidCase(list()),
                     "a case's name must be a character")
        expect_error(ensureValidCase(list(name="name")),
                     "a case's expression must be a CrunchLogicalExpr")
        expect_error(ensureValidCase(list(name="name", expression=CrunchLogicalExpr(), id=0.8)),
                     "a case's id must be an integer")
        expect_error(ensureValidCase(list(name="name", expression=CrunchLogicalExpr(), numeric_value="nope")),
                     "a case's numeric_value must be a numeric")
        expect_error(ensureValidCase(list(name="name", expression=CrunchLogicalExpr(), missing="nope")),
                     "a case's missing must be a logical")
        expect_error(ensureValidCase(list(not_right="not")),
                     "each case must have at most an id, name, expression, numeric_value, and missing element. The errant arguments were: not_right")
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
                                 list(id=1, name="Dudes", numeric_value=NULL, missing=FALSE),
                                 list(id=2, name="Old women", numeric_value=NULL, missing=FALSE)
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
                    list(id=10L, expression=ds$gender == "Male", name="Dudes", numeric_value=0),
                    list(id=11L, expression=ds$birthyr < 1950, name="Old women", missing=TRUE)
                ),
                name="Super clever segmentation"),
            case_output)
    })
    test_that("makeCaseVariable works with an else pre-specified", {
        case_output$derivation$args[[1]]$column[[3]] <- 3L
        case_output$derivation$args[[1]]$type$value$categories[[3]] <- list(
            id=3L, name="Other", numeric_value=NULL, missing=FALSE)
        expect_json_equivalent(
            makeCaseVariable(
                cases = list(
                    list(expression=ds$gender == "Male", name="Dudes"),
                    list(expression=ds$birthyr < 1950, name="Old women")
                ),
                else_case = list(name="Other", id=3L),
                name="Super clever segmentation"),
            case_output)
    })
    
    test_that("makeCaseVariable errors gracefully", {
        expect_error(makeCaseVariable(cases=list(
            list(expression=ds$gender == "Male", name="Dudes"))),
            'argument "name" is missing, with no default')
        expect_error(makeCaseVariable(`Old women`=ds$birthyr < 1950, cases=list(
            list(expression=ds$gender == "Male", name="Dudes")), name=""),
            "can't have case conditions both in ... as well as in the cases argument, please use one or the other.")
        expect_error(makeCaseVariable(name="Dudes"),
            "must supply case conditions in either ... or the cases argument, please use one or the other.")
        expect_error(makeCaseVariable(cases=list(
            list(expression=ds$gender == "Male", name="Dudes")),
            else_case = list(expression=ds$gender == "Female", name="Female"), name=""),
            "else_cases should not have any conditions expression")
        expect_error(makeCaseVariable(cases=list(
            list(expression=ds$gender == "Male", name="Dudes")),
            else_case = list(id=1L), name=""),
            "else_cases must have a \\(character\\) name")
        expect_error(makeCaseVariable(cases=list(
            list(expression=ds$gender == "Male", name="Dudes")),
            else_case = list(name="name", id=0.8), name=""),
            "id must be an integer")
        expect_error(
            makeCaseVariable(
                cases = list(
                    list(id=1L, expression=ds$gender == "Male", name="Dudes"),
                    list(id=1L, expression=ds$birthyr < 1950, name="Old women")
                ),
                name="Super clever segmentation"),
            "there are duplicate ids provided: 1 and 1")
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
        
        ds$catdog2 <- makeCaseVariable(cases=list(list(expression=ds$q1 == "Cat", name="Cats"),
                                                 list(expression=ds$q1 == "Dog", name="Dogs")),
                                       else_case=list(id=99L, name="Other", missing=FALSE),
                                      name="Cats or Dogs2")
        expect_equal(as.vector(ds$catdog2)[1:10],
                     factor(c("Other", "Cats", "Other", "Dogs", "Dogs", "Other", "Other", "Other",
                              "Cats", "Dogs"), levels=(c("Cats", "Dogs", "Other"))))
        expect_equal(ids(categories(ds$catdog2)), c(1,2,99))
        expect_equal(names(categories(ds$catdog2)), c("Cats", "Dogs", "Other"))
        
        # positive ids can be missing
        ds$catdog3 <- makeCaseVariable(cases=list(list(expression=ds$q1 == "Cat", name="Cats"),
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
