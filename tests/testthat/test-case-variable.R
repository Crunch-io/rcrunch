context("Case variables")

with_mock_crunch({
    ds <- loadDataset("test ds")
    
    test_that("Case class", {
        case <- Case(name="Dudes", case=ds$gender == "Male")
        expect_equal(name(case), "Dudes")
        name(case) <- "Cool Dudes"
        expect_equal(name(case), "Cool Dudes")
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
                list(
                    Case(case=ds$gender == "Male", name="Dudes"),
                    Case(case=ds$birthyr < 1950, name="Old women")
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
                list(
                    Case(id=10L, case=ds$gender == "Male", name="Dudes", numeric_value=0),
                    Case(id=11L, case=ds$birthyr < 1950, name="Old women", missing=TRUE)
                ),
                name="Super clever segmentation"),
            case_output)
    })
    
    test_that("makeCaseVariable errors gracefully", {
        expect_error(makeCaseVariable(cases = list(
            Case(case=ds$gender == "Male", name="Dudes"),
            "not a Case object"), name = "foo"),
            "All elements of the cases arugment must be of class Case")
    })
})

with_test_authentication({
    ds <- newDatasetFromFixture("apidocs")
    ds$catdog <- makeCaseVariable(list(Case(case = ds$q1 == "Cat", name="Cats"),
                                       Case(case = ds$q1 == "Dog", name="Dogs")),
                                  name = "Cats or Dogs")
    expect_equal(as.vector(ds$catdog)[1:10],
                 factor(c(NA, "Cats", NA, "Dogs", "Dogs", NA, NA, NA,
                          "Cats", "Dogs"), levels = (c("Cats", "Dogs"))))
    expect_equal(name(ds$catdog), "Cats or Dogs")
    expect_equal(ids(categories(ds$catdog)), c(1,2,-1))
    expect_equal(names(categories(ds$catdog)), c("Cats", "Dogs", "No Data"))
})
