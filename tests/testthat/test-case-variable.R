context("Case variables")

with_mock_crunch({
    ds <- loadDataset("test ds")
    
    test_that("Case class", {
        case <- Case(name="Dudes", case=ds$gender == "Male")
        expect_equal(name(case), "Dudes")
        name(case) <- "Cool Dudes"
        expect_equal(name(case), "Cool Dudes")
    })

    test_that("Case variable definition", {
        expect_json_equivalent(
            makeCaseVariable(
                list(
                    Case(case=ds$gender == "Male", name="Dudes"),
                    Case(case=ds$birthyr < 1950, name="Old women")
                ),
                name="Super clever segmentation"),
            list(
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
                                list(variable="https://app.crunch.io/api/datasets/1/variables/gender/", value=1)
                            )
                        ), list(
                            `function`="<",
                            args=list(
                                list(variable="https://app.crunch.io/api/datasets/1/variables/birthyr/", value=1950)
                            )
                        )
                    )
                )
                ))
        

    })
})
