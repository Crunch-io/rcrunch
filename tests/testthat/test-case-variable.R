context("Case variables")

with_mock_crunch({
    ds <- loadDataset("test ds")
    test_that("Case variable definition", {
        expect_json_equivalent(
            CaseVariable(
                list(
                    Case(ds$gender == "Male", name="Dudes"),
                    Case(ds$birthyr < 1950, name="Old women")
                ),
                name="Super clever segmentation"),
            list(
                name="Super clever segmentation",
                expr=list(
                    `function`="case",
                    args=list(
                        list(column=I(1:3)),
                        list(type=list(
                            value=list(
                                class="categorical",
                                categories=list(
                                    list()
                                    )
                                )
                            )
                        ),
                        list(
                            `function`="==",
                            args=list(

                            )
                        ), list(
                            `function`="<",
                            args=list(

                            )
                        )
                    )
                )
                ))
    })
})
