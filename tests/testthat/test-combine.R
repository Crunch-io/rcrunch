context("Combine categories")

with_mock_HTTP({
    ds <- loadDataset("test ds")

    both <- VariableDefinition(
        name="Gender 1 cat",
        # description="Gender",
        # discarded=FALSE,
        format=list(summary=list(digits=2)),
        view=list(include_missing=FALSE,
            show_counts=FALSE,
            show_codes=FALSE,
            column_width=NULL
        ),
        expr=list(
            `function`="combine_categories",
            args=list(
                list(variable="/api/datasets/dataset1/variables/gender.json"),
                list(value=list(
                    list(
                        name="Both",
                        combined_ids=I(c(1, 2)),
                        missing=FALSE,
                        numeric_value=NULL,
                        id=1
                    ),
                    list(
                        numeric_value=NULL,
                        missing=TRUE,
                        id=-1,
                        name="No Data",
                        combined_ids=I(-1)
                    )
                ))
            )
        )
    )
    test_that("combine() constructs the correct VarDef for categorical", {
        combine.names <- combine(ds$gender, name="Gender 1 cat",
            list(list(name="Both", categories=c("Male", "Female"))))
        expect_json_equivalent(combine.names, both)
        expect_json_equivalent(combine.names$expr, both$expr)
        combine.ids <- combine(ds$gender, name="Gender 1 cat",
            list(list(name="Both", categories=c(1,2))))
        expect_json_equivalent(combine.ids, both)


        # expect_error(ds$combined_mr <- combine(ds$mymrset, name="MR combined",
        #     list(list(name="Extremes", responses=c("First", "Last")))),
        #     mr.payload, fixed=TRUE)
    })

    test_that("combine() validation", {

    })
})

if (run.integration.tests) {
    with(test.authentication, {
        with(test.dataset(newDatasetFromFixture("apidocs")), {
            test_that("We can create a new categorical by combining", {
                crunch.debug=TRUE), ds$combined_pets <- combine(ds$q1, name="Pets (combined)",
                    list(list(name="Mammals", categories=c("Cat", "Dog"))))
                expect_identical(names(categories(ds$combined_pets)),
                    c("Mammals", "Bird", "Skipped", "Not Asked"))
            })
        })
    })
}
