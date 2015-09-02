context("Combine categories and responses")

with(fake.HTTP, {
    ds <- loadDataset("test ds")
    
    cat.payload <- 'Error: POST /api/datasets/variables.json {"name":"Gender 1 cat",}\n'
    mr.payload <- 'Error: POST /api/datasets/variables.json {"name":"MR combined",}\n'
    expect_error(ds$combined_cat <- combine(ds$gender, name="Gender 1 cat",
        list(list(name="Both", categories=c("Male", "Female")))),
        cat.payload, fixed=TRUE)
    expect_error(ds$combined_cat <- combine(ds$gender, name="Gender 1 cat",
        list(list(name="Both", categories=c(1,2)))),
        cat.payload, fixed=TRUE)
        
    expect_error(ds$combined_mr <- combine(ds$mymrset, name="MR combined", 
        list(list(name="Extremes", responses=c("First", "Last")))),
        mr.payload, fixed=TRUE)

})

if (run.integration.tests) {
    with(test.authentication, {
        with(test.dataset(newDatasetFromFixture("apidocs")), {
            ds$combined_pets <- combine(ds$q1, name="Pets (combined)", 
                list(list(name="Mammals", categories=c("Cat", "Dog"))))
            expect_identical(names(categories(ds$combined_pets)), 
                c("Mammals", "Bird", "Skipped", "Not Asked"))
        })
    })
}