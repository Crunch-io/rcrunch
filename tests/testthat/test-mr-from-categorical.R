context('make MR from two categorical variables')

if (run.integration.tests) {
    with(test.authentication, {
        with(test.dataset(newDatasetFromFixture("apidocs")), {
            expect_true('q1' %in% aliases(variables(ds)))
            expect_false('q2' %in% aliases(variables(ds)))
            ds$q2 <- VariableDefinition(factor(rep(c('Lizard', 'Cat', "Don't know", 'Dog'), 5)), name='Pet 2', description='What is your second favorite pet?')
            expect_true('q2' %in% aliases(variables(ds)))
            ds$q1_q2_test <- makeMRfromCat(ds$q1, ds$q2, name='test')
            expect_true('q1_q2_test' %in% aliases(variables(ds)))
            expect_false('q1_q2' %in% aliases(variables(ds)))
            expect_true('test' %in% names(variables(ds)))
            expect_true('q1' %in% aliases(variables(ds)))
            expect_true('q2' %in% aliases(variables(ds)))
            expect_equal(names(subvariables(ds$q1_q2_test)), union(names(categories(ds$q1)), names(categories(ds$q2))))
        })
    })
}