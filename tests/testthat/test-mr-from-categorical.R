context('make MR from categorical variables')

if (run.integration.tests) {
    with(test.authentication, {
        with(test.dataset(newDatasetFromFixture("apidocs")), {
            expect_true('q1' %in% aliases(variables(ds)))
            expect_false('q2' %in% aliases(variables(ds)))
            ds$q2 <- VariableDefinition(factor(rep(c('Lizard', 'Dog', "Don't know", 'Dog'), 5)), name='Pet 2', description='What is your second favorite pet?')
            expect_true('q2' %in% aliases(variables(ds)))
            ds$q3_test <- VariableDefinition(factor(rep(c('Lizard', 'Dog', "Bee", 'Cat', 'Snake'), 4)), name='Pet 3', description='What is your third favorite pet?')
            expect_true('q3_test' %in% aliases(variables(ds)))
            expect_false(is.na(categories(ds$q3_test))[4])
            is.na(categories(ds$q3_test))[names(categories(ds$q3_test)) %in% 'Cat'] <- TRUE
            is.na(categories(ds$q3_test))[names(categories(ds$q3_test)) %in% 'Bee']  <- TRUE
            ds$q1_q2_q3 <- makeMRfromCat(ds$q1, ds$q2, ds$q3_test, name='test')
            expect_false('Bee' %in% names(subvariables(ds$q1_q2_q3)))
            expect_true('Cat' %in% names(subvariables(ds$q1_q2_q3)))
            expect_true('test' %in% names(variables(ds)))
            expect_true('q1_q2_q3' %in% aliases(variables(ds)))
        })
        with(test.dataset(newDatasetFromFixture("apidocs")), {
            expect_true('q1' %in% aliases(variables(ds)))
            expect_false('q2' %in% aliases(variables(ds)))
            ds$q2 <- VariableDefinition(factor(rep(c('Lizard', 'Dog', "Don't know", 'Dog'), 5)), name='Pet 2', description='What is your second favorite pet?')
            expect_true('q2' %in% aliases(variables(ds)))
            ds$q3_test <- VariableDefinition(factor(rep(c('Lizard', 'Dog', "Bee", 'Cat', 'Snake'), 4)), name='Pet 3', description='What is your third favorite pet?')
            expect_true('q3_test' %in% aliases(variables(ds)))
            expect_false(is.na(categories(ds$q3_test))[4])
            is.na(categories(ds$q3_test))[names(categories(ds$q3_test)) %in% 'Cat'] <- TRUE
            is.na(categories(ds$q3_test))[names(categories(ds$q3_test)) %in% 'Bee']  <- TRUE
            ds$q1_q2_q3 <- makeMRfromCat(ds$q1, ds$q2, ds$q3_test, name='test', useNAcats = TRUE)
            expect_true('Bee' %in% names(subvariables(ds$q1_q2_q3)))
            expect_true('Cat' %in% names(subvariables(ds$q1_q2_q3)))
            expect_true('test' %in% names(variables(ds)))
            expect_true('q1_q2_q3' %in% aliases(variables(ds)))
        })
    })
}