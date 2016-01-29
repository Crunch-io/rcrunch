context('Combining MR variables')

if (run.integration.tests) {
    with(test.authentication, {
        with(test.dataset(newDatasetFromFixture("apidocs")), {
            expect_true('allpets' %in% aliases(variables(ds)))
            expect_false('allpets2' %in% aliases(variables(ds)))
            ds$allpets2 <- VariableDefinition(name='allpets2', type='categorical_array', 
                subvariables=lapply(names(subvariables(ds$allpets)), function(subvar) VarDef(as.vector(ds$allpets[[subvar]]),
                    name=subvar, alias=gsub("pets_", "pets2_", alias(ds$allpets[[subvar]])))))
            dichotomize(ds$allpets2, 'selected')
            ds <- refresh(ds)
            expect_true(type(ds$allpets2) == 'multiple_response')
            name(ds$allpets2[[3]]) <- 'Lizard'
            names(categories(ds$allpets2)) <- c('selected', 'not selected', 'No Data')
            ds$allpets2[['Cat']][1:20] <- c('not selected', 'not selected', 'selected', NA, 'not selected',
                'selected', 'selected', NA, NA, 'selected', NA, 'selected', NA, 'selected', 'not selected',
                NA, NA, NA, 'selected', 'not selected')
            ds <- addVariables(ds, combineMRs(ds$allpets, ds$allpets2))
            expect_true('allpets_allpets2' %in% aliases(variables(ds)))
            expect_identical(names(subvariables(ds$allpets_allpets2)), c('Cat', 'Dog', 'Bird', 'Lizard'))
            expect_identical(as.character(as.vector(ds$allpets_allpets2[['Lizard']])), as.character(as.vector(ds$allpets2[['Lizard']])))
            expect_identical(as.character(as.vector(ds$allpets_allpets2[['Bird']])), as.character(as.vector(ds$allpets[['Bird']])))
            expect_true(sum(as.character(as.vector(ds$allpets[['Cat']])) %in% 'selected' | as.character(as.vector(ds$allpets2[['Cat']])) %in% 'selected') == sum(as.character(as.vector(ds$allpets_allpets2[['Cat']])) %in% 'selected'))
        })
        with(test.dataset(newDatasetFromFixture("apidocs")), {
            expect_true('allpets' %in% aliases(variables(ds)))
            expect_false('allpets2' %in% aliases(variables(ds)))
            ds$allpets2 <- VariableDefinition(name='allpets2', type='categorical_array', 
                subvariables=lapply(names(subvariables(ds$allpets)), function(subvar) VarDef(as.vector(ds$allpets[[subvar]]),
                    name=subvar, alias=gsub("pets_", "pets2_", alias(ds$allpets[[subvar]])))))
            dichotomize(ds$allpets2, 'selected')
            ds <- refresh(ds)
            expect_true(type(ds$allpets2) == 'multiple_response')
            name(ds$allpets2[[3]]) <- 'Lizard'
            names(categories(ds$allpets2)) <- c('selected', 'not selected', 'No Data')
            ds$allpets2[['Cat']][1:20] <- c('not selected', 'not selected', 'selected', NA, 'not selected',
                'selected', 'selected', NA, NA, 'selected', NA, 'selected', NA, 'selected', 'not selected',
                NA, NA, NA, 'selected', 'not selected')
            ds <- addVariables(ds, combineMRs(ds$allpets, ds$allpets2, selectedFirst=FALSE))
            expect_true('allpets_allpets2' %in% aliases(variables(ds)))
            expect_identical(names(subvariables(ds$allpets_allpets2)), c('Cat', 'Dog', 'Bird', 'Lizard'))
            expect_identical(as.character(as.vector(ds$allpets_allpets2[['Lizard']])), as.character(as.vector(ds$allpets2[['Lizard']])))
            expect_identical(as.character(as.vector(ds$allpets_allpets2[['Bird']])), as.character(as.vector(ds$allpets[['Bird']])))
            expect_true(sum(as.character(as.vector(ds$allpets[['Cat']])) %in% 'not selected' | as.character(as.vector(ds$allpets2[['Cat']])) %in% 'not selected') == sum(as.character(as.vector(ds$allpets_allpets2[['Cat']])) %in% 'not selected'))
        })
    })
}