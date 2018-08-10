context("Cube standardized residuals")

##########################################
## fixutres from crunch cube
##########################################
cat_by_mr <- loadCube("cubes/selected-crosstab-array-last.json")
cat_by_mr_dims <- list(
    fruit = c("rambutan", "satsuma"),
    zoo = c("alligator", "oryx", "capybara")
)
mr_by_cat <- loadCube("cubes/selected-crosstab-4.json")
mr_by_cat_dims <- list(
    shower_thoughts_klima_2 = c(
        "Cupcakes are the best cakes",
        "Corgis are the future of dog shows",
        "I always ride a penny-farthing",
        "I never look at eclipses",
        "I never mess with Texas",
        "I don't mind pickles on my burger"
    ),
    pdl_gender = c("Male", "Female")
)
mr_by_mr <- loadCube("cubes/selected-by-selected.json")
mr_by_mr_dims <- list(
    zoo = c("alligator", "oryx", "capybara", "Any"),
    zoo = c("alligator", "oryx", "capybara", "Any")
)

mr_by_mr_heterogeneous <- loadCube("cubes/mr-by-mr-different-mrs.json")
mr_by_mr_heterogeneous_dims <- list(
    opinion_mr = c("food_opinion", "rest_opinion", "play_opinion"),
    feeling_mr = c("cat_feeling", "dog_feeling")
)

catarray <- loadCube("cubes/cat-array.json")
catarray_dims <- list(
    "feeling_ca" = c("cat_feeling", "dog_feeling"),
    "feeling_ca" = c("Extremely Happy", "Somewhat Happy", "Neutral", "Somewhat Unhappy", "Extremely Unhappy")
)

catarray_by_cat <- loadCube("cubes/catarray-x-cat.json")
catarray_by_cat_dims <- list(
    "feeling_ca" = c("cat_feeling", "dog_feeling"),
    "feeling_ca" = c("Extremely Happy", "Somewhat Happy", "Neutral", "Somewhat Unhappy", "Extremely Unhappy"),
    "animal" = c("cats", "dog")
)

catarray_by_mr <- loadCube("cubes/catarray-x-mr.json")
catarray_by_mr_dims <- list(
    "feeling_ca" = c("cat_feeling", "dog_feeling"),
    "feeling_ca" = c("Extremely Happy", "Somewhat Happy", "Neutral", "Somewhat Unhappy", "Extremely Unhappy"),
    "opinion_mr" = c("food_opinion", "rest_opinion", "play_opinion")
)


gender_x_ideology <- loadCube("cubes/econ-gender-x-ideology-weighted.json")

test_that("rstandard for CrunchCube normal contingency table is chisq standardized residuals", {
    # values from crunch-cube tests
    out <- chisq.test(as.array(gender_x_ideology))$stdres
    expect_equal(rstandard(gender_x_ideology), out)
})

##########################################
## fixutres from whaam
##########################################
# whaam fixtures
admit_by_dept_unweighted <- loadCube("cubes/admit-by-dept-unweighted.json")
admit_by_gender_weighted <- loadCube("cubes/admit-by-gender-weighted.json")
mr_by_cat_2 <- loadCube("cubes/selected-crosstab-array-first.json")

test_that("z-scores for unweighted normal crosstab", {
    out <- chisq.test(as.array(admit_by_dept_unweighted))$stdres
    expect_equal(rstandard(admit_by_dept_unweighted), out)
})

test_that("z-scores for weighted normal crosstab", {
    out <- chisq.test(as.array(admit_by_gender_weighted))$stdres
    expect_equal(rstandard(admit_by_gender_weighted), out)
})

## multiple response fun times!
test_that("residuals for MR by categorical unweighted", {
    out <- cubify(c(
        -10.883178882792022, 10.883178882792022,
        5.2357732555062135, -5.235773255506258,
        -8.890892610769924, 8.89089261076998,
        -7.313679316034684, 7.313679316034614,
        15.393601979290077, -15.393601979290064,
        -1.152196479675476, 1.152196479675452
    ),
    dims = mr_by_cat_dims
    )
    expect_equal(rstandard(mr_by_cat), out)
})

test_that("residuals for MR by cat from app", {
    out <- cubify(c(
        0.8013419145312314, -0.8013419145312314, 0.604556055828044,
        -0.6045560558280446, -0.3088424703459705, 0.30884247034596934
    ),
    dims = rev(cat_by_mr_dims)
    )
    expect_equal(rstandard(mr_by_cat_2), out)
})
test_that("residuals for categorical by MR, should be transpose of above", {
    out <- cubify(c(
        0.8013419145312314, 0.604556055828044, -0.3088424703459705,
        -0.8013419145312314, -0.6045560558280446, 0.30884247034596934
    ),
    dims = cat_by_mr_dims
    )
    expect_equal(rstandard(cat_by_mr), out)
})


test_that("residuals for MR by MR", {
    out <- cubify(c(
        12.884183726525881, 0.17813020167542695, -1.2190175787279611, 4.156824868149565,
        0.17813020167542695, 11.910821998732818, -2.700337815839828, 5.694768173489977,
        -1.219017578727962, -2.700337815839828, 13.453386657983003, 9.2929498417063,
        4.156824868149565, 5.694768173489977, 9.2929498417063, 15.379818568557937
    ),
    dims = mr_by_mr_dims
    )
    expect_equal(rstandard(mr_by_mr), out)
})

test_that("residuals for MR by MR (disparate MRs)", {
    out <- cubify(
        c(
            -0.172383926892779, 38.5164653199407,
            0.102711744395378, -39.1122969318395,
            -0.26443563922932, -39.6750394717687
        ),
        dims = mr_by_mr_heterogeneous_dims
    )
    expect_equal(rstandard(mr_by_mr_heterogeneous), out)
})

mr_by_mr_by_too_many <- loadCube("cubes/cat-x-mr-x-mr.json")

test_that("residuals for MR by MR by anything errors", {
    expect_error(rstandard(mr_by_mr_by_too_many), paste0(
        "Cannot compute residuals with more than two dimensions. Pick ",
        "a slice to evaluate"
    ))
})

test_that("residuals for catarray by cat", {
    expect_error(rstandard(catarray_by_cat), paste0(
        "Cannot compute residuals with more than two dimensions. Pick ",
        "a slice to evaluate"
    ))
    # TODO: Implement [.CrunchCube. Then check a slice
})

test_that("residuals for catarray", {
    expect_error(rstandard(catarray_by_mr), paste0(
        "Cannot compute residuals with more than two dimensions. Pick ",
        "a slice to evaluate"
    ))
    # TODO: Implement [.CrunchCube. Then check a slice
})
