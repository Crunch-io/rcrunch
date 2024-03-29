context("Cube standardized residuals")

# Skip tests on windows (because they're slow and CRAN complains)
if (tolower(Sys.info()[["sysname"]]) != "windows") {

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
        "feeling_ca" = c(
            "Extremely Happy", "Somewhat Happy", "Neutral", "Somewhat Unhappy",
            "Extremely Unhappy"
        )
    )

    catarray_by_cat <- loadCube("cubes/catarray-x-cat.json")
    catarray_by_cat_dims <- list(
        "feeling_ca" = c("cat_feeling", "dog_feeling"),
        "feeling_ca" = c(
            "Extremely Happy", "Somewhat Happy", "Neutral", "Somewhat Unhappy",
            "Extremely Unhappy"
        ),
        "animal" = c("cats", "dog")
    )

    catarray_by_mr <- loadCube("cubes/catarray-x-mr.json")
    catarray_by_mr_dims <- list(
        "feeling_ca" = c("cat_feeling", "dog_feeling"),
        "feeling_ca" = c(
            "Extremely Happy", "Somewhat Happy", "Neutral", "Somewhat Unhappy",
            "Extremely Unhappy"
        ),
        "opinion_mr" = c("food_opinion", "rest_opinion", "play_opinion")
    )


    gender_x_ideology <- loadCube("cubes/econ-gender-x-ideology-weighted.json")
    gender_x_ideology_dims <- dimnames(gender_x_ideology)
    gender_x_ideology_dims <- lapply(gender_x_ideology_dims, function(x) {
        return(x[!x %in% c("Skipped", "Not Asked", "No Data")])
    })


    cat_by_cat <- loadCube("cubes/feelings-pets.json")

    cat_by_mr_NSS_alltypes <- loadCube("cubes/cat-mr-NSS-alltypes.json")


    test_that(paste0(
        "zScores for CrunchCube normal contingency table is chisq standardized ",
        "residuals"
    ), {
        # values from crunch-cube tests
        out <- chisq.test(as.array(gender_x_ideology))$stdres
        expect_equal(zScores(gender_x_ideology), out)
    })

    test_that("zScores returns a CrunchCubeCalculation", {
        zscore <- zScores(mr_by_cat)
        expect_is(zscore, "CrunchCubeCalculation")
        expect_equal(attr(zscore, "type"), "z_score")
        expect_equal(attr(zscore, "dims"), mr_by_cat@dims)
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
        expect_equal(zScores(admit_by_dept_unweighted), out)
    })

    test_that("z-scores for weighted normal crosstab", {
        out <- chisq.test(as.array(admit_by_gender_weighted))$stdres
        expect_equal(zScores(admit_by_gender_weighted), out)
    })

    test_that("rstandard backwards compatibility", {
        expect_equal(
            zScores(admit_by_gender_weighted),
            rstandard(admit_by_gender_weighted)
        )
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
        expect_equal(as.array(rstandard(mr_by_cat)), out)
        expect_equal(as.array(zScores(mr_by_cat)), out)
    })

    test_that("residuals for MR by cat from app", {
        out <- cubify(c(
            0.8013419145312314, -0.8013419145312314, 0.604556055828044,
            -0.6045560558280446, -0.3088424703459705, 0.30884247034596934
        ),
        dims = rev(cat_by_mr_dims)
        )
        expect_equal(as.array(zScores(mr_by_cat_2)), out)
    })
    test_that("residuals for categorical by MR, should be transpose of above", {
        out <- cubify(c(
            0.8013419145312314, 0.604556055828044, -0.3088424703459705,
            -0.8013419145312314, -0.6045560558280446, 0.30884247034596934
        ),
        dims = cat_by_mr_dims
        )
        expect_equal(as.array(zScores(cat_by_mr)), out)
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
        expect_equal(as.array(zScores(mr_by_mr)), out)
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
        expect_equal(as.array(zScores(mr_by_mr_heterogeneous)), out)
    })

    mr_by_mr_by_too_many <- loadCube("cubes/cat-x-mr-x-mr.json")

    test_that("residuals for MR by MR by anything errors", {
        expect_error(zScores(mr_by_mr_by_too_many), paste0(
            "Cannot compute residuals with more than two dimensions. Pick ",
            "a slice to evaluate"
        ))
    })

    test_that("residuals for catarray by cat", {
        expect_error(zScores(catarray_by_cat), paste0(
            "Cannot compute residuals with more than two dimensions. Pick ",
            "a slice to evaluate"
        ))

        # TODO: Implement [.CrunchCube. Then check a slice
    })

    test_that("residuals for catarray", {
        expect_error(zScores(catarray_by_mr), paste0(
            "Cannot compute residuals with more than two dimensions. Pick ",
            "a slice to evaluate"
        ))
        # TODO: Implement [.CrunchCube. Then check a slice
    })

    test_that("compareCols()", {
        expected_zScores <- cubify(
            2.54925480834223,
            -2.54925480834223,
            dims = list(
                Gender = c("Male", "Female"),
                RespondentIdeology = c("Very Conservative")
            )
        )
        expect_equal(
            compareCols(
                gender_x_ideology,
                baseline = "Very liberal",
                x = "Very Conservative"
            ),
            expected_zScores
        )

        expected_zScores <- cubify(
            -2.54925480834223,
            2.54925480834223,
            dims = list(
                Gender = c("Male", "Female"),
                RespondentIdeology = c("Very liberal")
            )
        )
        expect_equal(
            compareCols(
                gender_x_ideology,
                baseline = "Very Conservative",
                x = "Very liberal"
            ),
            expected_zScores
        )
    })

    test_that("compareRows()", {
        cat_by_cat <- noTransforms(cat_by_cat)

        expected_zScores <- cubify(
            -0.97433731917929, 0.97433731917929,
            dims = list(
                feelings = c("extremely unhappy"),
                animals = c("cats", "dogs")
            )
        )
        expect_equal(
            compareRows(
                cat_by_cat,
                baseline = "extremely happy",
                x = "extremely unhappy"
            ),
            expected_zScores
        )

        expected_zScores <- cubify(
            0.97433731917929, -0.97433731917929,
            dims = list(
                feelings = c("extremely happy"),
                animals = c("cats", "dogs")
            )
        )
        expect_equal(
            compareRows(
                cat_by_cat,
                baseline = "extremely unhappy",
                x = "extremely happy"
            ),
            expected_zScores
        )
    })

    test_that("compareDims() dimension validation", {
        cat_by_cat <- noTransforms(cat_by_cat)

        expect_error(
            compareDims(
                cat_by_cat,
                baseline = "foo",
                x = "extremely unhappy",
                dim = "rows"
            ),
            "foo is not a column or row in the cube"
        )

        expect_error(
            compareDims(
                cat_by_cat,
                baseline = "extremely happy",
                x = "foo",
                dim = "rows"
            ),
            "foo is not a column or row in the cube"
        )

        expect_error(
            compareDims(
                cat_by_cat,
                baseline = 1,
                x = "extremely happy",
                dim = "rows"
            ),
            "Currently, column comparison only accepts at most one category name."
        )

        expect_error(
            compareDims(
                cat_by_cat,
                baseline = c("extremely happy", "somewhat happy"),
                x = "extremely happy",
                dim = "rows"
            ),
            "Currently, column comparison only accepts at most one category name."
        )

        expect_error(
            compareDims(
                cat_by_cat,
                baseline = "extremely happy",
                x = 1,
                dim = "rows"
            ),
            "Currently, column comparison only accepts at most one category name."
        )

        expect_error(
            compareDims(
                cat_by_cat,
                baseline = "extremely happy",
                x = c("extremely happy", "somewhat happy"),
                dim = "rows"
            ),
            "Currently, column comparison only accepts at most one category name."
        )
    })

    test_that("compareDims() with MRs", {
        # if the dimension you are trying to compare amongst is an MR you get an
        # error (for now)
        expect_error(
            compareCols(
                cat_by_mr_NSS_alltypes,
                baseline = "Denmark",
                x = "Sweden"
            ),
            paste0(
                "Column or row z-scores are not implemented for multiple ",
                "response dimensions"
            )
        )

        # But if the MR is not the dimension being compared amongst, we still
        # calculate a score
        expected_zScores <- cubify(
            -1.34840705967846,
            -0.319502930145056,
            -2.44219465036739,
            -3.14883276639645,
            4.11744429667266,
            dims = list(
                food_groups = c("Vegetables"),
                nordics = c("Denmark", "Finland", "Iceland", "Norway", "Sweden")
            )
        )

        expect_equal(
            compareRows(cat_by_mr_NSS_alltypes, baseline = "Fruit", x = "Vegetables"),
            expected_zScores
        )

        expect_error(
            compareRows(
                mr_by_cat,
                baseline = "Cupcakes are the best cakes",
                x = "I always ride a penny-farthing"
            ),
            paste0(
                "Column or row z-scores are not implemented for multiple ",
                "response dimensions"
            )
        )
    })



    test_that("compareColsPairwise()", {
        expected_zScores <- t(cubify(
            compareCols(gender_x_ideology, baseline = "Moderate", x = "Very liberal"),
            compareCols(gender_x_ideology, baseline = "Moderate", x = "Liberal"),
            0, 0,
            compareCols(gender_x_ideology, baseline = "Moderate", x = "Conservative"),
            compareCols(gender_x_ideology, baseline = "Moderate", x = "Very Conservative"),
            compareCols(gender_x_ideology, baseline = "Moderate", x = "Not sure"),
            dims = list(
                RespondentIdeology = c(
                    "Very liberal", "Liberal", "Moderate", "Conservative",
                    "Very Conservative", "Not sure"
                ),
                Gender = c("Male", "Female")
            )
        ))
        expect_equal(
            compareColsPairwise(
                gender_x_ideology,
                baseline = "Moderate"
            ),
            expected_zScores
        )
    })

    test_that("compareRowsPairwise()", {
        cat_by_cat <- noTransforms(cat_by_cat)

        expected_zScores <- cubify(
            compareRows(cat_by_cat, baseline = "neutral", x = "extremely happy"),
            compareRows(cat_by_cat, baseline = "neutral", x = "somewhat happy"),
            0, 0,
            compareRows(cat_by_cat, baseline = "neutral", x = "somewhat unhappy"),
            compareRows(cat_by_cat, baseline = "neutral", x = "extremely unhappy"),
            dims = list(
                feelings = c(
                    "extremely happy", "somewhat happy", "neutral",
                    "somewhat unhappy", "extremely unhappy"
                ),
                animals = c("cats", "dogs")
            )
        )
        expect_equal(
            compareRowsPairwise(cat_by_cat, baseline = "neutral"),
            expected_zScores
        )
    })
}
