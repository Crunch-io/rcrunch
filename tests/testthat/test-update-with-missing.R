context("Update variables with NAs")

with_mock_crunch({
    ds <- loadDataset("test ds")
    ds2 <- loadDataset("an archived dataset", kind = "archived")

    test_that("If an array gets NA assigned and it doesn't have No Data, we add that category", {
        expect_PATCH(
            ds2$mymrset[3] <- NA,
            "https://app.crunch.io/api/datasets/2/variables/mymrset/",
            '{"categories":'
        )
    })
    test_that("If an subvariables gets NA assigned and it doesn't have No Data, we add that category to its parent", {
        expect_PATCH(
            ds2$mymrset[[1]][3] <- NA,
            "https://app.crunch.io/api/datasets/2/variables/mymrset/",
            '{"categories":'
        )
    })
    test_that("is.na<- sends an expression", {
        expect_POST(
            is.na(ds$birthyr) <- ds$birthyr > 2016,
            "https://app.crunch.io/api/datasets/1/table/", '{"command":"update","variables":',
            '{"https://app.crunch.io/api/datasets/1/variables/birthyr/":{"value":null,',
            '"type":{"class":"numeric"}}},"filter":{"function":">","args":',
            '[{"variable":"https://app.crunch.io/api/datasets/1/variables/birthyr/"},',
            '{"value":2016}]}}'
        )
    })
})

with_test_authentication({
    ds <- newDatasetFromFixture("apidocs")
    test_that("Insert NA into numeric", {
        expect_equivalent(
            as.vector(ds$ndogs[1:5]),
            c(1, NA, 2, 3, 1)
        )
        ds$ndogs[ds$ndogs > 2] <- NA
        expect_equivalent(
            as.vector(ds$ndogs[1:5]),
            c(1, NA, 2, NA, 1)
        )
    })
    test_that("Insert NA into categorical", {
        expect_equivalent(
            as.character(as.vector(ds$q1[1:5])),
            c(NA, "Cat", NA, "Dog", "Dog")
        )
        expect_equivalent(
            as.vector(ds$q1[1:5], mode = "id"),
            c(8, 1, 9, 2, 2)
        )
        ds$q1[4] <- NA
        expect_equivalent(
            as.character(as.vector(ds$q1[1:5])),
            c(NA, "Cat", NA, NA, "Dog")
        )
        expect_equivalent(
            as.vector(ds$q1[1:5], mode = "id"),
            c(8, 1, 9, -1, 2)
        )
        expect_true(-1 %in% ids(categories(ds$q1)))
    })
    test_that("Insert NA into datetime", {
        expect_equivalent(
            as.vector(ds$wave[1:5]),
            rep(as.Date("2014-12-01"), 5)
        )
        ds$wave[4] <- NA
        expect_equivalent(
            as.vector(ds$wave[1:5]),
            c(rep(as.Date("2014-12-01"), 3), NA, as.Date("2014-12-01"))
        )
    })
    test_that("Insert NA into text", {
        expect_equivalent(
            as.vector(ds$q3[1:3]),
            c("Jasmine", "Clyde", "Geoffrey")
        )
        ds$q3[2] <- NA
        expect_equivalent(
            as.vector(ds$q3[1:3]),
            c("Jasmine", NA, "Geoffrey")
        )
    })
    test_that("Insert NA into multiple response", {
        expect_equivalent(
            as.vector(ds$allpets$allpets_1, mode = "id")[1:5],
            c(1, 9, 1, 1, 9)
        )
        expect_equivalent(
            as.vector(ds$allpets$allpets_2, mode = "id")[1:5],
            c(8, 1, 9, 9, 9)
        )
        expect_equivalent(
            as.vector(ds$allpets$allpets_3, mode = "id")[1:5],
            c(8, 2, 8, 8, 8)
        )
        ds$allpets[2] <- NA
        expect_equivalent(
            as.vector(ds$allpets$allpets_1, mode = "id")[1:5],
            c(1, -1, 1, 1, 9)
        )
        expect_equivalent(
            as.vector(ds$allpets$allpets_2, mode = "id")[1:5],
            c(8, -1, 9, 9, 9)
        )
        expect_equivalent(
            as.vector(ds$allpets$allpets_3, mode = "id")[1:5],
            c(8, -1, 8, 8, 8)
        )
    })
    test_that("Insert NA into categorical array", {
        expect_equivalent(
            as.vector(ds$petloc$petloc_home, mode = "id")[1:5],
            c(8, 2, 9, 9, 1)
        )
        expect_equivalent(
            as.vector(ds$petloc$petloc_work, mode = "id")[1:5],
            c(9, 3, 3, 2, 2)
        )
        ds$petloc[3] <- NA
        expect_equivalent(
            as.vector(ds$petloc$petloc_home, mode = "id")[1:5],
            c(8, 2, -1, 9, 1)
        )
        expect_equivalent(
            as.vector(ds$petloc$petloc_work, mode = "id")[1:5],
            c(9, 3, -1, 2, 2)
        )
    })

    # skip("Transitioning to an API that does this for us")
    # test_that("If No Data isn't a category, it is added automatically to categorical", {
    #     ## Create a clean categorical
    #     ds$cat <- VarDef(
    #         values = rep(1, nrow(ds)),
    #         type = "categorical",
    #         categories = list(
    #             list(id = 1L, name = "B", numeric_value = 1L, missing = FALSE)
    #         ),
    #         name = "All B"
    #     )
    #     expect_equal(ids(categories(ds$cat)), 1)
    #     ds$cat[5] <- NA
    #     expect_equal(ids(categories(ds$cat)), c(1, -1))
    #     expect_equal(as.vector(ds$cat[3:7], mode = "id"), c(1, 1, -1, 1, 1))
    # })
    # test_that("If No Data isn't a category, it is added automatically to array", {
    #     ## Set up for next tests. Deep copy array, purge its missings,
    #     ## remove No Data category.
    #     generate_categorical <- function() {
    #         v <- VariableDefinition(factor(sample(c("cat", "dog"), 20, replace = TRUE)))
    #         v$categories <- v$categories[1:2]
    #         return(v)
    #     }
    #     ds$sub1 <- generate_categorical()
    #     ds$sub2 <- generate_categorical()
    #     ds$array <- makeArray(ds[, c("sub1", "sub2")], name = "array")
    #     expect_equal(ids(categories(ds$array)), c(1, 2))
    #     ds$array[[1]][5] <- NA
    #     expect_equal(ids(categories(ds$array)), c(1, 2, -1))
    #     expect_equal(as.vector(ds$array[[1]][5], mode = "id"), -1)
    # })

    ## Roll it back, do some more updating
    ds <- restoreVersion(ds, 1)
    test_that("Insert values including NA into numeric", {
        expect_equivalent(
            as.vector(ds$ndogs[1:5]),
            c(1, NA, 2, 3, 1)
        )
        ds$ndogs[2:4] <- c(1, NA, 2)
        expect_equivalent(
            as.vector(ds$ndogs[1:5]),
            c(1, 1, NA, 2, 1)
        )
    })
    test_that("Insert values including NA into categorical", {
        expect_equivalent(
            as.character(as.vector(ds$q1[1:5])),
            c(NA, "Cat", NA, "Dog", "Dog")
        )
        ds$q1[2:4] <- c(NA, "Cat", "Cat")
        expect_equivalent(
            as.character(as.vector(ds$q1[1:5])),
            c(NA, NA, "Cat", "Cat", "Dog")
        )
    })
    test_that("Insert values including NA into datetime", {
        expect_equivalent(
            as.vector(ds$wave[1:5]),
            rep(as.Date("2014-12-01"), 5)
        )
        ds$wave[2:4] <- as.Date(c("2014-12-15", NA, "2014-11-01"))
        expect_equivalent(
            as.Date(as.vector(ds$wave[1:5])), ## it's POSIXt
            as.Date(c("2014-12-01", "2014-12-15", NA, "2014-11-01", "2014-12-01"))
        )
    })
    test_that("Insert values including NA into text", {
        expect_equivalent(
            as.vector(ds$q3[1:3]),
            c("Jasmine", "Clyde", "Geoffrey")
        )
        ds$q3[2:3] <- c(NA, "Jeff")
        expect_equivalent(
            as.vector(ds$q3[1:3]),
            c("Jasmine", NA, "Jeff")
        )
    })
    test_that("Insert values including NA into multiple response", {

    })
    test_that("Insert values including NA into categorical array", {

    })
})
