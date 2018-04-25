context("Cube queries with on-the-fly expressions")

with_mock_crunch({
    ds <- loadDataset("test ds")
    expect_JSON <- function (object, expected) {
        expect_identical(unclass(toJSON(object)),
            gsub("\n +", "", gsub(": +", ":", expected)))
    }
    test_that("formulaToCubeQuery", {
        expect_JSON(formulaToCubeQuery(~ gender + bin(birthyr), data=ds),
            '{"dimensions":[
                {"variable":"https://app.crunch.io/api/datasets/1/variables/gender/"},
                {
                    "function":"bin",
                    "args":[
                        {"variable":"https://app.crunch.io/api/datasets/1/variables/birthyr/"}
                    ],
                    "references":{
                        "name": "bin(birthyr)",
                        "alias": "bin(birthyr)"
                    }
                }
                ],
            "measures":{"count":{"function":"cube_count","args":[]}}}')
        expect_JSON(formulaToCubeQuery(~ gender + catarray, data=ds),
            '{"dimensions":[
                {"variable":"https://app.crunch.io/api/datasets/1/variables/gender/"},
                {"each":"https://app.crunch.io/api/datasets/1/variables/catarray/"},
                {"variable":"https://app.crunch.io/api/datasets/1/variables/catarray/"}
                ],
            "measures":{"count":{"function":"cube_count","args":[]}}}')
        expect_JSON(formulaToCubeQuery(~ gender + as_selected(mymrset), data=ds),
            '{"dimensions":[
                {"variable":"https://app.crunch.io/api/datasets/1/variables/gender/"},
                {"each":"https://app.crunch.io/api/datasets/1/variables/mymrset/"},
                {"function": "as_selected",
                    "args": [{"variable":"https://app.crunch.io/api/datasets/1/variables/mymrset/"}]}
                ],
            "measures":{"count":{"function":"cube_count","args":[]}}}')
        expect_error(formulaToCubeQuery(~ gender + as_selected(catarray), data=ds),
            paste("Cannot analyze a variable of type",
                dQuote("categorical_array"), "'as_selected'"))
        expect_JSON(formulaToCubeQuery(~ gender + as_array(mymrset), data=ds),
            '{"dimensions":[
                {"variable":"https://app.crunch.io/api/datasets/1/variables/gender/"},
                {"each":"https://app.crunch.io/api/datasets/1/variables/mymrset/"},
                {"variable":"https://app.crunch.io/api/datasets/1/variables/mymrset/"}
                ],
            "measures":{"count":{"function":"cube_count","args":[]}}}')
        expect_JSON(formulaToCubeQuery(~ gender + (birthyr > 1980), data=ds),
            '{"dimensions":[
                {"variable":"https://app.crunch.io/api/datasets/1/variables/gender/"},
                {
                    "function":">",
                    "args":[
                        {"variable": "https://app.crunch.io/api/datasets/1/variables/birthyr/"},
                        {"value": 1980}
                    ],
                    "references":{
                        "name": "birthyr > 1980",
                        "alias": "birthyr > 1980"
                    }}
                ],
            "measures":{"count":{"function":"cube_count","args":[]}}}')
    })

    test_that("Boolean cube dims", {
        ## TODO: delete this when boolean support is removed
        cube <- crtabs(~ birthyr > 1980, data=ds)
        expect_identical(as.array(cube),
            array(c(14, 6),
                dim=2L,
                dimnames=list(`birthyr > 1980`=c("FALSE", "TRUE"))))
    })

    test_that("3VL logical cube dims", {
        ## This is `crtabs(~ q1 != "Cat" & !is.na(q1), data=ds)` from below
        cube <- loadCube("cubes/3vl.json")
        expect_identical(as.array(cube),
            array(c(7, 6),
                dim=2L,
                dimnames=list(`q1 != "Cat" & !is.na(q1)`=c("TRUE", "FALSE"))))
    })
})

with_test_authentication({
    ds <- newDatasetFromFixture("apidocs")
    test_that("<, <= on numeric", {
        # ndogs
        # 0 1 2 3 6
        # 2 3 7 3 1
        expect_equivalent(as.array(crtabs(~ ndogs < 2, data=ds))["TRUE"], 5)
        expect_equivalent(as.array(crtabs(~ ndogs <= 2, data=ds))["TRUE"], 12)
        expect_equivalent(as.array(crtabs(~ ndogs > 2, data=ds))["TRUE"], 4)
        expect_equivalent(as.array(crtabs(~ ndogs >= 2, data=ds))["TRUE"], 11)
        expect_equivalent(as.array(crtabs(~ ndogs > 1 & ndogs <=3, data=ds))["TRUE"], 10)
    })

    test_that("%in% with categorical", {
        # q1
        #  Cat  Dog Bird
        #    6    4    3
        skip("(400) Bad Request: The truth value of an array with more than one element is ambiguous. Use a.any() or a.all()")
        expect_equivalent(as.array(crtabs(~ q1 %in% c("Cat", "Dog"),
            data=ds))["TRUE"], 10)
        expect_equivalent(as.array(crtabs(~ !(q1 %in% c("Cat", "Dog")),
            data=ds))["TRUE"], 3)
    })

    test_that("==, != with categorical", {
        expect_equivalent(as.array(crtabs(~ q1 == "Cat",
            data=ds))["TRUE"], 6)
        expect_equivalent(as.array(crtabs(~ q1 != "Cat" & !is.na(q1),
            data=ds))["TRUE"], 7)
    })
})
