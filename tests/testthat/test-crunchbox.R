context("CrunchBox")

test_that("Box size limit check", {
    expect_false(boxTooBig(0, 0))
    expect_false(boxTooBig(1, 0))
    expect_false(boxTooBig(0, 1))
    expect_true(boxTooBig(100, 6))
})

with_mock_HTTP({
    ds <- loadDataset("test ds")
    ds3 <- loadDataset("ECON.sav")

    test_that("Basic box", {
        expect_POST(crunchBox(ds, filters=NULL),
            '/api/datasets/dataset1/boxdata/',
            '{"element":"shoji:entity","body":{"filters":[]}}')
    })
    test_that("Basic box with metadata and filters", {
        expect_POST(crunchBox(ds, title="Test box"),
            '/api/datasets/dataset1/boxdata/',
            '{"element":"shoji:entity","body":{"filters":[',
            '{"filter":"/api/datasets/dataset1/filters/filter1/"},',
            '{"filter":"/api/datasets/dataset1/filters/filter2/"}],',
            '"title":"Test box"}}')
    })
    test_that("Select variables in box", {
        expect_POST(crunchBox(ds[2:4], filters=NULL),
            '/api/datasets/dataset1/boxdata/',
            '{"element":"shoji:entity","body":{"filters":[],',
            '"where":{"function":"select","args":[{"map":{',
            '"66ae9881e3524f7db84970d556c34552":',
            '{"variable":"/api/datasets/dataset1/variables/gender/"},',
            '"949d2dc7e7a24e6090cc88bb92e1d2fb":',
            '{"variable":"/api/datasets/dataset1/variables/mymrset/"},',
            '"text":{"variable":"/api/datasets/dataset1/variables/textVar/"}',
            '}}]}}}')
    })
    test_that("Hidden variables are automatically 'selected' out", {
        expect_POST(crunchBox(ds3),
            '/api/datasets/dataset3/boxdata/',
            '{"element":"shoji:entity","body":{"filters":[],"where":',
            '{"function":"select","args":[{"map":{',
            '"66ae9881e3524f7db84970d556c34552":',
            '{"variable":"/api/datasets/dataset3/variables/gender/"},',
            '"d7c21314ca9e453c93069168681a285c":',
            '{"variable":"/api/datasets/dataset3/variables/starttime/"}}}]}}}')
    })
    test_that("Select filters in box", {
        expect_POST(crunchBox(ds, filters=filters(ds)[c(2, 1)], title="Test box"),
            '/api/datasets/dataset1/boxdata/',
            '{"element":"shoji:entity","body":{"filters":[',
            '{"filter":"/api/datasets/dataset1/filters/filter2/"},',
            '{"filter":"/api/datasets/dataset1/filters/filter1/"}],',
            '"title":"Test box"}}')
        expect_POST(crunchBox(ds, filters=filters(ds)[2]),
            '/api/datasets/dataset1/boxdata/',
            '{"element":"shoji:entity","body":{"filters":[',
            '{"filter":"/api/datasets/dataset1/filters/filter2/"}]}}')
    })
    test_that("Input validation", {
        expect_error(crunchBox(), "'dataset' must be a CrunchDataset")
        expect_error(crunchBox(4), "'dataset' must be a CrunchDataset")
        expect_error(crunchBox(ds, 4),
            "'filters' should be a FilterCatalog or NULL")

    })
    test_that("Box too big message", {
        with_mock(`crunch:::.boxlimit`=function () -1, {
            expect_error(crunchBox(ds[2:5], filters=filters(ds)[1]),
                "4 variables and 1 filter results in too many cubes")
            expect_error(crunchBox(ds[2]),
                "1 variable and 2 filters results in too many cubes")
        })
    })
})
