context("CrunchBox")

test_that("Box size limit check", {
    expect_false(boxTooBig(0, 0))
    expect_false(boxTooBig(1, 0))
    expect_false(boxTooBig(0, 1))
    expect_true(boxTooBig(100, 6))
})

test_that("Embed URL", {
    expect_identical(boxdataToWidgetURL("http://cf.example/d/stuff/1a1577c91fbb2c1cbd3800e181188508/dataset.json"),
        "//s.crunch.io/widget/index.html#/ds/1a1577c91fbb2c1cbd3800e181188508/")
    expect_identical(boxdataToWidgetURL("//s.crunch.io/widget/index.html#/ds/1a1577c91fbb2c1cbd3800e181188508/"),
        "//s.crunch.io/widget/index.html#/ds/1a1577c91fbb2c1cbd3800e181188508/")
})

test_that("Iframe code (prints and returns invisibly)", {
    ## Use the testthat version of expect_output because the crunch version
    ## calls print explicitly
    testthat::expect_output(
        expect_identical(embedCrunchBox("http://cf.example/d/stuff/1a1577c91fbb2c1cbd3800e181188508/dataset.json"),
            '<iframe src="//s.crunch.io/widget/index.html#/ds/1a1577c91fbb2c1cbd3800e181188508/" width="600" height="480" style="border: 1px solid #d3d3d3;"></iframe>'),
        '<iframe src="//s.crunch.io/widget/index.html#/ds/1a1577c91fbb2c1cbd3800e181188508/" width="600" height="480" style="border: 1px solid #d3d3d3;"></iframe>',
        fixed=TRUE)
})

iframe_with_logo <- '<figure style="text-align: left;" class="content-list-component image">
    <img src="//s.crunch.io/public/branding/example.gif" style="height:auto; width:200px; margin-left:-4px"></img>
    <iframe src="//s.crunch.io/widget/index.html#/ds/1a1577c91fbb2c1cbd3800e181188508/" width="600" height="480" style="border: 1px solid #d3d3d3;"></iframe>
</figure>'

iframe_with_title <- '<figure style="text-align: left;" class="content-list-component image">
    <div style="padding-bottom: 12px">
        <span style="font-size: 18px; color: #444444; line-height: 1;">Example title here</span>
    </div>
    <iframe src="//s.crunch.io/widget/index.html#/ds/1a1577c91fbb2c1cbd3800e181188508/" width="600" height="480" style="border: 1px solid #d3d3d3;"></iframe>
</figure>'


test_that("Iframe code with logo", {
    testthat::expect_output(
        expect_identical(embedCrunchBox("http://cf.example/d/stuff/1a1577c91fbb2c1cbd3800e181188508/dataset.json", logo="//s.crunch.io/public/branding/example.gif"),
            iframe_with_logo),
        iframe_with_logo,
        fixed=TRUE)
})

test_that("Iframe code with title", {
    testthat::expect_output(
        expect_identical(embedCrunchBox("http://cf.example/d/stuff/1a1577c91fbb2c1cbd3800e181188508/dataset.json", title="Example title here"),
            iframe_with_title),
        iframe_with_title,
        fixed=TRUE)
})

with_mock_crunch({
    ds <- loadDataset("test ds")
    ds3 <- loadDataset("ECON.sav")

    test_that("preCrunchBoxCheck does not error", {
        expect_output(preCrunchBoxCheck(ds),
            "We recommend using only categorical and multiple_response variables. These 4 variables have an unsupported type")
    })

    test_that("Basic box", {
        expect_POST(crunchBox(ds, filters=NULL),
            'https://app.crunch.io/api/datasets/1/boxdata/',
            '{"element":"shoji:entity","body":{"filters":[]}}')
    })
    test_that("Basic box with metadata and filters", {
        expect_POST(crunchBox(ds, title="Test box"),
            'https://app.crunch.io/api/datasets/1/boxdata/',
            '{"element":"shoji:entity","body":{"filters":[',
            '{"filter":"https://app.crunch.io/api/datasets/1/filters/filter1/"},',
            '{"filter":"https://app.crunch.io/api/datasets/1/filters/filter2/"}],',
            '"title":"Test box"}}')
    })
    test_that("Select variables in box", {
        expect_POST(crunchBox(ds[2:5], filters=NULL),
            'https://app.crunch.io/api/datasets/1/boxdata/',
            '{"element":"shoji:entity","body":{"filters":[],',
            '"where":{"function":"select","args":[{"map":{',
            '"66ae9881e3524f7db84970d556c34552":',
            '{"variable":"https://app.crunch.io/api/datasets/1/variables/gender/"},',
            '"loc":',
            '{"variable":"https://app.crunch.io/api/datasets/1/variables/location/"},',
            '"949d2dc7e7a24e6090cc88bb92e1d2fb":',
            '{"variable":"https://app.crunch.io/api/datasets/1/variables/mymrset/"},',
            '"text":{"variable":"https://app.crunch.io/api/datasets/1/variables/textVar/"}',
            '}}]}}}')
    })
    test_that("Hidden variables are automatically 'selected' out", {
        expect_POST(crunchBox(ds3),
            'https://app.crunch.io/api/datasets/3/boxdata/',
            '{"element":"shoji:entity","body":{"filters":[],"where":',
            '{"function":"select","args":[{"map":{',
            '"66ae9881e3524f7db84970d556c34552":',
            '{"variable":"https://app.crunch.io/api/datasets/3/variables/gender/"},',
            '"d7c21314ca9e453c93069168681a285c":',
            '{"variable":"https://app.crunch.io/api/datasets/3/variables/starttime/"}}}]}}}')
    })
    test_that("Select filters in box", {
        expect_POST(crunchBox(ds, filters=filters(ds)[c(2, 1)], title="Test box"),
            'https://app.crunch.io/api/datasets/1/boxdata/',
            '{"element":"shoji:entity","body":{"filters":[',
            '{"filter":"https://app.crunch.io/api/datasets/1/filters/filter2/"},',
            '{"filter":"https://app.crunch.io/api/datasets/1/filters/filter1/"}],',
            '"title":"Test box"}}')
        expect_POST(crunchBox(ds, filters=filters(ds)[2]),
            'https://app.crunch.io/api/datasets/1/boxdata/',
            '{"element":"shoji:entity","body":{"filters":[',
            '{"filter":"https://app.crunch.io/api/datasets/1/filters/filter2/"}]}}')
    })
    test_that("Boxes can have color palletes specified", {
              expect_POST(crunchBox(ds, brand_colors = list("primary"="#7eb62f",
                                                            "secondary"="#7eb62f",
                                                            "message"="#7eb62f"),
                                    filters = NULL),
                          'https://app.crunch.io/api/datasets/1/boxdata/',
                          '{"element":"shoji:entity","body":{"filters":[]',
                          ',"display_settings":{"palette":{"brand_colors":',
                          '{"primary":"#7eb62f","secondary":"#7eb62f",',
                          '"message":"#7eb62f"}}}}}')
              expect_POST(crunchBox(ds, static_colors = list("#7eb62f",
                                                             "#7eb62f",
                                                             "#7eb62f"),
                                    filters = NULL),
                          'https://app.crunch.io/api/datasets/1/boxdata/',
                          '{"element":"shoji:entity","body":{"filters":[]',
                          ',"display_settings":{"palette":{"static_colors":',
                          '["#7eb62f","#7eb62f","#7eb62f"]}}}}')
              expect_POST(crunchBox(ds, category_color_lookup =
                                        list("cat1"="#7eb62f",
                                             "cat2"="#7eb62f",
                                             "cat3"="#7eb62f"),
                                    filters = NULL),
                          'https://app.crunch.io/api/datasets/1/boxdata/',
                          '{"element":"shoji:entity","body":{"filters":[]',
                          ',"display_settings":{"palette":{"category_lookup":',
                          '{"cat1":"#7eb62f","cat2":"#7eb62f",',
                          '"cat3":"#7eb62f"}}}}}')
              })

    test_that("Input validation", {
        expect_error(crunchBox(), "'dataset' must be a CrunchDataset")
        expect_error(crunchBox(4), "'dataset' must be a CrunchDataset")
        expect_error(crunchBox(ds, 4),
            "'filters' should be a FilterCatalog or NULL")
        expect_error(crunchBox(ds, brand_colors = "a"),
                     paste0(
                         sQuote("brand_colors"), " must be a named list with only ",
                         serialPaste(dQuote(c("primary", "secondary", "message")),
                                     collapse = "or")))
        expect_error(crunchBox(ds, brand_colors = list(a = "foo")),
                     paste0(
                         sQuote("brand_colors"), " must be a named list with only ",
                         serialPaste(dQuote(c("primary", "secondary", "message")),
                                     collapse = "or")))
        expect_error(crunchBox(ds, static_colors = "a"),
                     paste0(sQuote("static_colors"), " must be a list of characters"))
        expect_error(crunchBox(ds, category_color_lookup = "a"),
                     paste0(sQuote("category_color_lookup"), " must be a named list"))
        expect_error(crunchBox(ds, category_color_lookup = list("a")),
                     paste0(sQuote("category_color_lookup"), " must be a named list"))
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

with_test_authentication({
    testdf <- as.data.frame(sapply(letters[1:8], function (x) df$v4, simplify=FALSE))
    testdf$cat <- as.factor(letters[1:10])
    testdf$num <- 1
    ds <- newDataset(testdf)
    ds$mr <- makeMR(ds[letters[1:8]],
        name="Excessively long variable name to trigger the check for length",
        selections="B")
    names(subvariables(ds$mr))[1] <- "Another really really long name to check for the same for subvariables"
    names(categories(ds$cat))[2] <- "Extra long category name because we check those too"

    test_that("check box catches the various cases", {
        expect_output(preCrunchBoxCheck(ds),
            "Shorter variable names will display in the menus better. This variable has a name longer than 40 characters")
    })

    weight(ds) <- ds$num
    hiddenVariables(ds) <- "num"
    filters(ds)[["A filter"]] <- ds$cat == "d"
    test_that("We can make a box", {
        expect_true(is.character(crunchBox(ds)))
    })
})
