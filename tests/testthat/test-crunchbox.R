context("CrunchBox")

test_that("Box size limit check", {
    expect_false(boxTooBig(0, 0))
    expect_false(boxTooBig(1, 0))
    expect_false(boxTooBig(0, 1))
    expect_true(boxTooBig(100, 6))
})

test_that("Embed URL", {
    expect_identical(
        boxdataToWidgetURL(
            "http://cf.example/d/stuff/1a1577c91fbb2c1cbd3800e181188508/dataset.json"
        ),
        "//s.crunch.io/widget/index.html#/ds/1a1577c91fbb2c1cbd3800e181188508/"
    )
    expect_identical(
        boxdataToWidgetURL("//s.crunch.io/widget/index.html#/ds/1a1577c91fbb2c1cbd3800e181188508/"),
        "//s.crunch.io/widget/index.html#/ds/1a1577c91fbb2c1cbd3800e181188508/"
    )
})

test_that("Iframe code (prints and returns invisibly)", {
    expect_output(
        expect_identical(
            embedCrunchBox("http://cf.example/d/stuff/1a1577c91fbb2c1cbd3800e181188508/dataset.json"),
            paste0(
                '<iframe src="//s.crunch.io/widget/index.html#/ds/1a1577c91fbb2c',
                '1cbd3800e181188508/" width="600" height="480" style="border: ',
                '1px solid #d3d3d3;"></iframe>'
            )
        ),
        paste0(
            '<iframe src="//s.crunch.io/widget/index.html#/ds/1a1577c91fbb2c1cbd3800e181188508/"',
            ' width="600" height="480" style="border: 1px solid #d3d3d3;"></iframe>'
        ),
        fixed = TRUE
    )
})

iframe_with_logo <- paste0(
    '<figure style="text-align: left;" class="content-list-component image">
    <img src="//s.crunch.io/public/branding/example.gif" style="height:auto; ',
    'width:200px; margin-left:-4px"></img>
    <iframe src="//s.crunch.io/widget/index.html#/ds/1a1577c91fbb2c1cbd3800e181188508/"',
    ' width="600" height="480" style="border: 1px solid #d3d3d3;"></iframe>
</figure>'
)

iframe_with_title <- paste0(
    '<figure style="text-align: left;" class="content-list-component image">
    <div style="padding-bottom: 12px">
        <span style="font-size: 18px; color: #444444; line-height: 1;">Example title here</span>
    </div>
    <iframe src="//s.crunch.io/widget/index.html#/ds/1a1577c91fbb2c1cbd3800e181188508/"',
    ' width="600" height="480" style="border: 1px solid #d3d3d3;"></iframe>
</figure>'
)


test_that("Iframe code with logo", {
    expect_output(
        expect_identical(
            embedCrunchBox(
                "http://cf.example/d/stuff/1a1577c91fbb2c1cbd3800e181188508/dataset.json",
                logo = "//s.crunch.io/public/branding/example.gif"
            ),
            iframe_with_logo
        ),
        iframe_with_logo,
        fixed = TRUE
    )
})

test_that("Iframe code with title", {
    expect_output(
        expect_identical(
            embedCrunchBox(
                "http://cf.example/d/stuff/1a1577c91fbb2c1cbd3800e181188508/dataset.json",
                title = "Example title here"
            ),
            iframe_with_title
        ),
        iframe_with_title,
        fixed = TRUE
    )
})

test_that("color validator validates", {
    expect_error(
        validHexColor(1),
        "A color must be a character, got numeric instead"
    )
    expect_error(
        validHexColor("#ffeeaaffeeaa"),
        "is not a valid hex color"
    )
    expect_error(
        validHexColor("#zzzzzz"),
        paste0(dQuote("#zzzzzz"), " is not a valid hex color")
    )
    expect_error(
        validHexColor("#aabbc"),
        paste0(dQuote("#aabbc"), " is not a valid hex color")
    )
    expect_equal(validHexColor("#d3d3d3"), "#d3d3d3")
    expect_equal(validHexColor("d3d3d3"), "#d3d3d3")
    expect_equal(validHexColor("#d3d3d3ff"), "#d3d3d3")
    expect_equal(validHexColor("d3d3d3ff"), "#d3d3d3")
    expect_equal(validHexColor("aliceblue"), "#F0F8FF")
})

with_mock_crunch({
    ds <- loadDataset("test ds")
    ds3 <- loadDataset("ECON.sav")

    test_that("preCrunchBoxCheck does not error", {
        expect_prints(
            preCrunchBoxCheck(ds),
            paste0(
                "We recommend using only categorical and multiple_response variables. ",
                "These 4 variables have an unsupported type"
            )
        )
    })

    test_that("Basic box", {
        expect_POST(
            crunchBox(ds, filters = NULL),
            "https://app.crunch.io/api/datasets/1/boxdata/",
            '{"element":"shoji:entity","body":{"filters":[],"weight":null}}'
        )
    })
    test_that("Basic box with metadata and filters", {
        expect_POST(
            crunchBox(ds, title = "Test box"),
            "https://app.crunch.io/api/datasets/1/boxdata/",
            '{"element":"shoji:entity","body":{"filters":[',
            '{"filter":"https://app.crunch.io/api/datasets/1/filters/filter1/"},',
            '{"filter":"https://app.crunch.io/api/datasets/1/filters/filter2/"}],',
            '"weight":null,"title":"Test box"}}'
        )
    })
    test_that("Select variables in box", {
        expect_POST(
            crunchBox(ds[2:5], filters = NULL),
            "https://app.crunch.io/api/datasets/1/boxdata/",
            '{"element":"shoji:entity","body":{"filters":[],"weight":null,',
            '"where":{"function":"select","args":[{"map":{',
            '"66ae9881e3524f7db84970d556c34552":',
            '{"variable":"https://app.crunch.io/api/datasets/1/variables/gender/"},',
            '"loc":',
            '{"variable":"https://app.crunch.io/api/datasets/1/variables/location/"},',
            '"949d2dc7e7a24e6090cc88bb92e1d2fb":',
            '{"variable":"https://app.crunch.io/api/datasets/1/variables/mymrset/"},',
            '"text":{"variable":"https://app.crunch.io/api/datasets/1/variables/textVar/"}',
            "}}]}}}"
        )
    })
    test_that("Hidden variables are automatically 'selected' out (and weight is used)", {
        expect_POST(
            crunchBox(ds3),
            "https://app.crunch.io/api/datasets/3/boxdata/",
            '{"element":"shoji:entity","body":{"filters":[],',
            '"weight":"https://app.crunch.io/api/datasets/3/variables/birthyr/",',
            '"where":{"function":"select","args":[{"map":{',
            '"66ae9881e3524f7db84970d556c34552":',
            '{"variable":"https://app.crunch.io/api/datasets/3/variables/gender/"},',
            '"d7c21314ca9e453c93069168681a285c":',
            '{"variable":"https://app.crunch.io/api/datasets/3/variables/starttime/"}}}]}}}'
        )
    })
    test_that("Can override the current weight", {
        expect_POST(
            crunchBox(ds3, weight = NULL),
            "https://app.crunch.io/api/datasets/3/boxdata/",
            '{"element":"shoji:entity","body":{"filters":[],',
            '"weight":null,',
            '"where":{"function":"select","args":[{"map":{',
            '"66ae9881e3524f7db84970d556c34552":',
            '{"variable":"https://app.crunch.io/api/datasets/3/variables/gender/"},',
            '"d7c21314ca9e453c93069168681a285c":',
            '{"variable":"https://app.crunch.io/api/datasets/3/variables/starttime/"}}}]}}}'
        )
    })
    test_that("Select filters in box", {
        expect_POST(
            crunchBox(ds, filters = filters(ds)[c(2, 1)], title = "Test box"),
            "https://app.crunch.io/api/datasets/1/boxdata/",
            '{"element":"shoji:entity","body":{"filters":[',
            '{"filter":"https://app.crunch.io/api/datasets/1/filters/filter2/"},',
            '{"filter":"https://app.crunch.io/api/datasets/1/filters/filter1/"}],',
            '"weight":null,"title":"Test box"}}'
        )
        expect_POST(
            crunchBox(ds, filters = filters(ds)[2]),
            "https://app.crunch.io/api/datasets/1/boxdata/",
            '{"element":"shoji:entity","body":{"filters":[',
            '{"filter":"https://app.crunch.io/api/datasets/1/filters/filter2/"}],',
            '"weight":null}}'
        )
    })
    test_that("Boxes can have color palletes specified", {
        expect_POST(
            crunchBox(ds,
                brand_colors = c("#ff0aa4", "#af17ff", "#260aff"),
                filters = NULL
            ),
            "https://app.crunch.io/api/datasets/1/boxdata/",
            '{"element":"shoji:entity","body":{"filters":[],"weight":null,',
            '"display_settings":{"palette":{"brand_colors":',
            '{"primary":"#ff0aa4","secondary":"#af17ff",',
            '"message":"#260aff"}}}}}'
        )
        expect_POST(
            crunchBox(ds,
                brand_colors = c(message = "#ff0aa4", secondary = "#af17ff", primary = "#260aff"),
                filters = NULL
            ),
            "https://app.crunch.io/api/datasets/1/boxdata/",
            '{"element":"shoji:entity","body":{"filters":[],"weight":null,',
            '"display_settings":{"palette":{"brand_colors":',
            '{"message":"#ff0aa4","secondary":"#af17ff",',
            '"primary":"#260aff"}}}}}'
        )
        expect_POST(
            crunchBox(ds,
                brand_colors = list(
                    secondary = "#af17ff", message = "#ff0aa4", primary = "#260aff"
                ),
                filters = NULL
            ),
            "https://app.crunch.io/api/datasets/1/boxdata/",
            '{"element":"shoji:entity","body":{"filters":[],"weight":null,',
            '"display_settings":{"palette":{"brand_colors":',
            '{"secondary":"#af17ff","message":"#ff0aa4",',
            '"primary":"#260aff"}}}}}'
        )
        expect_POST(
            crunchBox(ds,
                static_colors = list(
                    "#ff0aa4",
                    "#af17ff",
                    "#260aff"
                ),
                filters = NULL
            ),
            "https://app.crunch.io/api/datasets/1/boxdata/",
            '{"element":"shoji:entity","body":{"filters":[],"weight":null,',
            '"display_settings":{"palette":{"static_colors":',
            '["#ff0aa4","#af17ff","#260aff"]}}}}'
        )
        expect_POST(
            crunchBox(ds,
                category_color_lookup =
                    list(
                        "cat1" = "#ff0aa4",
                        "cat2" = "#af17ff",
                        "cat3" = "#260aff"
                    ),
                filters = NULL
            ),
            "https://app.crunch.io/api/datasets/1/boxdata/",
            '{"element":"shoji:entity","body":{"filters":[],"weight":null,',
            '"display_settings":{"palette":{"category_lookup":',
            '{"cat1":"#ff0aa4","cat2":"#af17ff",',
            '"cat3":"#260aff"}}}}}'
        )
    })

    test_that("Input validation", {
        expect_error(crunchBox(), "'dataset' must be a CrunchDataset")
        expect_error(crunchBox(4), "'dataset' must be a CrunchDataset")
        expect_error(
            crunchBox(ds, 4),
            "'filters' should be a FilterCatalog or NULL"
        )
        expect_error(
            crunchBox(ds, brand_colors = c(
                "#ff0aa4", "#af17ff",
                "#260aff", "#ff0aa4",
                "#af17ff", "#260aff"
            )),
            paste0(
                sQuote("brand_colors"), " must be at most 3 elements long"
            )
        )
        expect_error(
            crunchBox(ds, brand_colors = 1),
            paste0(
                sQuote("brand_colors"), " must be character ",
                "vector or list of characters"
            )
        )
        expect_error(
            crunchBox(ds, brand_colors = list(a = "aabbcc")),
            paste0(
                "If ", sQuote("brand_colors"),
                " is a named list, it must contain only ",
                serialPaste(dQuote(c("primary", "secondary", "message")),
                    collapse = "and"
                )
            )
        )
        expect_error(
            crunchBox(ds, static_colors = 1),
            paste0(
                sQuote("static_colors"), " must be a vector or ",
                "list of characters"
            )
        )
        expect_error(
            crunchBox(ds, category_color_lookup = "a"),
            paste0(sQuote("category_color_lookup"), " must be a named list")
        )
        expect_error(
            crunchBox(ds, category_color_lookup = list("a")),
            paste0(sQuote("category_color_lookup"), " must be a named list")
        )
    })
    test_that("Box too big message", {
        with_mock(`crunch:::.boxlimit` = function() -1, {
            expect_error(
                crunchBox(ds[2:5], filters = filters(ds)[1]),
                "4 variables and 1 filter results in too many cubes"
            )
            expect_error(
                crunchBox(ds[2]),
                "1 variable and 2 filters results in too many cubes"
            )
        })
    })
})

with_test_authentication({
    testdf <- as.data.frame(sapply(letters[1:8], function(x) df$v4, simplify = FALSE))
    testdf$cat <- as.factor(letters[1:10])
    testdf$num <- 1
    ds <- newDataset(testdf)
    ds$mr <- makeMR(ds[letters[1:8]],
        name = "Excessively long variable name to trigger the check for length",
        selections = "B"
    )
    names(subvariables(ds$mr))[1] <-
        "Another really really long name to check for the same for subvariables"
    names(categories(ds$cat))[2] <- "Extra long category name because we check those too"

    test_that("check box catches the various cases", {
        expect_prints(
            preCrunchBoxCheck(ds),
            paste0(
                "Shorter variable names will display in the menus better. ",
                "This variable has a name longer than 40 characters"
            )
        )
    })

    weight(ds) <- ds$num
    hiddenVariables(ds) <- "num"
    filters(ds)[["A filter"]] <- ds$cat == "d"
    test_that("We can make a box", {
        expect_true(is.character(crunchBox(ds)))
    })
})
