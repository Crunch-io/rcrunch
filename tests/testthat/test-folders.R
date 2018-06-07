context("Folders of variables")

test_that("parseFolderPath", {
    expect_identical(parseFolderPath("foo"), "foo")
    expect_identical(parseFolderPath(c("foo", "bar")), c("foo", "bar"))
    expect_identical(parseFolderPath(c("foo/bar")), c("foo", "bar"))
    expect_identical(parseFolderPath(c("foo/bar", "blech")), c("foo/bar", "blech"))
    with(temp.option(crunch.delimiter="|"), {
        expect_identical(parseFolderPath(c("foo|bar")), c("foo", "bar"))
        expect_identical(parseFolderPath(c("foo/bar|blech")), c("foo/bar", "blech"))
    })
    expect_identical(parseFolderPath("/"), "")
})

with_mock_crunch({
    ds <- loadDataset("test ds")

    test_that("cd() returns a folder", {
        expect_identical(cd(ds, "Group 1/Nested"),
            folders(ds)[["Group 1/Nested"]])
    })
    test_that("cd() can operate on a folder too", {
        expect_identical(cd(cd(ds, "Group 1"), "Nested"),
            folders(ds)[["Group 1/Nested"]])
    })
    test_that("cd() errors if the path isn't a folder or doesn't exist", {
        expect_error(cd(ds, "Group 1/Birth Year"),
            '"Group 1/Birth Year" is not a folder')
        expect_error(cd(ds, "Group 1/foo"),
            '"Group 1/foo" is not a folder')
    })
    test_that("cd errors if it doesn't receive a dataset or folder as first arg", {
        expect_error(cd("/"), paste(dQuote("cd()"),
            "requires a Crunch Dataset or Folder as its first argument"),
            fixed=TRUE)
    })
    test_that("cd attempts to create folders if create=TRUE", {
        expect_POST(cd(ds, "Group 1/foo", create=TRUE),
            "https://app.crunch.io/api/datasets/1/folders/1/",
            '{"element":"shoji:catalog","body":{"name":"foo"}}')
        expect_POST(cd(ds, "Group 1/foo/bar", create=TRUE),
            "https://app.crunch.io/api/datasets/1/folders/1/",
            '{"element":"shoji:catalog","body":{"name":"foo"}}')
    })
    test_that("cd ..", {
        expect_identical(ds %>% cd("Group 1/Nested") %>% cd(".."),
            cd(ds, "Group 1"))
        expect_identical(ds %>% cd("Group 1") %>% cd(".."),
            folders(ds))
        expect_identical(ds %>% cd("Group 1/Nested") %>% cd("../.."),
            folders(ds))
        expect_error(folders(ds) %>% cd(".."), '".." is an invalid path')
        expect_error(cd(ds, ".."), '".." is an invalid path')
    })
    test_that("cd /", {
        expect_identical(ds %>% cd("/"), folders(ds))
        expect_identical(ds %>% cd("Group 1") %>% cd("/Group 2"),
            cd(ds, "Group 2"))
        expect_identical(ds %>% cd("Group 1") %>% cd("/"),
            folders(ds))
    })

    add_birthyr_to_group2 <- paste0(
        'https://app.crunch.io/api/datasets/1/folders/2/',
        ' ',
        '{"element":"shoji:catalog","index":{',
        '"https://app.crunch.io/api/datasets/1/variables/birthyr/":{}},',
        '"graph":[',
        '"https://app.crunch.io/api/datasets/1/variables/starttime/",',
        '"https://app.crunch.io/api/datasets/1/variables/catarray/",',
        '"https://app.crunch.io/api/datasets/1/variables/birthyr/"',
        ']}'
    )
    test_that("mv variables to existing folder, selecting from dataset", {
        expect_PATCH(ds %>% mv(c("birthyr", "gender"), "Group 2"),
            'https://app.crunch.io/api/datasets/1/folders/2/',
            '{"element":"shoji:catalog","index":{',
            '"https://app.crunch.io/api/datasets/1/variables/birthyr/":{},',
            '"https://app.crunch.io/api/datasets/1/variables/gender/":{}},',
            '"graph":[',
            '"https://app.crunch.io/api/datasets/1/variables/starttime/",',
            '"https://app.crunch.io/api/datasets/1/variables/catarray/",',
            '"https://app.crunch.io/api/datasets/1/variables/birthyr/",',
            '"https://app.crunch.io/api/datasets/1/variables/gender/"',
            ']}')
    })
    test_that("mv doesn't include vars that already exist in the folder in index patch", {
        expect_PATCH(ds %>% mv(c("birthyr", "starttime"), "Group 2"),
            add_birthyr_to_group2)
        expect_no_request(ds %>% mv("starttime", "Group 2"))
    })
    test_that("mkdir", {
        expect_POST(ds %>% mkdir("Group 1/foo"),
            "https://app.crunch.io/api/datasets/1/folders/1/",
            '{"element":"shoji:catalog","body":{"name":"foo"}}')
        expect_POST(ds %>% cd("Group 1") %>% mkdir("foo"),
            "https://app.crunch.io/api/datasets/1/folders/1/",
            '{"element":"shoji:catalog","body":{"name":"foo"}}')
        expect_POST(ds %>% mkdir("bar"),
            "https://app.crunch.io/api/datasets/1/folders/",
            '{"element":"shoji:catalog","body":{"name":"bar"}}')
    })
    test_that("mv makes destination directory if it doesn't exist", {
        expect_POST(ds %>% mv("birthyr", "Group 1/foo"),
            "https://app.crunch.io/api/datasets/1/folders/1/",
            '{"element":"shoji:catalog","body":{"name":"foo"}}')
    })
    test_that("mv to the folder() of an object", {
        expect_PATCH(ds %>% mv("birthyr", folder(ds$starttime)),
            add_birthyr_to_group2)
    })
    test_that("cd then mv (relative path)", {
        expect_PATCH(ds %>% cd("Group 1") %>% mv("Birth Year", "../Group 2"),
            add_birthyr_to_group2)
    })
    test_that("mv (contents of) a folder by specifying it as 'variables'", {
        expected_patch <- paste0(
            'https://app.crunch.io/api/datasets/1/folders/2/',
            ' ',
            '{"element":"shoji:catalog","index":{',
            '"https://app.crunch.io/api/datasets/1/variables/birthyr/":{},',
            '"https://app.crunch.io/api/datasets/1/folders/3/":{},',
            '"https://app.crunch.io/api/datasets/1/variables/textVar/":{}},',
            '"graph":[',
            '"https://app.crunch.io/api/datasets/1/variables/starttime/",',
            '"https://app.crunch.io/api/datasets/1/variables/catarray/",',
            '"https://app.crunch.io/api/datasets/1/variables/birthyr/",',
            '"https://app.crunch.io/api/datasets/1/folders/3/",',
            '"https://app.crunch.io/api/datasets/1/variables/textVar/"',
            ']}'
        )
        expect_PATCH(ds %>% mv(cd(ds, "Group 1"), "Group 2"),
            expected_patch)
        ## Can use . to say current directory contents, but have to specify it twice
        expect_PATCH(ds %>% cd("Group 1") %>% mv(., ., "../Group 2"),
            expected_patch)
        ## Or can say "TRUE" to select all
        expect_PATCH(ds %>% cd("Group 1") %>% mv(TRUE, "../Group 2"),
            expected_patch)
    })
    test_that("mv folders", {
        expect_PATCH(ds %>% cd("Group 1") %>% mv("Nested", "../Group 2"),
            'https://app.crunch.io/api/datasets/1/folders/2/',
            '{"element":"shoji:catalog","index":{',
            '"https://app.crunch.io/api/datasets/1/folders/3/":{}},',
            '"graph":[',
            '"https://app.crunch.io/api/datasets/1/variables/starttime/",',
            '"https://app.crunch.io/api/datasets/1/variables/catarray/",',
            '"https://app.crunch.io/api/datasets/1/folders/3/"',
            ']}')
    })
    test_that("mv error handling", {
        expect_error(ds %>% cd("Group 1") %>% mv("NOT A VARIABLE", "../Group 2"),
            "Undefined elements selected: NOT A VARIABLE")
    })
    test_that("mv with dplyr-esque utilities", {
        expect_PATCH(ds %>% cd("Group 1") %>% mv(starts_with("Birth"), "../Group 2"),
            add_birthyr_to_group2)
        expect_PATCH(ds %>% cd("Group 1") %>% mv(ends_with("Year"), "../Group 2"),
            add_birthyr_to_group2)
        ## Duplicates are resolved
        expect_PATCH(ds %>% cd("Group 1") %>% mv(c(starts_with("Birth"), ends_with("Year")), "../Group 2"),
            add_birthyr_to_group2)
        expect_PATCH(ds %>% cd("Group 1") %>% mv(c(starts_with("Birth"), contains("est")), "../Group 2"),
            'https://app.crunch.io/api/datasets/1/folders/2/',
            '{"element":"shoji:catalog","index":{',
            '"https://app.crunch.io/api/datasets/1/variables/birthyr/":{},',
            '"https://app.crunch.io/api/datasets/1/folders/3/":{}},',
            '"graph":[',
            '"https://app.crunch.io/api/datasets/1/variables/starttime/",',
            '"https://app.crunch.io/api/datasets/1/variables/catarray/",',
            '"https://app.crunch.io/api/datasets/1/variables/birthyr/",',
            '"https://app.crunch.io/api/datasets/1/folders/3/"',
            ']}')
    })

    test_that("rmdir deletes", {
        expect_error(ds %>% rmdir("Group 1/Nested"),
            "Must confirm deleting folder")
        with_consent({
            expect_DELETE(ds %>% rmdir("Group 1/Nested"),
                "https://app.crunch.io/api/datasets/1/folders/3/")
            expect_DELETE(ds %>% cd("Group 1") %>% rmdir("Nested"),
                "https://app.crunch.io/api/datasets/1/folders/3/")
            expect_DELETE(ds %>% cd("Group 1") %>% rmdir("../Group 2"),
                "https://app.crunch.io/api/datasets/1/folders/2/")
        })
        expect_error(ds %>% rmdir("/"),
            "Cannot delete root folder")
    })

    test_that("setOrder", {
        expect_identical(ds %>% cd("Group 1") %>% names(),
            c("Birth Year", "Nested", "Text variable ftw"))
        expect_PATCH(ds %>% cd("Group 1") %>% setOrder(c(2, 1, 3)),
            'https://app.crunch.io/api/datasets/1/folders/1/',
            '{"element":"shoji:catalog","graph":[',
            '"https://app.crunch.io/api/datasets/1/folders/3/",',
            '"https://app.crunch.io/api/datasets/1/variables/birthyr/",',
            '"https://app.crunch.io/api/datasets/1/variables/textVar/"]}')
        with_PATCH(NULL, {
            ## Since we reorder in place without doing a GET to refresh, check
            ## that that matches our expectations, ignoring the PATCH request
            expect_identical(ds %>% cd("Group 1") %>% setOrder(c(2, 1, 3)) %>% names(),
                c("Nested", "Birth Year", "Text variable ftw"))
            expect_identical(
                ds %>%
                    cd("Group 1") %>%
                    setOrder(., sort(names(.), decreasing=TRUE)) %>%
                    names(),
                c("Text variable ftw", "Nested", "Birth Year"))
        })
    })
    test_that("setOrder makes no request if the order does not change", {
        expect_no_request(ds %>% cd("Group 1") %>% setOrder(1:3))
    })
    test_that("setOrder validation", {
        g1 <- cd(ds, "Group 1")
        expect_error(setOrder(g1, c("Birth Year", "NOTAFOLDER")),
            "Invalid value: NOTAFOLDER")
        expect_error(setOrder(g1, NULL),
            "Order values must be character or numeric, not NULL")
        expect_error(setOrder(g1, -3),
            "Invalid value: -3")
        expect_error(setOrder(g1, c(NA, 2, -4)),
            "Invalid values: NA, -4")
        expect_error(setOrder(g1, c(2, 1, 1, 3)),
            "Order values must be unique: 1 is duplicated")
    })
})

with_test_authentication({
    ds <- newDataset(mtcars)
    test_that("Basic folder integration test", {
        ds <- mkdir(ds, "test")
        expect_true("test" %in% names(cd(ds, "/")))
        ds <- mv(ds, "mpg", "test")
        # Variable is in `names`
        expect_true("mpg" %in% names(ds))
        # But it's not at the top level
        expect_false("mpg" %in% names(cd(ds, "/")))
        expect_true("mpg" %in% names(cd(ds, "test")))
        with_consent(ds <- rmdir(ds, "test"))
        expect_false("test" %in% names(cd(ds, "/")))
        expect_false("mpg" %in% names(ds))
    })

    test_that("mv preserves order of variables", {
        ds <- mv(ds, c("qsec", "drat", "vs", "wt"), "newdir")
        expect_identical(names(cd(ds, "newdir")), c("qsec", "drat", "vs", "wt"))
    })
})
