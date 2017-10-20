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
})

with_mock_crunch({
    ds <- loadDataset("test ds")
    test.ord <- ordering(ds)
    ent.urls <- urls(test.ord)
    varcat_url <- self(allVariables(ds))
    nested.ord <- VariableOrder(
        VariableGroup(name="Group 1",
            entities=list(ent.urls[1],
                        VariableGroup(name="Nested", entities=ent.urls[2:4]),
                        ent.urls[5])),
        VariableGroup(name="Group 2", entities=ent.urls[6:7]),
        catalog_url=varcat_url)

    test_that("Setup: reminder of what those orders look like", {
        expect_fixed_output(nested.ord,
            paste("[+] Group 1",
                  "    Birth Year",
                  "    [+] Nested",
                  "        Gender",
                  "        Categorical Location",
                  "        mymrset",
                  "    Text variable ftw",
                  "[+] Group 2",
                  "    starttime",
                  "    Cat Array",
                  sep="\n"))
    })
    test_that(".mkdir.inner (VariableOrder manipulation)", {
        add_empty_dir <- .mkdir.inner(nested.ord, c("Group 1", "New group"))
        expect_fixed_output(add_empty_dir,
            paste("[+] Group 1",
                  "    Birth Year",
                  "    [+] Nested",
                  "        Gender",
                  "        Categorical Location",
                  "        mymrset",
                  "    Text variable ftw",
                  "    [+] New group",
                  "        (Empty group)",
                  "[+] Group 2",
                  "    starttime",
                  "    Cat Array",
                  sep="\n"))
        ## This is to test that "New group" is only added once (expect_output
        ## does pattern matching, so that string matches even if it's not
        ## identical
        expect_identical(grep("New group", capture.output(add_empty_dir)), 8L)
    })
    test_that(".mkdir.inner with variables (as aliases)", {
        add_dir_w_vars <- .mkdir.inner(nested.ord, c("Group 1", "New group"),
            ds[c("gender", "starttime")])
        expect_fixed_output(add_dir_w_vars,
            paste("[+] Group 1",
                  "    Birth Year",
                  "    [+] Nested",
                  "        Categorical Location",
                  "        mymrset",
                  "    Text variable ftw",
                  "    [+] New group",
                  "        Gender",
                  "        starttime",
                  "[+] Group 2",
                  "    Cat Array",
                  sep="\n"))
    })
})
