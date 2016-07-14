context("Comparing datasets and variables")

cat1 <- Categories(
    list(id=1L, name="B", numeric_value=1L, missing=FALSE),
    list(id=2L, name="C", numeric_value=2L, missing=FALSE),
    list(id=-1L, name="No Data", numeric_value=NULL, missing=TRUE)
)
cat2 <- Categories(
    list(id=1L, name="Name 1", numeric_value=3L, missing=FALSE),
    list(id=2L, name="B", numeric_value=4L, missing=FALSE),
    list(id=-1L, name="No Data", numeric_value=NULL, missing=TRUE)
)

test_that("compareCategories", {
    expect_equivalent(compareCategories(cat1, cat2),
        data.frame(
            # numeric_value.A=c(1L, 2L, NA, NA),
            id.A=c(1L, 2L, -1L, NA),
            name=c("B", "C", "No Data", "Name 1"),
            id.B=c(2L, NA, -1L, 1L),
            # numeric_value.B=c(4L, NA, NA, 3L),
            stringsAsFactors=FALSE
        ))
})

test_that("summarizeCompareCategories", {
    expect_equal(summary(compareCategories(cat1, cat2))$problems,
        list(mismatched.ids=c("B", "C", "Name 1")))
    expect_equal(summary(compareCategories(cat1, cat1))$problems,
        list(mismatched.ids=character(0)))
})

test_that("print compareCategory summary", {
    expect_output(summary(compareCategories(cat1, cat2)),
        paste(
            "Total categories: 4 ",
            "",
            "Mismatched ids: 3 ",
            " id.A   name id.B",
            "    1      B    2",
            "    2      C   NA",
            "   NA Name 1    1",
            sep="\n"))
})

test_that("print compareCategory summary when categories are equivalent", {
    summary(compareCategories(cat1, cat1))
    expect_output(summary(compareCategories(cat1, cat1)),
        paste(
            "Total categories: 3 ",
            "All good :)",
            sep="\n"
            ))
})

with_mock_HTTP({
    ds1 <- loadDataset("test ds")
    ds2 <- loadDataset("an archived dataset", "archived")

    test_that("compareVariables", {
        expect_equal(summarizeCompareVariables(compareVariables(allVariables(ds1), allVariables(ds2)))$problems,
            list(mismatched.type="birthyr",
                mismatched.name=c("birthyr", "starttime", "birthyr2")))
    })

    test_that("print compareVariables", {
        expect_output(summary(compareVariables(allVariables(ds1), allVariables(ds2))),
            paste(
                "Total variables: 8 ",
                "",
                "Type mismatch: 1 ",
                "     name.A  type.A   alias   type.B    name.B",
                " Birth Year numeric birthyr datetime starttime",
                "",
                "Name mismatch: 3 ",
                "     name.A     alias     name.B",
                " Birth Year   birthyr  starttime",
                "  starttime starttime       <NA>",
                "       <NA>  birthyr2 Birth Year",
                sep="\n"
                ))
    })
    test_that("compareVariables when everything is ok", {
        expect_output(summary(compareVariables(allVariables(ds1), allVariables(ds1))),
            paste(
                "Total variables: 6 ",
                "No type or name mismatches.",
                sep="\n"
                ))
    })

    test_that("compareSubvariables", {

    })

    test_that("compareDatasets", {
        expect_output(summary(compareDatasets(ds1, ds2)),
            paste(
                "Total variables: 8 ",
                "",
                "Type mismatch: 1 ",
                "     name.A  type.A   alias   type.B    name.B",
                " Birth Year numeric birthyr datetime starttime",
                "",
                "Name mismatch: 3 ",
                "     name.A     alias     name.B",
                " Birth Year   birthyr  starttime",
                "  starttime starttime       <NA>",
                "       <NA>  birthyr2 Birth Year",
                "",
                "Matched variables with categories: 2 ",
                "With issues: 1 ",
                "",
                "$gender",
                "Total categories: 4 ",
                "",
                "Mismatched ids: 2 ",
                " id.A   name id.B",
                "    2 Female    3",
                "   NA  Other    2",
                "",
                "",
                "Matched array variables: 1 ",
                "With subvariable issues: 1 ",
                "",
                "$mymrset",
                "Total subvariables: 4 ",
                "",
                "Mismatched names: 2 ",
                " name.A   alias name.B",
                " Second subvar1   <NA>",
                "   <NA> subvar4 Second",
                "",
                "Subvariables in B have multiple parents: 949d2dc7e7a24e6090cc88bb92e1d2fb and 12345 ",
                "",
                sep="\n"),
                fixed=TRUE)
    })

    test_that("compareDatasets when everything is ok", {
        expect_output(summary(compareDatasets(ds1, ds1)),
            paste(
                "Total variables: 6 ",
                "All good :)",
                sep="\n"
                ))
    })
})
