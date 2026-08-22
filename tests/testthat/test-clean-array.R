context("Cleaning array variables")

test_that("findCommonPrefix", {
    expect_identical(findCommonPrefix(c("abc def", "ab cd ef")), "ab")
    expect_identical(findCommonPrefix(c("XX select all. A", "XX select all. BB")), "XX select all. ")
    expect_identical(findCommonPrefix(c("A", "B")), "")
    expect_identical(findCommonPrefix(c("abc defg", "abc defg")), "abc defg")
    expect_identical(findCommonPrefix(c("abc defg", "gfed cbaooo")), "")
})
