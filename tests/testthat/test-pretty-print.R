context("Pretty printing")

tst_array <- array(c(100,NA,3,40,500,6000,70000,80,9),
                   dim=c(3, 3))
tst_array[3] <- crayon::bgWhite(tst_array[3])
tst_array[5] <- crayon::red(tst_array[5])
tst_array[7] <- crayon::bgBlue(tst_array[7])

colnames(tst_array) <- c("A", "B", "C")
rownames(tst_array) <- c("one", "two", "three")

test_that("nonas removes NAs", {
    expect_equal(nonas(crayon::italic("NA")), crayon::italic("  "))
    expect_equal(nonas("NA"), "  ")
    expect_equal(nonas(crayon::italic("   NA")), crayon::italic("     "))
    expect_equal(nonas("   NA"), "     ")
})

test_that("applyStyles applies styles", {
    expect_equal(applyStyles("foo", c(crayon::italic, crayon::red)),
                 crayon::red(crayon::italic("foo")))
    expect_equal(applyStyles("NA", c(nonas, crayon::red)), crayon::red("  "))
    expect_equal(applyStyles("NA", c(crayon::red, nonas)), crayon::red("  "))
    expect_equal(applyStyles("NA", c(crayon::red)), crayon::red("NA"))
    expect_equal(applyStyles("NA", c(nonas)), "  ")
})

test_that("prettyPrint2d works with no styles", {
    expect_equal(prettyPrint2d(tst_array),
                 list("        ",
                      "        A    B     C",
                      "  one 100   40 \033[44m70000\033[49m",
                      "  two  NA  \033[31m500\033[39m    80",
                      "three   \033[47m3\033[49m 6000     9"
                 ))
})

test_that("prettyPrint2d works with row styles", {
    expect_equal(prettyPrint2d(tst_array, row_styles = list(crayon::bgWhite,
                                                            crayon::italic,
                                                            crayon::red)),
                  list("        ",
                       "        A    B     C",
                       "\033[47m  one 100   40 \033[44m70000\033[47m\033[49m",
                       "\033[3m  two  NA  \033[31m500\033[39m    80\033[23m",
                       "\033[31mthree   \033[47m3\033[49m 6000     9\033[39m"
                       ))
})

test_that("prettyPrint2d works with row styles and dimnames", {
    names(dimnames(tst_array)) <- c("foo", "bar")
    expect_equal(prettyPrint2d(tst_array, row_styles = list(crayon::bgWhite,
                                                            crayon::italic,
                                                            crayon::red)),
                 list("     bar",
                      "foo     A    B     C",
                      "\033[47m  one 100   40 \033[44m70000\033[47m\033[49m",
                      "\033[3m  two  NA  \033[31m500\033[39m    80\033[23m",
                      "\033[31mthree   \033[47m3\033[49m 6000     9\033[39m"
                 ))
})

test_that("prettyPrint2d works with multiple styles", {
    expect_equal(prettyPrint2d(tst_array, row_styles = list(crayon::bgWhite,
                                                            list(nonas, crayon::italic),
                                                            crayon::red)),
                 list("        ",
                      "        A    B     C",
                      "\033[47m  one 100   40 \033[44m70000\033[47m\033[49m",
                      "\033[3m  two      \033[31m500\033[39m    80\033[23m",
                      "\033[31mthree   \033[47m3\033[49m 6000     9\033[39m"
                 ))
})

test_that("prettyPrint2d works with colum styles", {
    expect_equal(prettyPrint2d(tst_array, col_styles = list(crayon::bgWhite,
                                                            crayon::italic,
                                                            crayon::red)),
                 list("        ",
                      "      \033[47m  A\033[49m \033[3m   B\033[23m \033[31m    C\033[39m",
                      "  one \033[47m100\033[49m \033[3m  40\033[23m \033[31m\033[44m70000\033[49m\033[39m",
                      "  two \033[47m NA\033[49m \033[3m \033[31m500\033[39m\033[23m \033[31m   80\033[39m",
                      "three \033[47m  \033[47m3\033[47m\033[49m \033[3m6000\033[23m \033[31m    9\033[39m"
                 ))
})

test_that("prettyPrint2d works with both styles", {
        expect_equal(prettyPrint2d(tst_array,
                                   row_styles = list(crayon::bgBlack,
                                                     crayon::bgBlue,
                                                     crayon::underline),
                                   col_styles = list(crayon::bgWhite,
                                                     crayon::italic,
                                                     crayon::red)),
                     list("        ",
                          "      \033[47m  A\033[49m \033[3m   B\033[23m \033[31m    C\033[39m",
                          "\033[40m  one \033[47m100\033[40m \033[3m  40\033[23m \033[31m\033[44m70000\033[40m\033[39m\033[49m",
                          "\033[44m  two \033[47m NA\033[44m \033[3m \033[31m500\033[39m\033[23m \033[31m   80\033[39m\033[49m",
                          "\033[4mthree \033[47m  \033[47m3\033[47m\033[49m \033[3m6000\033[23m \033[31m    9\033[39m\033[24m"
                     ))
})

test_that("prettyPrint2d input validation", {
    expect_error(prettyPrint2d(tst_array, row_styles = list(crayon::bgBlack)),
                 "The number of row styles doesn't match the number of rows")
    expect_error(prettyPrint2d(tst_array, col_styles = list(crayon::bgWhite)),
                 "The number of column styles doesn't match the number of columns")
})

test_that("prettyPrint2d works with row styles and 1d", {
    tst_array <- array(c(100,NA,3),
                       dim=c(3))
    tst_array[3] <- crayon::bgWhite(tst_array[3])
    rownames(tst_array) <- c("one", "two", "three")

    expect_equal(prettyPrint2d(tst_array, row_styles = list(crayon::bgWhite,
                                                            crayon::italic,
                                                            crayon::red)),
                 list("        ",
                      "     ",
                      "\033[47m  one 100\033[49m",
                      "\033[3m  two  NA\033[23m",
                      "\033[31mthree   \033[47m3\033[49m\033[39m"
                 ))
})
