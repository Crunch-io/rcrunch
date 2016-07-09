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
    expect_equal(compareCategories(cat1, cat2),
        data.frame(
            numeric_value.A=c(1L, 2L, NA, NA),
            id.A=c(1L, 2L, -1L, NA),
            name=c("B", "C", "No Data", "Name 1"),
            id.B=c(2L, NA, -1L, 1L),
            numeric_value.B=c(4L, NA, NA, 3L),
            stringsAsFactors=FALSE
        ))
})
