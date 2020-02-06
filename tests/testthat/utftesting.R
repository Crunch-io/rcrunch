test_that("encoding + JSON reads correctly", {
    s <- iconv("aided_follow_grid:ElCorteInglés", to = "UTF-8")
    expect_identical(Encoding(s), "UTF-8")
    expect_true(grepl("Inglés", s))
    sj <- toJSON(s)
    expect_true(grepl("Inglés", sj))
    s2 <- fromJSON(sj)
    expect_identical(s2, s)
    expect_identical(fromJSON("utf-test.json"), "Bud\u011bjovick\u00fd Budvar")
})

with_mock_crunch({
    ds <- loadDataset("test ds")
    test_that("Reading UTF in tests", {
        expect_identical(description(ds$textVar), "Bud\u011bjovick\u00fd Budvar")
    })
})

with_test_authentication({
    ds <- newDataset(df[1:2, 1:2])
    test_that("Properly encoded UTF is sent and received", {
        s <- iconv("aided_follow_grid:ElCorteInglés", to = "UTF-8")
        name(ds$v1) <- s
        expect_identical(name(ds$v1), s)
        expect_identical(name(refresh(ds)$v1), s)
        s2 <- "Bud\u011bjovick\u00fd Budvar"
        name(ds$v2) <- s2
        expect_identical(name(ds$v2), s2)
        expect_identical(name(refresh(ds)$v2), s2)
    })
})
