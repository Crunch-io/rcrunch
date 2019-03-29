context("Datetime formatting")

test_that("from8601 parses", {
    expect_equal(
        from8601("2015-02-12T10:28:05.632000+00:00"),
        structure(list(
            sec = 5.632, min = 28L, hour = 10L, mday = 12L, mon = 1L,
            year = 115L, wday = 4L, yday = 42L, isdst = 0L,
            gmtoff = 0L
        ),
        .Names = c(
            "sec", "min", "hour", "mday", "mon",
            "year", "wday", "yday", "isdst", "gmtoff"
        ),
        class = c("POSIXlt", "POSIXt"), tzone = "UTC"
        )
    )
    expect_equal(
        from8601("2015-02-12T10:28:05.632000"),
        structure(list(
            sec = 5.632, min = 28L, hour = 10L, mday = 12L, mon = 1L,
            year = 115L, wday = 4L, yday = 42L, isdst = 0L,
            gmtoff = 0L
        ),
        .Names = c(
            "sec", "min", "hour", "mday", "mon",
            "year", "wday", "yday", "isdst", "gmtoff"
        ),
        class = c("POSIXlt", "POSIXt"), tzone = "UTC"
        )
    )
    expect_equal(
        from8601("2015-02-12T10:28:05.632000+06:00"),
        structure(list(
            sec = 5.632, min = 28L, hour = 4L, mday = 12L, mon = 1L,
            year = 115L, wday = 4L, yday = 42L, isdst = 0L,
            gmtoff = 21600L
        ), # 6 * 60 * 60
        .Names = c(
            "sec", "min", "hour", "mday", "mon",
            "year", "wday", "yday", "isdst", "gmtoff"
        ),
        class = c("POSIXlt", "POSIXt"), tzone = "UTC"
        )
    )
})

test_that("from8601 returns Date class if given only dates", {
    expect_identical(from8601("2015-02-12"), as.Date("2015-02-12"))
    expect_identical(from8601(c("2015-02-12", NA)), as.Date(c("2015-02-12", NA)))
})


test_that("default date formater", {
    expect_error(
        datetimeFormater("not a resolution"),
        paste0(
            dQuote("resolution"), " is invalid. Valid values are ",
            serialPaste(c("Y", "Q", "M", "W", "D", "h", "m", "s", "ms"),
                collapse = "or"
            )
        )
    )
    expect_equal(datetimeFormater("Y"), "%Y")
    expect_equal(datetimeFormater("Q"), "%Y-%m-%d")
    expect_equal(datetimeFormater("M"), "%Y-%m")
    expect_equal(datetimeFormater("W"), "%Y W%W")
    expect_equal(datetimeFormater("D"), "%Y-%m-%d")
    expect_equal(datetimeFormater("h"), "%Y-%m-%d %H:00")
    expect_equal(datetimeFormater("m"), "%Y-%m-%d %H:%M")
    expect_equal(datetimeFormater("s"), "%Y-%m-%d %H:%M:%S")
    expect_equal(datetimeFormater("ms"), "%Y-%m-%d %H:%M:%S.%f")
    expect_equal(datetimeFormater(NULL), "%Y-%m-%d %H:%M:%S")
})
