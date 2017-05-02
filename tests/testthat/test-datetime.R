context("Datetime formatting")

test_that("from8601 parses", {
    expect_equal(from8601("2015-02-12T10:28:05.632000+00:00"),
        structure(list(sec = 5.632, min = 28L, hour = 10L, mday = 12L, mon = 1L,
                    year = 115L, wday = 4L, yday = 42L, isdst = 0L,
                    gmtoff = 0L),
                    .Names = c("sec", "min", "hour", "mday", "mon",
                    "year", "wday", "yday", "isdst", "gmtoff"),
                    class = c("POSIXlt", "POSIXt"), tzone="UTC"))
    expect_equal(from8601("2015-02-12T10:28:05.632000"),
        structure(list(sec = 5.632, min = 28L, hour = 10L, mday = 12L, mon = 1L,
                    year = 115L, wday = 4L, yday = 42L, isdst = 0L,
                    gmtoff = 0L),
                    .Names = c("sec", "min", "hour", "mday", "mon",
                    "year", "wday", "yday", "isdst", "gmtoff"),
                    class = c("POSIXlt", "POSIXt"), tzone="UTC"))
    expect_equal(from8601("2015-02-12T10:28:05.632000+06:00"),
        structure(list(sec = 5.632, min = 28L, hour = 4L, mday = 12L, mon = 1L,
                    year = 115L, wday = 4L, yday = 42L, isdst = 0L,
                    gmtoff = 21600L), # 6 * 60 * 60
                    .Names = c("sec", "min", "hour", "mday", "mon",
                    "year", "wday", "yday", "isdst", "gmtoff"),
                    class = c("POSIXlt", "POSIXt"), tzone="UTC"))
})

test_that("from8601 returns Date class if given only dates", {
    expect_identical(from8601("2015-02-12"), as.Date("2015-02-12"))
    expect_identical(from8601(c("2015-02-12", NA)), as.Date(c("2015-02-12", NA)))
})
