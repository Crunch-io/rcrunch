test_that("can create a categorical variable", {
    values <- tibble::tibble(
        value = c("1", "2", "3"),
        label = letters[1:3],
        missing = c(TRUE, TRUE, FALSE),
        scale = NA_real_,
        selected = NA,
        date = NA_character_
    )
    var <- crunch_categorical_variable(
        c(1:3, NA),
        label = "CAT",
        values = values,
        description = "D",
        notes = "N",
        path = "/Public/"
    )

    expect_equal(vec_data(var), c("1", "2", "3", NA), ignore_attr = TRUE)
    expect_equal(label(var), "CAT")
    expect_equal(values(var), values)
    expect_equal(description(var), "D")
    expect_equal(notes(var), "N")
    expect_equal(path(var), "/Public/")
})

test_that("validation runs on creation", {
    var <- crunch_categorical_variable(
        as.Date("2025-01-01"),
        label = 95,
        values = 123,
        description = ~abc,
        notes = list(),
        path = function(x) x + 1
    ) |> expect_snapshot_error()
})

test_that("validate_category_value_data coerces to character", {
    expect_equal(validate_category_value_data("x"), "x")
    expect_equal(validate_category_value_data(1), "1")
    expect_equal(validate_category_value_data(NA), NA_character_)
    expect_equal(
        validate_category_value_data(data.frame(x = "z", y = 1, z = NA)),
        data.frame(x = "z", y = "1", z = NA_character_)
    )

    expect_snapshot_error(validate_category_value_data(1.2))
    expect_snapshot_error(validate_category_value_data(list(1)))
})

test_that("validate_category_values validates and converts", {
    expect_equal(
        validate_category_values(data.frame(value = 1:3, label = letters[1:3], scale = NA, missing = c(TRUE, TRUE, FALSE), selected = NA, date = c("2025", "2026", NA)), 1:3),
        data.frame(value = c("1", "2", "3"), label = letters[1:3], scale = NA_real_, missing = c(TRUE, TRUE, FALSE), selected = NA, date = c("2025", "2026", NA))
    )

    validate_category_values(1, 1:3) |> expect_snapshot_error()
    validate_category_values(data.frame(zzz = 1), 1:3) |> expect_snapshot_error()
    validate_category_values(data.frame(value = 1.1, label = 2, missing = "x", scale = list(1), selected = "y", date = 35), 1:3) |>
        expect_snapshot_error()
    validate_category_values(data.frame(value = c(1, 1), label = c("a", "a"), scale = 1, missing = NA, selected = NA, date = "ZZ"), 1:3) |>
        expect_snapshot_error()

})

test_that("can set values of categorical variable", {
    var <- ex$cat1
    values(var) <- values(var) |> dplyr::mutate(missing = ifelse(value == 3, FALSE, missing))
    expected <- tibble::tibble(
        value = c("1", "2", "3", "99"),
        label = c("Once", "Twice", "More than twice", "Don't Know"),
        missing = c(FALSE, FALSE, FALSE, TRUE),
        scale = c(1, 2, 5, NA),
        selected = NA,
        date = NA_character_
    )
    expect_equal(values(var), expected)
    expect_equal(values(var[[1]]), expected)

    var <- ex$cat1
    expect_snapshot_error(set_values(var, values(var) |> dplyr::slice(1)))
})

test_that("can convert categorical variables to base types", {
    expect_equal(
        as.factor(ex$cat1),
        factor(c("Once", "Twice", "More than twice", NA, NA), levels = c("Once", "Twice", "More than twice"))
    )
    expect_equal(
        as.factor(ex$cat1, collapse_missing = FALSE),
        factor(c("Once", "Twice", "More than twice", "Don't Know", NA), levels = c("Once", "Twice", "More than twice", "Don't Know"))
    )

    expect_equal(as.character(ex$cat1), c("Once", "Twice", "More than twice", NA, NA))
    expect_equal(as.character(ex$cat1, collapse_missing = FALSE), c("Once", "Twice", "More than twice", "Don't Know", NA))
    expect_equal(as.character(ex$cat1, from = "value"), c("1", "2", "3", NA, NA))
    expect_equal(as.character(ex$cat1, from = "value", collapse_missing = FALSE), c("1", "2", "3", "99", NA))
    expect_equal(as.character(ex$cat2, from = "date"), c("2025", "2026", "2027", NA, NA))

    expect_equal(as.Date(ex$cat2), as.Date(c("2025-01-01", "2026-01-01", "2027-01-01", NA, NA)))
    expect_equal(as.POSIXct(ex$cat2), as.POSIXct(c("2025-01-01", "2026-01-01", "2027-01-01", NA, NA), tz = "UTC"))
    expect_equal(as.POSIXlt(ex$cat2), as.POSIXlt(c("2025-01-01", "2026-01-01", "2027-01-01", NA, NA), tz = "UTC"))
})

test_that("is.crunch_categorical_variable", {
    expect_true(is.crunch_categorical_variable(ex$cat1))
    expect_false(is.crunch_categorical_variable(ex$cat_arr1))
    expect_false(is.crunch_categorical_variable(1:3))
})

test_that("vec_ptype successes and validation", {
    expect_equal(vec_ptype2(ex$cat1, 1:3), ex$cat1[integer(0)])
    expect_equal(vec_ptype2(1:3, ex$cat1), ex$cat1[integer(0)])
    expect_equal(vec_ptype2(ex$cat1, "1"), ex$cat1[integer(0)])
    expect_equal(vec_ptype2("1", ex$cat1), ex$cat1[integer(0)])
    expect_equal(vec_ptype2(ex$cat1, ex$cat1[1] |> set_label("New!")), ex$cat1[integer(0)])

    vec_ptype2(ex$cat1, ex$cat2) |> expect_snapshot_error()
    vec_ptype2(ex$cat1, ex$num1) |> expect_snapshot_error()
})

test_that("vec_cast successes and validation", {
    expect_equal(
        vec_cast(1:3, ex$cat1),
        crunch_categorical_variable(
            1:3,
            label = label(ex$cat1),
            description = description(ex$cat1),
            values = values(ex$cat1),
            notes = notes(ex$cat1),
            path = path(ex$cat1)
        )
    )

    expect_equal(
        vec_cast("2", ex$cat1),
        crunch_categorical_variable(
            2,
            label = label(ex$cat1),
            description = description(ex$cat1),
            values = values(ex$cat1),
            notes = notes(ex$cat1),
            path = path(ex$cat1)
        )
    )

    # It may seem wrong to have vec_cast not change metadata
    # But found that (at least the way I'm currently doing) assignment
    # made it so that if we didn't retain metadata, eg `[[<-` wouldn't update
    # labels
    expect_equal(
        vec_cast(ex$cat_arr1[[1]][1:2], ex$cat1),
        crunch_categorical_variable(
            c(1, 2),
            label = label(ex$cat_arr1[[1]]),
            description = description(ex$cat_arr1[[1]]),
            values = values(ex$cat_arr1[[1]]),
            notes = notes(ex$cat_arr1[[1]]),
            path = path(ex$cat_arr1[[1]])
        )
    )

    vec_cast("1000", ex$cat1) |> expect_snapshot_error()
    vec_cast(ex$cat2, ex$cat1) |> expect_snapshot_error()
    vec_cast(ex$cat1, ex$num1) |> expect_snapshot_error()
})

test_that("is.na", {
    expect_equal(is.na(ex$cat1), c(FALSE, FALSE, FALSE, TRUE, TRUE))
})

test_that("Cannot compare categoricals", {
    expect_snapshot_error(ex$cat1[1] > ex$cat1[2])
    expect_snapshot_error(ex$cat1[1] > 1)
    expect_snapshot_error(1 > ex$cat1[1])
})

test_that("sorting categoricals uses values order", {
    expect_equal(sort(ex$cat1[3:2]), sort(ex$cat1[2:3]))
    var <- ex$cat1 |> set_values(values(ex$cat1) |> dplyr::slice(4:1))
    expect_equal(sort(var[2:3]), sort(var[3:2]))
})

test_that("equality and inequality only work when values match", {
    expect_true(ex$cat1[1] == ex$cat1[1])
    expect_false(ex$cat1[1] != ex$cat1[1])
    expect_false(ex$cat1[1] == ex$cat1[2])
    expect_true(ex$cat1[1] != ex$cat1[2])

    expect_snapshot_error(ex$cat1[1] == ex$cat2[1])
    expect_snapshot_error(ex$cat1[1] != ex$cat2[1])
})

test_that("regular & pillar printing", {
    expect_snapshot_output(tibble::tibble(cat1 = ex$cat1, cat2 = ex$cat2))
    expect_snapshot_output(ex$cat1)
    expect_snapshot_output(ex$cat2)
})

test_that("coalesce_categorical_values error messages", {
    expect_snapshot_error(coalesce_categorical_values(ex$cat1, ex$cat2, "x arg", "y arg", action = "combine"))

    # New/old values
    c1 <- crunch_categorical_variable(
        NA,
        "lbl",
        values = tibble::tibble(
            value = c("1", "2"),
            label = letters[1:2],
            missing = FALSE,
            scale = 1:2,
            selected = c(TRUE, FALSE),
            date = NA_character_
        )
    )
    c2 <- crunch_categorical_variable(
        NA,
        "lbl",
        values = tibble::tibble(
            value = c("1", "2", "3", "4", "5", "6"),
            label = letters[1:6],
            missing = FALSE,
            scale = 1:6,
            selected = rep(c(TRUE, FALSE), 3),
            date = NA_character_
        )
    )
    expect_snapshot_error(coalesce_categorical_values(c1, c2, "c1", "c2", action = "combine"))
    expect_snapshot_error(coalesce_categorical_values(c2, c1, "c2", "c1", action = "combine"))

    # Different ordering
    c1 <- crunch_categorical_variable(
        NA,
        "lbl",
        values = tibble::tibble(
            value = c("1", "2"),
            label = letters[1:2],
            missing = FALSE,
            scale = 1:2,
            selected = c(TRUE, FALSE),
            date = NA_character_
        )
    )
    c2 <- crunch_categorical_variable(
        NA,
        "lbl",
        values = values(c1) |> dplyr::slice(2:1)
    )
    expect_snapshot_error(coalesce_categorical_values(c1, c2, "c1", "c2", action = "combine"))

    # Attributes different
    c1 <- crunch_categorical_variable(
        NA,
        "lbl",
        values = tibble::tibble(
            value = c("1", "2"),
            label = letters[1:2],
            missing = FALSE,
            scale = 1:2,
            selected = c(TRUE, FALSE),
            date = NA_character_
        )
    )
    c2 <- crunch_categorical_variable(
        NA,
        "lbl",
        values =  tibble::tibble(
            value = c("1", "2"),
            label = letters[1:2],
            missing = TRUE,
            scale = NA,
            selected = c(TRUE, FALSE),
            date = NA_character_
        )
    )
    expect_snapshot_error(coalesce_categorical_values(c1, c2, "c1", "c2", action = "combine"))

    c1 <- crunch_categorical_variable(
        NA,
        "lbl",
        values = tibble::tibble(
            value = c("1", "2"),
            label = letters[1:2],
            missing = FALSE,
            scale = NA,
            selected = c(TRUE, FALSE),
            date = c("2026", "2025")
        )
    )
    c2 <- crunch_categorical_variable(
        NA,
        "lbl",
        values = tibble::tibble(
            value = c("1", "2"),
            label = letters[1:2],
            missing = FALSE,
            scale = NA,
            selected = c(FALSE, TRUE),
            date = c("2027", "2025")
        )
    )
    expect_snapshot_error(coalesce_categorical_values(c1, c2, "c1", "c2", action = "combine"))

    # Match on label but not value
    c1 <- crunch_categorical_variable(
        NA,
        "lbl",
        values = tibble::tibble(
            value = c("1", "2"),
            label = letters[1:2],
            missing = FALSE,
            scale = 1:2,
            selected = c(TRUE, FALSE),
            date = NA_character_
        )
    )
    c2 <- crunch_categorical_variable(
        NA,
        "lbl",
        values = values(c1) |> dplyr::mutate(value = paste0(.data$value, "X"))
    )
    expect_snapshot_error(coalesce_categorical_values(c1, c2, "c1", "c2", action = "combine"))

    # Match on value but not label
    c1 <- crunch_categorical_variable(
        NA,
        "lbl",
        values = tibble::tibble(
            value = c("1", "2"),
            label = letters[1:2],
            missing = FALSE,
            scale = 1:2,
            selected = c(TRUE, FALSE),
            date = NA_character_
        )
    )
    c2 <- crunch_categorical_variable(
        NA,
        "lbl",
        values = values(c1) |> dplyr::mutate(label = paste0(.data$label, "X"))
    )
    expect_snapshot_error(coalesce_categorical_values(c1, c2, "c1", "c2", action = "combine"))
})

test_that("code_to_str works as expected", {
    expect_equal(code_to_str("x"), "x")
    expect_equal(code_to_str(1.0), "1")
    expect_snapshot_error(code_to_str(1.1))
    expect_snapshot_error(code_to_str(list(x = 11)))
})

test_that("catdate_str_to_date works as expected", {
    expect_equal(catdate_str_to_date("2026"), as.Date("2026-01-01"))
    expect_equal(catdate_str_to_date("2026-02"), as.Date("2026-02-01"))
    expect_equal(catdate_str_to_date("2026-05-23"), as.Date("2026-05-23"))
    expect_equal(catdate_str_to_date("2026-W22"), as.Date("2026-06-01"))
    expect_equal(
        catdate_str_to_date(c("2025", "2025-02", NA)),
        c(as.Date("2025-01-01"), as.Date("2025-02-01"), NA)
    )

    expect_snapshot_error(catdate_str_to_date(c("XYZ", "AB")))
})

test_that("check_catdate_string works", {
    expect_equal(check_catdate_string("2026"), "2026")
    expect_equal(check_catdate_string(c("2026-01", "2026-2-1", NA)), c("2026-01", "2026-2-1", NA))
    expect_equal(check_catdate_string(c("2026-W1", "2026-W02")), c("2026-W1", "2026-W02"))

    expect_snapshot_error(check_catdate_string(c("l;kajg", "25-01")))
})

test_that("mean of categorical", {
    expect_equal(mean(ex$cat1, na.rm = TRUE), 8/3)
    expect_equal(mean(ex$cat1), NA_real_)
    expect_equal(mean(ex$cat_arr2[[1]], from = "selected", na.rm = TRUE), 0.5)
})
