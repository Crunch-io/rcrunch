test_that("is.crunch_array_variable works", {
    expect_true(is.crunch_array_variable(ex$cat_arr1))
    expect_true(is.crunch_array_variable(ex$num_arr1))
    expect_false(is.crunch_array_variable(1:3))
})

test_that("validate_axes failure messages when axes is NULL", {
    # without crunch variables
    validate_axes(NULL, tibble::tibble(x = "a"), "numeric") |>
        expect_snapshot_error()

    # with crunch variables of different types
    validate_axes(NULL, tibble::tibble(x = ex$num_arr1[[1]], y = ex$num_arr2[[1]]), "numeric") |>
        expect_snapshot_error()

    # with non-matching second dimension and duplicated item labels
    validate_axes(NULL, tibble::tibble(x = ex$num_arr1, y = ex$num_arr1[, 1]), "numeric") |>
        expect_snapshot_error()

    # with 3rd dimension
    validate_axes(NULL, tibble::tibble(x = ex$num_arr2, y = ex$num_arr2 |> set_label("another")), "numeric") |>
        expect_snapshot_error()
})

test_that("validate_axes failure messages when axes is provided", {
    # Non list
    validate_axes(123, tibble::tibble(x = 1:3, y = 2:4), "numeric") |>
        expect_snapshot_error()

    # Empty data.frame
    validate_axes(list(), tibble::tibble(x = 1:3, y = 2:4), "numeric") |>
        expect_snapshot_error()

    # non coercible axes, value, label & description + Missing columns
    validate_axes(
        list(
            tibble::tibble(value = c(1.2, 1.3), label = list("a", "b"), description = c(~1, ~2)),
            tibble::tibble()
        ),
        tibble::tibble(x = 1:3, y = 2:4),
        "numeric"
    ) |>
        expect_snapshot_error()

    # Invalid axis, mismatched value to data (outer)
    validate_axes(
        list(tibble::tibble(value = c("a", "b"), label = c("a", "b"), description = NA)),
        tibble::tibble(x = 1:3, y = 2:4),
        "numeric"
    ) |>
        expect_snapshot_error()

    # Invalid axis, mismatched value to data (outer & inner)
    validate_axes(
        list(
            tibble::tibble(value = c("a", "b"), label = c("a", "b"), description = NA),
            tibble::tibble(value = c("x", "y"), label = c("a", "b"), description = NA)
        ),
        tibble::tibble(r = tibble::tibble(c = 1, d = 2), s = tibble::tibble(c = 1, d = 2)),
        "numeric"
    ) |>
        expect_snapshot_error()


    # Non unique value & non-unique label
    validate_axes(
        list(tibble::tibble(value = c("a", "a"), label = c("b", "b"), description = NA)),
        tibble::tibble(a = 1:3, b = 1:3) |> setNames(c("a", "a")),
        "numeric"
    ) |>
        expect_snapshot_error()
})

test_that("validate and coerce in validate_axes()", {
    validated <- validate_axes(
        NULL,
        tibble::tibble(
            x = crunch_numeric_variable(1:3, label = "xl", description = "xd", notes = "xn", path = "/Public"),
            y = crunch_numeric_variable(2:4, label = "yl", description = "yd", notes = "yn", path = "/Hidden")
        ),
        "numeric"
    )
    expect_s3_class(validated$x, "crunch_numeric_variable")
    expect_equal(label(validated$x), "xl")
    expect_equal(description(validated$x), "xd")
    expect_null(notes(validated$x))
    expect_null(path(validated$x))
    expect_equal(label(validated$y), "yl")
    expect_equal(description(validated$y), "yd")
    expect_null(notes(validated$y))
    expect_null(path(validated$y))


    validated <- validate_axes(
        list(tibble::tibble(value = c("x", "y"), label = c("X", "Y"), description = NA)),
        tibble::tibble(x = 1:3, y = 2:4),
        "numeric"
    )
    expect_s3_class(validated$x, "crunch_numeric_variable")
    expect_null(description(validated$x))
    expect_equal(vec_data(validated$x), 1:3)
    expect_s3_class(validated$y, "crunch_numeric_variable")
    expect_null(description(validated$y))
    expect_equal(vec_data(validated$y), 2:4)
})

test_that("can get axes from crunch array", {
    expect_equal(
        axes(ex$cat_arr1),
        list(tibble::tribble(
            ~value, ~label, ~description,
            "cat1", "ARR 1", NA_character_,
            "cat2", "ARR 2", NA_character_
        ))
    )

    expect_equal(
        axes(ex$num_arr2),
        list(
            tibble::tribble(
                ~value, ~label,  ~description,
                "a", "AX A", NA_character_,
                "b", "AX B", NA_character_,
                "c", "AX C", NA_character_
            ),
            tibble::tribble(
                ~value, ~label,  ~description,
                "x", "AX X", NA_character_,
                "y", "AX Y", NA_character_
            )
        )

    )
})

test_that("can convert categorical array to factor", {
    expect_equal(
        as.factor(ex$cat_arr1),
        tibble::tibble(
            cat1 = factor(c("Once", "Twice", "More than twice", NA, NA), levels = c("Once", "Twice", "More than twice")),
            cat2 = factor(c("Twice", "Twice", "Twice", "Once", "Once"), levels = c("Once", "Twice", "More than twice"))
        )
    )

    expect_equal(
        as.factor(ex$cat_arr1, collapse_missing = FALSE),
        tibble::tibble(
            cat1 = factor(c("Once", "Twice", "More than twice", "Don't Know", NA), levels = c("Once", "Twice", "More than twice", "Don't Know")),
            cat2 = factor(c("Twice", "Twice", "Twice", "Once", "Once"), levels = c("Once", "Twice", "More than twice", "Don't Know"))
        )
    )
})

test_that("can convert array to character", {
    expect_equal(
        as.character(ex$cat_arr1),
        tibble::tibble(
            cat1 = c("Once", "Twice", "More than twice", NA, NA),
            cat2 = c("Twice", "Twice", "Twice", "Once", "Once")
        )
    )

    expect_equal(
        as.character(ex$cat_arr1, collapse_missing = FALSE),
        tibble::tibble(
            cat1 = c("Once", "Twice", "More than twice", "Don't Know", NA),
            cat2 = c("Twice", "Twice", "Twice", "Once", "Once")
        )
    )

    expect_equal(
        as.character(ex$num_arr1),
        tibble::tibble(
            num1 = c("1.25", "35", "1.4"),
            num2 = c(NA, "100", "9")
        )
    )
})

test_that("can convert array to numeric", {
    expect_equal(
        as.double(ex$cat_arr1),
        tibble::tibble(
            cat1 = c(1, 2, 3, NA, NA),
            cat2 = c(2, 2, 2, 1, 1)
        )
    )

    expect_equal(
        as.double(ex$cat_arr1, from = "scale"),
        tibble::tibble(
            cat1 = c(1, 2, 5, NA, NA),
            cat2 = c(2, 2, 2, 1, 1)
        )
    )


    expect_equal(
        as.double(ex$num_arr1),
        tibble::tibble(
            num1 = c(1.25, 35, 1.4),
            num2 = c(NA, 100, 9)
        )
    )

    expect_equal(
        as.numeric(ex$num_arr2),
        tibble::tibble(
            a = tibble::tibble(x = c(22, 42, 43), y = c(-1, -2, -3)),
            b = tibble::tibble(x = c(99, 98, 97), y = c(-2, -2, -2)),
            c = tibble::tibble(x = c(50, 51, 50), y = c(-5, -19, -20))
        )
    )
})

test_that("can convert categorical array to logical", {
    expect_equal(
        as.logical(ex$cat_arr2),
        tibble::tibble(
            mr1 = c(TRUE, TRUE, FALSE, FALSE, NA),
            mr2 = c(FALSE, NA, FALSE, NA, TRUE),
            mr3 = c(NA, NA, NA, NA, NA)
        )
    )
})

test_that("as.data.frame & as_tibble on array variables", {
    expect_equal(
        as.data.frame(ex$cat_arr1),
        data.frame(
            cat1 = ex$cat_arr1[[1]],
            cat2 = ex$cat_arr1[[2]]
        )
    )

    expect_equal(
        as_tibble(ex$cat_arr1),
        tibble::tibble(
            cat1 = ex$cat_arr1[[1]],
            cat2 = ex$cat_arr1[[2]]
        )
    )
})

test_that("names validates & can set names on array", {
    var <- ex$cat_arr1
    expect_snapshot_error(names(ex$cat_arr1) <- NULL)
    expect_snapshot_error(names(ex$cat_arr1) <- c("a", "a"))
    expect_snapshot_error(names(ex$cat_arr1) <- c("a"))

    var <- ex$cat_arr1
    names(ex$cat_arr1) <- c("a", "b")
    expect_equal(names(ex$cat_arr1), c("a", "b"))
})

test_that("pillar print methods snapshot - cat_arr1", {
    expect_snapshot_output(ex$cat_arr1)
})

test_that("pillar print methods snapshot - cat_arr2", {
    expect_snapshot_output(ex$cat_arr2)
})

test_that("pillar print methods snapshot - cat_arr3", {
    expect_snapshot_output(ex$cat_arr3)
})
