test_that("can create crunch_categorical_array_variable - From base types (1D)", {
    var <- crunch_categorical_array_variable(
        tibble::tibble(
            cat1 = c("1", "2"),
            cat2 = c(NA, NA),
            cat3 = c(2, 3)
        ),
        label = "Cat Array 1",
        axes = list(tibble::tibble(
            value = c("cat1", "cat2", "cat3"),
            label = c("ARR 1", "ARR 2", "ARR 3"),
            description = c("DESC 1", "DESC 2", "DESC 3")
        )),
        values = .cat_arr1_values,
        notes = "NOTES",
        description = "DESC",
        path = "/Public/d1"
    )
    expect_s3_class(var, "crunch_categorical_array_variable")
    expect_equal(ncol(var), 3)
    expect_equal(names(var), c("cat1", "cat2", "cat3"))
    expect_equal(label(var), "Cat Array 1")
    expect_equal(notes(var), "NOTES")
    expect_equal(description(var), "DESC")
    expect_equal(path(var), "/Public/d1")
    expect_equal(
        values(var),
        .cat_arr1_values |>
            dplyr::mutate(value = code_to_str(value), date = as.character(date))
    )
    expect_s3_class(var[[1]], "crunch_categorical_variable")
    expect_equal(typeof(var[[1]]), "character")
    expect_equal(var[[1]], crunch_categorical_variable(
        c("1", "2"),
        label = "ARR 1",
        values = .cat_arr1_values,
        description = "DESC 1"
    ))
    expect_s3_class(var[[2]], "crunch_categorical_variable")
    expect_equal(typeof(var[[2]]), "character")
    expect_equal(var[[2]], crunch_categorical_variable(
        c(NA_character_, NA_character_),
        label = "ARR 2",
        values = .cat_arr1_values,
        description = "DESC 2"
    ))
    expect_s3_class(var[[3]], "crunch_categorical_variable")
    expect_equal(typeof(var[[3]]), "character")
    expect_equal(var[[3]], crunch_categorical_variable(
        c("2", "3"),
        label = "ARR 3",
        values = .cat_arr1_values,
        description = "DESC 3"
    ))
})

test_that("can create crunch_categorical_array_variable - From crunch_categorical_variables (1D)", {
    var <- crunch_categorical_array_variable(
        tibble::tibble(
            cat1 = crunch_categorical_variable(
                1:2,
                label = "CAT 1",
                values = .cat_arr1_values,
                description = "ARR 1",
                notes = "NOTES",
                path = "/Public/d1"
            )),
        label = "Cat Array 1"
    )
    expect_s3_class(var, "crunch_categorical_array_variable")
    expect_equal(ncol(var), 1)
    expect_equal(names(var), "cat1")
    expect_equal(
        values(var),
        .cat_arr1_values |>
            dplyr::mutate(value = code_to_str(value), date = as.character(date))
    )
    expect_s3_class(var[[1]], "crunch_categorical_variable")
    expect_equal(typeof(var[[1]]), "character")
    expect_equal(var[[1]], crunch_categorical_variable(
        c("1", "2"),
        label = "CAT 1",
        values = .cat_arr1_values,
        description = "ARR 1"
    ))

    # From data.frame of base types (2D)
    var <- crunch_categorical_array_variable(
        tibble::tibble(
            cat1 = tibble::tibble(x = c("1", "2")),
            cat2 = tibble::tibble(x = c(NA, NA)),
            cat3 = tibble::tibble(x = c(2, 3))
        ),
        label = "Cat Array 1",
        axes = list(
            tibble::tibble(
                value = c("cat1", "cat2", "cat3"),
                label = c("ARR 1", "ARR 2", "ARR 3"),
                description = c("DESC 1", "DESC 2", "DESC 3")
            ),
            tibble::tibble(
                value = c("x"),
                label = c("ARR X"),
                description = c("DESC X")
            )
        ),
        values = .cat_arr1_values,
        notes = "NOTES",
        description = "DESC",
        path = "/Public/d1"
    )
    expect_s3_class(var, "crunch_categorical_array_variable")
    expect_equal(ncol(var), 3)
    expect_equal(names(var), c("cat1", "cat2", "cat3"))
    expect_equal(label(var), "Cat Array 1")
    expect_equal(notes(var), "NOTES")
    expect_equal(description(var), "DESC")
    expect_equal(path(var), "/Public/d1")
    expect_equal(
        values(var),
        .cat_arr1_values |>
            dplyr::mutate(value = code_to_str(value), date = as.character(date))
    )
    expect_s3_class(var[[1]], "crunch_categorical_array_variable")
    expect_equal(typeof(var[[1]][[1]]), "character")
    expect_equal(var[[1]], crunch_categorical_array_variable(
        tibble::tibble(x = c("1", "2")),
        axes = list(tibble::tibble(
            value = c("x"),
            label = c("ARR X"),
            description = c("DESC X")
        )),
        label = "ARR 1",
        values = .cat_arr1_values,
        description = "DESC 1"
    ))
    expect_s3_class(var[[2]], "crunch_categorical_array_variable")
    expect_equal(typeof(var[[2]][[1]]), "character")
    expect_equal(var[[2]], crunch_categorical_array_variable(
        tibble::tibble(x = c(NA_character_, NA_character_)),
        axes = list(tibble::tibble(
            value = c("x"),
            label = c("ARR X"),
            description = c("DESC X")
        )),
        label = "ARR 2",
        values = .cat_arr1_values,
        description = "DESC 2"
    ))
    expect_s3_class(var[[3]], "crunch_categorical_array_variable")
    expect_equal(typeof(var[[3]][[1]]), "character")
    expect_equal(var[[3]], crunch_categorical_array_variable(
        tibble::tibble(x = c("2", "3")),
        axes = list(tibble::tibble(
            value = c("x"),
            label = c("ARR X"),
            description = c("DESC X")
        )),
        label = "ARR 3",
        values = .cat_arr1_values,
        description = "DESC 3"
    ))

})

test_that("can create crunch_categorical_array_variable - From data.frame of base types (2D)", {
    var <- crunch_categorical_array_variable(
        tibble::tibble(
            cat1 = tibble::tibble(x = c("1", "2")),
            cat2 = tibble::tibble(x = c(NA, NA)),
            cat3 = tibble::tibble(x = c(2, 3))
        ),
        label = "Cat Array 1",
        axes = list(
            tibble::tibble(
                value = c("cat1", "cat2", "cat3"),
                label = c("ARR 1", "ARR 2", "ARR 3"),
                description = c("DESC 1", "DESC 2", "DESC 3")
            ),
            tibble::tibble(
                value = c("x"),
                label = c("ARR X"),
                description = c("DESC X")
            )
        ),
        values = .cat_arr1_values,
        notes = "NOTES",
        description = "DESC",
        path = "/Public/d1"
    )
    expect_s3_class(var, "crunch_categorical_array_variable")
    expect_equal(ncol(var), 3)
    expect_equal(names(var), c("cat1", "cat2", "cat3"))
    expect_equal(label(var), "Cat Array 1")
    expect_equal(notes(var), "NOTES")
    expect_equal(description(var), "DESC")
    expect_equal(path(var), "/Public/d1")
    expect_equal(
        values(var),
        .cat_arr1_values |>
            dplyr::mutate(value = code_to_str(value), date = as.character(date))
    )
    expect_s3_class(var[[1]], "crunch_categorical_array_variable")
    expect_equal(typeof(var[[1]][[1]]), "character")
    expect_equal(var[[1]], crunch_categorical_array_variable(
        tibble::tibble(x = c("1", "2")),
        axes = list(tibble::tibble(
            value = c("x"),
            label = c("ARR X"),
            description = c("DESC X")
        )),
        label = "ARR 1",
        values = .cat_arr1_values,
        description = "DESC 1"
    ))
    expect_s3_class(var[[2]], "crunch_categorical_array_variable")
    expect_equal(typeof(var[[2]][[1]]), "character")
    expect_equal(var[[2]], crunch_categorical_array_variable(
        tibble::tibble(x = c(NA_character_, NA_character_)),
        axes = list(tibble::tibble(
            value = c("x"),
            label = c("ARR X"),
            description = c("DESC X")
        )),
        label = "ARR 2",
        values = .cat_arr1_values,
        description = "DESC 2"
    ))
    expect_s3_class(var[[3]], "crunch_categorical_array_variable")
    expect_equal(typeof(var[[3]][[1]]), "character")
    expect_equal(var[[3]], crunch_categorical_array_variable(
        tibble::tibble(x = c("2", "3")),
        axes = list(tibble::tibble(
            value = c("x"),
            label = c("ARR X"),
            description = c("DESC X")
        )),
        label = "ARR 3",
        values = .cat_arr1_values,
        description = "DESC 3"
    ))
})

test_that("can create crunch_categorical_array_variable - From data.frame of crunch_categorical_variables (2D)", {
    var <- crunch_categorical_array_variable(
        tibble::tibble(
            cat1 = tibble::tibble(x = crunch_categorical_variable(c("1", "2"), label = "ARR X", description = "DESC X", values = .cat_arr1_values)),
            cat2 = tibble::tibble(x = crunch_categorical_variable(c(NA, NA), label = "ARR X", description = "DESC X", values = .cat_arr1_values)),
            cat3 = tibble::tibble(x = crunch_categorical_variable(c(2, 3), label = "ARR X", description = "DESC X", values = .cat_arr1_values))
        ),
        label = "Cat Array 1",
        axes = list(
            tibble::tibble(
                value = c("cat1", "cat2", "cat3"),
                label = c("ARR 1", "ARR 2", "ARR 3"),
                description = c("DESC 1", "DESC 2", "DESC 3")
            ),
            tibble::tibble(
                value = c("x"),
                label = c("ARR X"),
                description = c("DESC X")
            )
        ),
        values = .cat_arr1_values,
        notes = "NOTES",
        description = "DESC",
        path = "/Public/d1"
    )
    expect_s3_class(var, "crunch_categorical_array_variable")
    expect_equal(ncol(var), 3)
    expect_equal(names(var), c("cat1", "cat2", "cat3"))
    expect_equal(label(var), "Cat Array 1")
    expect_equal(notes(var), "NOTES")
    expect_equal(description(var), "DESC")
    expect_equal(path(var), "/Public/d1")
    expect_equal(
        values(var),
        .cat_arr1_values |>
            dplyr::mutate(value = code_to_str(value), date = as.character(date))
    )
    expect_s3_class(var[[1]], "crunch_categorical_array_variable")
    expect_equal(typeof(var[[1]][[1]]), "character")
    expect_equal(var[[1]], crunch_categorical_array_variable(
        tibble::tibble(x = c("1", "2")),
        axes = list(tibble::tibble(
            value = c("x"),
            label = c("ARR X"),
            description = c("DESC X")
        )),
        label = "ARR 1",
        values = .cat_arr1_values,
        description = "DESC 1"
    ))
    expect_s3_class(var[[2]], "crunch_categorical_array_variable")
    expect_equal(typeof(var[[2]][[1]]), "character")
    expect_equal(var[[2]], crunch_categorical_array_variable(
        tibble::tibble(x = c(NA_character_, NA_character_)),
        axes = list(tibble::tibble(
            value = c("x"),
            label = c("ARR X"),
            description = c("DESC X")
        )),
        label = "ARR 2",
        values = .cat_arr1_values,
        description = "DESC 2"
    ))
    expect_s3_class(var[[3]], "crunch_categorical_array_variable")
    expect_equal(typeof(var[[3]][[1]]), "character")
    expect_equal(var[[3]], crunch_categorical_array_variable(
        tibble::tibble(x = c("2", "3")),
        axes = list(tibble::tibble(
            value = c("x"),
            label = c("ARR X"),
            description = c("DESC X")
        )),
        label = "ARR 3",
        values = .cat_arr1_values,
        description = "DESC 3"
    ))
})

test_that("can create crunch_categorical_array_variable - From crunch_categorical_array_variables explicit axes (2D)", {
    var <- crunch_categorical_array_variable(
        tibble::tibble(
            cat1 = crunch_categorical_array_variable(
                tibble::tibble(x = crunch_categorical_variable(c("1", "2"), label = "ARR X", description = "DESC X", values = .cat_arr1_values)),
                label = "ARR 1",
                description = "DESC 1"
            ),
            cat2 = crunch_categorical_array_variable(
                tibble::tibble(x = crunch_categorical_variable(c(NA, NA), label = "ARR X", description = "DESC X", values = .cat_arr1_values)),
                label = "ARR 2",
                description = "DESC 2"
            ),
            cat3 = crunch_categorical_array_variable(
                tibble::tibble(x = crunch_categorical_variable(c(2, 3), label = "ARR X", description = "DESC X", values = .cat_arr1_values)),
                label = "ARR 3",
                description = "DESC 3"
            ),
        ),
        label = "Cat Array 1",
        axes = list(
            tibble::tibble(
                value = c("cat1", "cat2", "cat3"),
                label = c("ARR 1", "ARR 2", "ARR 3"),
                description = c("DESC 1", "DESC 2", "DESC 3")
            ),
            tibble::tibble(
                value = c("x"),
                label = c("ARR X"),
                description = c("DESC X")
            )
        ),
        values = .cat_arr1_values,
        notes = "NOTES",
        description = "DESC",
        path = "/Public/d1"
    )
    expect_s3_class(var, "crunch_categorical_array_variable")
    expect_equal(ncol(var), 3)
    expect_equal(names(var), c("cat1", "cat2", "cat3"))
    expect_equal(label(var), "Cat Array 1")
    expect_equal(notes(var), "NOTES")
    expect_equal(description(var), "DESC")
    expect_equal(path(var), "/Public/d1")
    expect_equal(
        values(var),
        .cat_arr1_values |>
            dplyr::mutate(value = code_to_str(value), date = as.character(date))
    )
    expect_s3_class(var[[1]], "crunch_categorical_array_variable")
    expect_equal(typeof(var[[1]][[1]]), "character")
    expect_equal(var[[1]], crunch_categorical_array_variable(
        tibble::tibble(x = c("1", "2")),
        axes = list(tibble::tibble(
            value = c("x"),
            label = c("ARR X"),
            description = c("DESC X")
        )),
        label = "ARR 1",
        values = .cat_arr1_values,
        description = "DESC 1"
    ))
    expect_s3_class(var[[2]], "crunch_categorical_array_variable")
    expect_equal(typeof(var[[2]][[1]]), "character")
    expect_equal(var[[2]], crunch_categorical_array_variable(
        tibble::tibble(x = c(NA_character_, NA_character_)),
        axes = list(tibble::tibble(
            value = c("x"),
            label = c("ARR X"),
            description = c("DESC X")
        )),
        label = "ARR 2",
        values = .cat_arr1_values,
        description = "DESC 2"
    ))
    expect_s3_class(var[[3]], "crunch_categorical_array_variable")
    expect_equal(typeof(var[[3]][[1]]), "character")
    expect_equal(var[[3]], crunch_categorical_array_variable(
        tibble::tibble(x = c("2", "3")),
        axes = list(tibble::tibble(
            value = c("x"),
            label = c("ARR X"),
            description = c("DESC X")
        )),
        label = "ARR 3",
        values = .cat_arr1_values,
        description = "DESC 3"
    ))
})

test_that("can create crunch_categorical_array_variable - From crunch_categorical_array_variables implied axes (2D)", {
    var <- crunch_categorical_array_variable(
        tibble::tibble(
            cat1 = crunch_categorical_array_variable(
                tibble::tibble(x = crunch_categorical_variable(c("1", "2"), label = "ARR X", description = "DESC X", values = .cat_arr1_values)),
                label = "ARR 1",
                description = "DESC 1"
            ),
            cat2 = crunch_categorical_array_variable(
                tibble::tibble(x = crunch_categorical_variable(c(NA, NA), label = "ARR X", description = "DESC X", values = .cat_arr1_values)),
                label = "ARR 2",
                description = "DESC 2"
            ),
            cat3 = crunch_categorical_array_variable(
                tibble::tibble(x = crunch_categorical_variable(c(2, 3), label = "ARR X", description = "DESC X", values = .cat_arr1_values)),
                label = "ARR 3",
                description = "DESC 3"
            ),
        ),
        label = "Cat Array 1",
        values = .cat_arr1_values,
        notes = "NOTES",
        description = "DESC",
        path = "/Public/d1"
    )
    expect_s3_class(var, "crunch_categorical_array_variable")
    expect_equal(ncol(var), 3)
    expect_equal(names(var), c("cat1", "cat2", "cat3"))
    expect_equal(label(var), "Cat Array 1")
    expect_equal(notes(var), "NOTES")
    expect_equal(description(var), "DESC")
    expect_equal(path(var), "/Public/d1")
    expect_equal(
        values(var),
        .cat_arr1_values |>
            dplyr::mutate(value = code_to_str(value), date = as.character(date))
    )
    expect_s3_class(var[[1]], "crunch_categorical_array_variable")
    expect_equal(typeof(var[[1]][[1]]), "character")
    expect_equal(var[[1]], crunch_categorical_array_variable(
        tibble::tibble(x = c("1", "2")),
        axes = list(tibble::tibble(
            value = c("x"),
            label = c("ARR X"),
            description = c("DESC X")
        )),
        label = "ARR 1",
        values = .cat_arr1_values,
        description = "DESC 1"
    ))
    expect_s3_class(var[[2]], "crunch_categorical_array_variable")
    expect_equal(typeof(var[[2]][[1]]), "character")
    expect_equal(var[[2]], crunch_categorical_array_variable(
        tibble::tibble(x = c(NA_character_, NA_character_)),
        axes = list(tibble::tibble(
            value = c("x"),
            label = c("ARR X"),
            description = c("DESC X")
        )),
        label = "ARR 2",
        values = .cat_arr1_values,
        description = "DESC 2"
    ))
    expect_s3_class(var[[3]], "crunch_categorical_array_variable")
    expect_equal(typeof(var[[3]][[1]]), "character")
    expect_equal(var[[3]], crunch_categorical_array_variable(
        tibble::tibble(x = c("2", "3")),
        axes = list(tibble::tibble(
            value = c("x"),
            label = c("ARR X"),
            description = c("DESC X")
        )),
        label = "ARR 3",
        values = .cat_arr1_values,
        description = "DESC 3"
    ))
})

test_that("is.crunch_categorical_array_variable works", {
    expect_true(is.crunch_categorical_array_variable(ex$cat_arr1))
    expect_true(is.crunch_categorical_array_variable(ex$cat_arr2))
    expect_false(is.crunch_categorical_array_variable(1:3))
    expect_false(is.crunch_categorical_array_variable(ex$num_arr1))
})

test_that("Can get and set values", {
    expect_equal(values(ex$cat_arr1), tibble::tibble(
        value = c("1", "2", "3", "99"),
        label = c("Once", "Twice", "More than twice", "Don't Know"),
        missing = c(FALSE, FALSE, FALSE, TRUE),
        scale = c(1, 2, 5, NA),
        selected = NA,
        date = NA_character_
    ))

    var <- ex$cat_arr1
    values(var) <- values(var) |>
        dplyr::mutate(scale = scale * 2)

    expect_equal(values(var), tibble::tibble(
        value = c("1", "2", "3", "99"),
        label = c("Once", "Twice", "More than twice", "Don't Know"),
        missing = c(FALSE, FALSE, FALSE, TRUE),
        scale = c(1, 2, 5, NA) * 2,
        selected = NA,
        date = NA_character_
    ))

    var2 <- set_values(ex$cat_arr1, values(ex$cat_arr1) |> dplyr::mutate(label = paste0(label, "!!!")))
    expect_equal(values(var2), tibble::tibble(
        value = c("1", "2", "3", "99"),
        label = paste0(c("Once", "Twice", "More than twice", "Don't Know"), "!!!"),
        missing = c(FALSE, FALSE, FALSE, TRUE),
        scale = c(1, 2, 5, NA),
        selected = NA,
        date = NA_character_
    ))
})

test_that("Setting values runs validation", {
    incomplete_vals <- values(ex$cat_arr1) |> dplyr::slice(1)
    ex$cat_arr1 |> set_values(incomplete_vals) |>
        expect_snapshot_error()

    dup_labels_vals <- values(ex$cat_arr1) |>
        dplyr::mutate(label = "a")
    ex$cat_arr1 |> set_values(dup_labels_vals) |>
        expect_snapshot_error()

})

test_that("Validation messages in crunch_categorical_array_variable make sense ", {
    crunch_categorical_array_variable(
        x = tibble::tibble(cat1 = 1:3, cat2 = 2:4),
        label = NULL,
        axes = 123,
        values = 234
    ) |> expect_snapshot_error()
})

test_that("validate_array_category_data validation messages: NULL values", {
    # bad types
    validate_array_category_data(NULL, tibble::tibble(x = 1:3)) |>
        expect_snapshot_error()

    # mismatched values
    validate_array_category_data(NULL, tibble::tibble(
        x = ex$cat_arr1[[1]],
        y = ex$cat_arr2[[1]]
    )) |>
        expect_snapshot_error()
})

test_that("validate_array_category_data validation messages: values with crunch variables", {
    # mixed types
    validate_array_category_data(values(ex$cat_arr1[[1]]), tibble::tibble(
        x = seq_along(ex$cat_arr1[[1]]),
        y = ex$cat_arr1[[1]]
    )) |>
        expect_snapshot_error()

    # mismatched values and provided values
    validate_array_category_data(values(ex$cat_arr1[[1]]), tibble::tibble(
        x = ex$cat_arr2[[1]],
        y = ex$cat_arr2[[2]]
    )) |>
        expect_snapshot_error()
})

test_that("validate_array_category_data validation messages: values with base types", {
    # passes on to validate_category_data
    validate_array_category_data(values(ex$cat_arr1[[1]]), tibble::tibble(x = 1:100)) |>
        expect_snapshot_error()
})

test_that("list dimensional subsets work in `[[`", {
    var <- ex$cat_arr3
    var1d <- ex$cat_arr1

    expect_equal(var1d[[list("cat1")]], var1d[["cat1"]])
    expect_equal(var[[list("x", NULL)]], var[["x"]])
    expect_equal(var[[list("z", "cat2")]], var[["z"]][["cat2"]])
    expect_equal(var[[1, list("z", "cat2")]], var[["z"]][[1, "cat2"]])
    expect_equal(var[[list(NULL, "cat2")]], crunch_categorical_array_variable(
        as.data.frame(lapply(var, function(x) x[["cat2"]])),
        label = label(var[[1]][["cat2"]]),
        description = description(var[[1]][["cat2"]]),
        values = values(var),
        axes = axes(var)[1]
    ))

    expect_snapshot_error(var1d[[list("cat1", NULL)]])
    expect_snapshot_error(var1d[[list(NULL)]])
    expect_snapshot_error(var1d[[list(c("cat1", "cat2"))]])
    expect_snapshot_error(var1d[[list(~1)]])
    expect_snapshot_error(var[[list("x")]])
    expect_snapshot_error(var[[list(NULL, NULL)]])
    expect_snapshot_error(var[[list(NULL, c("cat1", "cat2"))]])
    expect_snapshot_error(var1d[[list(NULL, list(NULL))]])
})

test_that("`[[<-` & `$<-` prevent adding columns with values or axes that don't work or duplicate labels", {
    var <- ex$cat_arr1
    var2d <- ex$cat_arr3
    expect_snapshot_error(var[["cat3"]] <- ex$cat_arr2$mr1) # Defined labels don't match
    expect_snapshot_error(var$cat3 <- seq_len(nrow(var)) + 99) # Extra values
    expect_snapshot_error(var$cat3 <- ex$num1[c(1:4, 1)]) # Non-categorical crunch variable
    expect_snapshot_error(var$cat3 <- ex$cat_arr1) # 2D adding into 1D
    expect_snapshot_error(var2d$z2 <- ex$cat_arr1[[1]]) # 1D adding into 2D
    expect_snapshot_error(var2d$z2 <- ex$cat_arr1[1]) # 2D adding into non-matching 2D
    expect_snapshot_error(var2d$z$cat3 <- NA) # Add an item to only 1 of inner arrays 2D
    expect_snapshot_error(var2d$z$cat2 <- NULL) # Remove an item from only 1 of inner arrays 2D
    expect_snapshot_error(var[["cat3"]] <- ex$cat_arr1$cat1) # Duplicate label
})

test_that("but can add and remove columns with `[[<-` (1D)", {
    # Non-crunch variable (1D)
    var <- ex$cat_arr1
    var$cat3 <- 1
    expect_equal(var$cat3, new_crunch_categorical_variable(rep("1", nrow(var)), label = "cat3", values = values(var)))

    # Crunch variable (1D)
    var <- ex$cat_arr1
    var$cat3 <- var$cat1 |> set_label("ARR 3")
    expect_equal(var$cat3, var$cat1 |> set_label("ARR 3"))

    # Removing variable (1D)
    var <- ex$cat_arr1
    var$cat1 <- NULL
    expect_equal(names(var), "cat2")

    # Partial column: Non-crunch variable (1D)
    var <- ex$cat_arr1
    var[[1, "cat2"]] <- 99
    expect_equal(var$cat2[1], new_crunch_categorical_variable("99", label = "ARR 2", values = values(var)))

    # Partial column: crunch variable (1D)
    var <- ex$cat_arr1
    var[[1, "cat2"]] <- var$cat1[[1]]
    expect_equal(var$cat2[1], new_crunch_categorical_variable(var$cat1[[1]], label = "ARR 2", values = values(var)))
})

test_that("and can add and remove columns with `[[<-` (2D)", {
    # Non-crunch variable (2D) addition in first dimension
    var <- ex$cat_arr3
    var$z2 <- data.frame(cat1 = rep("2", nrow(var)), cat2 = rep(99, nrow(var)))
    expect_equal(var$z2, crunch_categorical_array_variable(
        data.frame(cat1 = rep("2", nrow(var)), cat2 = rep(99, nrow(var))),
        label = "z2",
        values = values(var),
        axes = axes(var[[1]])
    ))

    # Crunch variable (2D) addition in first dimension
    var <- ex$cat_arr3
    var[["z2"]] <- crunch_categorical_array_variable(
        data.frame(cat1 = rep("2", nrow(var)), cat2 = rep(99, nrow(var))),
        label = "cat3",
        values = values(var),
        axes = axes(var[[1]])
    )
    expect_equal(var$z2, crunch_categorical_array_variable(
        data.frame(cat1 = rep("2", nrow(var)), cat2 = rep(99, nrow(var))),
        label = "cat3",
        values = values(var),
        axes = axes(var[[1]])
    ))

    # Removing variable (2D) in first dimension
    var <- ex$cat_arr3
    var$z <- NULL
    expect_equal(names(var), c("x", "y"))

    # Partial column: base data.frame (2D) in first dimension
    var <- ex$cat_arr3
    var[[2, "z"]] <- data.frame(cat1 = "2", cat2 = 99)
    expect_equal(var$z[2, ], crunch_categorical_array_variable(
        data.frame(cat1 = "2", cat2 = 99),
        label = label(var$z),
        values = values(var),
        axes = axes(var[[1]])
    ))

    # Partial column: Crunch variable (2D) in first dimension
    var <- ex$cat_arr3
    var[[3, "z"]] <- var[[1, "x"]]
    expect_equal(var$z[3, ], crunch_categorical_array_variable(
        var[[1, "x"]],
        label = label(var$z),
        values = values(var),
        axes = axes(var[[1]])
    ))
})

test_that("`[[<-` with list subscripts", {
    var <- ex$cat_arr1
    var[[list("cat1")]] <- NULL
    expect_equal(
        axes(var),
        list(tibble::tibble(value = "cat2", label = "ARR 2", description = NA_character_))
    )
    var <- ex$cat_arr1
    var[[, list("cat1")]] <- NULL
    expect_equal(
        axes(var),
        list(tibble::tibble(value = "cat2", label = "ARR 2", description = NA_character_))
    )


    var <- ex$cat_arr3
    var[[list("x", NULL)]] <- NULL
    expect_equal(
        axes(var),
        list(
            tibble::tibble(
                value = c("y", "z"),
                label = c("ARR Y", "ARR Z"),
                description = NA_character_
            ),
            tibble::tibble(
                value = c("cat1", "cat2"),
                label = c("ARR 1", "ARR 2"),
                description = NA_character_
            )

        )
    )
    var <- ex$cat_arr3
    var[[,list("x", NULL)]] <- NULL
    expect_equal(
        axes(var),
        list(
            tibble::tibble(
                value = c("y", "z"),
                label = c("ARR Y", "ARR Z"),
                description = NA_character_
            ),
            tibble::tibble(
                value = c("cat1", "cat2"),
                label = c("ARR 1", "ARR 2"),
                description = NA_character_
            )
        )
    )

    var <- ex$cat_arr3
    var[[list(NULL, 2)]] <- NULL
    expect_equal(
        axes(var),
        list(
            tibble::tibble(
                value = c("x", "y", "z"),
                label = c("ARR X", "ARR Y", "ARR Z"),
                description = NA_character_
            ),
            tibble::tibble(
                value = c("cat1"),
                label = c("ARR 1"),
                description = NA_character_
            )
        )
    )
    var <- ex$cat_arr3
    var[[, list(NULL, 2)]] <- NULL
    expect_equal(
        axes(var),
        list(
            tibble::tibble(
                value = c("x", "y", "z"),
                label = c("ARR X", "ARR Y", "ARR Z"),
                description = NA_character_
            ),
            tibble::tibble(
                value = c("cat1"),
                label = c("ARR 1"),
                description = NA_character_
            )
        )
    )

    # Non-crunch variable (2D) addition in first dimension
    var <- ex$cat_arr3
    var$z2 <- data.frame(cat1 = rep("2", nrow(var)), cat2 = rep(99, nrow(var)))
    expect_equal(var$z2, crunch_categorical_array_variable(
        data.frame(cat1 = rep("2", nrow(var)), cat2 = rep(99, nrow(var))),
        label = "z2",
        values = values(var),
        axes = axes(var[[1]])
    ))

    # New array in outer dimension
    var <- ex$cat_arr3
    var[[list("z2", NULL)]] <- data.frame(cat1 = rep("2", nrow(var)), cat2 = rep(99, nrow(var)))
    expect_equal(var$z2, crunch_categorical_array_variable(
        data.frame(cat1 = rep("2", nrow(var)), cat2 = rep(99, nrow(var))),
        label = "z2",
        values = values(var),
        axes = axes(var[[1]])
    ))

    # Replace array in outer dimension
    var <- ex$cat_arr3
    var[[list("z", NULL)]] <- crunch_categorical_array_variable(
        data.frame(cat1 = rep("2", nrow(var)), cat2 = rep(99, nrow(var))),
        label = "cat3",
        values = values(var),
        axes = axes(var[[1]])
    )
    expect_equal(var$z, crunch_categorical_array_variable(
        data.frame(cat1 = rep("2", nrow(var)), cat2 = rep(99, nrow(var))),
        label = "cat3",
        values = values(var),
        axes = axes(var[[1]])
    ))

    # New array in inner dimension
    var <- ex$cat_arr3
    var[[list(NULL, "cat3")]] <- crunch_categorical_array_variable(
        data.frame(x = rep("2", nrow(var)), y = rep(99, nrow(var)), z = rep(NA, nrow(var))),
        label = "ARR 3",
        values = values(var),
        axes = axes(var[[list(NULL, 1)]])
    )
    expect_equal(var[[list(NULL, "cat3")]], crunch_categorical_array_variable(
        data.frame(x = rep("2", nrow(var)), y = rep(99, nrow(var)), z = rep(NA, nrow(var))),
        label = "ARR 3",
        values = values(ex$cat_arr3),
        axes = axes(ex$cat_arr3[[list(NULL, 1)]])
    ))

    # Existing array in inner dimension
    var <- ex$cat_arr3
    var[[list(NULL, "cat2")]] <- data.frame(x = rep("2", nrow(var)), y = rep(99, nrow(var)), z = rep(NA, nrow(var)))
    expect_equal(var[[list(NULL, "cat2")]], crunch_categorical_array_variable(
        data.frame(x = rep("2", nrow(var)), y = rep(99, nrow(var)), z = rep(NA, nrow(var))),
        label = "ARR 2",
        values = values(ex$cat_arr3),
        axes = axes(ex$cat_arr3[[list(NULL, 1)]])
    ))

    # Existing item in inner dimension
    var <- ex$cat_arr3
    var[[list(NULL, "cat2")]] <- crunch_categorical_array_variable(
        data.frame(x = rep("2", nrow(var)), y = rep(99, nrow(var)), z = rep(NA, nrow(var))),
        label = "ARR TWO",
        values = values(var),
        axes = axes(var[[list(NULL, 1)]])
    )
    expect_equal(var[[list(NULL, "cat2")]], crunch_categorical_array_variable(
        data.frame(x = rep("2", nrow(var)), y = rep(99, nrow(var)), z = rep(NA, nrow(var))),
        label = "ARR TWO",
        values = values(ex$cat_arr3),
        axes = axes(ex$cat_arr3[[list(NULL, 1)]])
    ))

    # Make sure labels get set correctly in list assignment
    var <- ex$cat_arr3
    var[[1, list("x", "cat1")]] <- var$x$cat2[1]
    expect_equal(var[[1, list("x", "cat1")]], crunch_categorical_variable(
        "3",
        label = "ARR 1",
        values = values(var)
    ))

    var <- ex$cat_arr3
    var[[1, list(NULL, "cat1")]] <- var[[1, list(NULL, "cat2")]]
    expect_equal(var[[1, list("x", "cat1")]], crunch_categorical_variable(
        "3",
        label = "ARR 1",
        values = values(var)
    ))
})

test_that("and list subsets with `[[<-` give good errors", {
    var <- ex$cat_arr3
    expect_snapshot_error(var[[list(1)]] <- var[[list(NULL, 1)]])
    expect_snapshot_error(var[[list(NULL, c(1, 2))]] <- var[[list(NULL, 1)]])
    expect_snapshot_error(var[[list(NULL, NULL)]] <- var[[list(NULL, 1)]])
    expect_snapshot_error(var[[list(NULL, 1000)]] <- var[[list(NULL, 1)]])
    expect_snapshot_error(var[[]] <- var[[list(NULL, 1)]])
})

test_that("`dplyr::mutate()` Prevents adding columns with values that don't work and duplicate labels", {
    var <- ex$cat_arr1
    var2d <- ex$cat_arr3
    expect_snapshot_error(dplyr::mutate(var, cat3 = ex$cat_arr2$mr1)) # Defined labels don't match
    expect_snapshot_error(dplyr::mutate(var, cat3 = seq_len(nrow(var)) + 99)) # Extra values
    expect_snapshot_error(dplyr::mutate(var, cat3 = ex$num1[c(1:4, 1)])) # Non-categorical crunch variable
    expect_snapshot_error(dplyr::mutate(var, cat3 = ex$cat_arr1)) # 2D adding into 1D
    expect_snapshot_error(dplyr::mutate(var2d, z2 = ex$cat_arr1[[1]])) # 1D adding into 2D
    expect_snapshot_error(dplyr::mutate(var2d, z2 = ex$cat_arr1[1])) # 2D adding into non-matching 2D
    expect_snapshot_error(dplyr::mutate(var2d, z = dplyr::mutate(z, cat3 = NA))) # Add an item to only 1 of inner arrays 2D
    expect_snapshot_error(dplyr::mutate(var2d, z = dplyr::select(z, "cat1"))) # Remove an item from only 1 of inner arrays 2D
    expect_snapshot_error(dplyr::mutate(var, cat3 = ex$cat_arr1$cat1)) # Duplicate label
})

test_that("but can add and remove columns with `dplyr::mutate()` (1D) ", {
    # Non-crunch variable (1D)
    var <- ex$cat_arr1
    var <- dplyr::mutate(var, cat3 = 1)
    expect_equal(var$cat3, new_crunch_categorical_variable(rep("1", nrow(var)), label = "cat3", values = values(var)))

    # Crunch variable (1D)
    var <- ex$cat_arr1
    var <- dplyr::mutate(var, cat3 = var$cat1 |> set_label("ARR 3"))
    expect_equal(var$cat3, var$cat1 |> set_label("ARR 3"))

    # Removing variable (1D)
    var <- ex$cat_arr1
    var <- dplyr::mutate(var, cat1 = NULL)
    expect_equal(names(var), "cat2")
    var <- ex$cat_arr1
    var <- dplyr::select(var, -cat1)
    expect_equal(names(var), "cat2")
})

test_that("and can add and remove columns with `dplyr::mutate()` (2D) ", {
    # Non-crunch variable (2D) addition in first dimension
    var <- ex$cat_arr3
    var <- dplyr::mutate(var, z2 = data.frame(cat1 = rep("2", nrow(var)), cat2 = rep(99, nrow(var))))
    expect_equal(var$z2, crunch_categorical_array_variable(
        data.frame(cat1 = rep("2", nrow(var)), cat2 = rep(99, nrow(var))),
        label = "z2",
        values = values(var),
        axes = axes(var[[1]])
    ))

    # Crunch variable (2D) addition in first dimension
    var <- ex$cat_arr3
    var <- dplyr::mutate(var, z2 = crunch_categorical_array_variable(
        data.frame(cat1 = rep("2", nrow(var)), cat2 = rep(99, nrow(var))),
        label = "cat3",
        values = values(var),
        axes = axes(var[[1]])
    ))
    expect_equal(var$z2, crunch_categorical_array_variable(
        data.frame(cat1 = rep("2", nrow(var)), cat2 = rep(99, nrow(var))),
        label = "cat3",
        values = values(var),
        axes = axes(var[[1]])
    ))

    # Removing variable (2D) in first dimension
    var <- ex$cat_arr3
    var <- dplyr::mutate(var, z = NULL)
    expect_equal(names(var), c("x", "y"))
    var <- ex$cat_arr3
    var <- dplyr::select(var, -z)
    expect_equal(names(var), c("x", "y"))

    # Crunch variable (2D) addition in second dimension
    var <- ex$cat_arr3
    var <- dplyr::mutate(
        var,
        dplyr::across(
            dplyr::everything(),
            ~dplyr::mutate(., cat3 = crunch_categorical_variable("3", label = "cat3", values = values(var)))
        )
    )
    expect_equal(var$z, crunch_categorical_array_variable(
        data.frame(cat1 = c("1", rep(NA, nrow(var) - 1)), cat2 = c("3", rep(NA, nrow(var) -1)), cat3 = rep(3, nrow(var))),
        label = "ARR Z",
        values = values(var),
        axes = axes(var[[1]])
    ))
})

test_that("`[` works for basic subsetting", {
    actual <- ex$cat_arr1[1:3, ]
    expect_s3_class(actual, "crunch_categorical_array_variable")
    expect_equal(label(actual), label(ex$cat_arr1))
    expect_equal(description(actual), description(ex$cat_arr1))
    expect_equal(notes(actual), notes(ex$cat_arr1))
    expect_equal(path(actual), path(ex$cat_arr1))
    expect_equal(nrow(actual), 3)
    expect_equal(ncol(actual), ncol(ex$cat_arr1))
    expect_equal(actual[[1]], ex$cat_arr1[[1]][1:3])
    expect_equal(actual[[2]], ex$cat_arr1[[2]][1:3])

    actual <- ex$cat_arr1[, 2:1]
    expect_s3_class(actual, "crunch_categorical_array_variable")
    expect_equal(label(actual), label(ex$cat_arr1))
    expect_equal(description(actual), description(ex$cat_arr1))
    expect_equal(notes(actual), notes(ex$cat_arr1))
    expect_equal(path(actual), path(ex$cat_arr1))
    expect_equal(nrow(actual), nrow(ex$cat_arr1))
    expect_equal(ncol(actual), 2)
    expect_equal(actual[[1]], ex$cat_arr1[[2]])
    expect_equal(actual[[2]], ex$cat_arr1[[1]])


    actual <- ex$cat_arr1[]
    expect_s3_class(actual, "crunch_categorical_array_variable")
    expect_equal(actual, ex$cat_arr1)

})

test_that("`[` works for list subsetting", {
    var <- ex$cat_arr3
    var1d <- ex$cat_arr1

    expect_equal(var1d[list(c("cat2", "cat1"))], var1d[c("cat2", "cat1")])
    expect_equal(var[list(c("x", "y"), NULL)], var[c("x", "y")])
    expect_equal(var[list("z", "cat2")], var["z"] |> dplyr::mutate(dplyr::across(dplyr::everything(), ~dplyr::select(., "cat2"))))
    expect_equal(
        var[1:2, list(c("z", "y"), c("cat2", "cat1"))],
        var[c("z", "y")] |> dplyr::mutate(dplyr::across(dplyr::everything(), ~dplyr::select(., c("cat2", "cat1")))) |> dplyr::slice(1:2)
    )
    expect_equal(
        var[, list(NULL, c("cat2", "cat1"))],
        var |> dplyr::mutate(dplyr::across(dplyr::everything(), ~dplyr::select(., c("cat2", "cat1"))))
    )
    expect_equal(var[list(NULL, NULL)], var)
    expect_equal(var[1:3, list(NULL, NULL)], var[1:3, ])

    expect_snapshot_error(var1d[list("cat1", NULL)])
    expect_snapshot_error(var1d[list(~1)])
    expect_snapshot_error(var[list("x")])
})

# TODO: `[<-` tests

test_that("can set values of categorical array", {
    var <- ex$cat_arr1
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

    var <- ex$cat_arr1
    expect_snapshot_error(set_values(var, values(var) |> dplyr::slice(1)))
})

test_that("can get mean of categorical array", {
    expect_equal(mean(ex$cat_arr1, from = "scale", na.rm = TRUE), tibble::tibble(cat1 = 8/3, cat2 = 1.6))
    expect_equal(mean(ex$cat_arr1, from = "scale"), tibble::tibble(cat1 = NA_real_, cat2 = 1.6))
    expect_equal(mean(ex$cat_arr2, from = "selected", na.rm = TRUE), tibble::tibble(mr1 = 0.5, mr2 = 1/3, mr3 = NaN))
    expect_equal(
        mean(ex$cat_arr3, na.rm = TRUE),
        tibble::tibble(
            x = tibble::tibble(cat1 = 8/3, cat2 = 5),
            y = tibble::tibble(cat1 = 8/3, cat2 = 1.6),
            z = tibble::tibble(cat1 = 1, cat2 = 5)
        )
    )
})

test_that("rbind and bind_rows work as expected", {
    # works on compatible arrays
    actual <- rbind(ex$cat_arr1, ex$cat_arr1 |> set_label("New!"))
    expect_s3_class(actual, "crunch_categorical_array_variable")
    expect_equal(label(actual), label(ex$cat_arr1))
    expect_equal(actual[seq_len(nrow(ex$cat_arr1)), ], ex$cat_arr1)
    expect_equal(actual[nrow(ex$cat_arr1) + seq_len(nrow(ex$cat_arr1)), ], ex$cat_arr1)
    actual <- dplyr::bind_rows(ex$cat_arr1, ex$cat_arr1 |> set_label("New!"))
    expect_s3_class(actual, "crunch_categorical_array_variable")
    expect_equal(label(actual), label(ex$cat_arr1))
    expect_equal(actual[seq_len(nrow(ex$cat_arr1)), ], ex$cat_arr1)
    expect_equal(actual[nrow(ex$cat_arr1) + seq_len(nrow(ex$cat_arr1)), ], ex$cat_arr1)

    # But can't bind incompatible axes
    rbind(ex$cat_arr1, ex$cat_arr2) |> expect_snapshot_error()
    dplyr::bind_rows(ex$cat_arr1, ex$cat_arr2) |> expect_snapshot_error()

    # Nor incompatible values
    rbind(ex$cat_arr1, ex$cat_arr2 |> dplyr::select(cat1 = 1, cat2 = 2)) |> expect_snapshot_error()
    dplyr::bind_rows(ex$cat_arr1, ex$cat_arr2 |> dplyr::select(cat1 = 1, cat2 = 2)) |> expect_snapshot_error()
})

test_that("bind_cols works as expected", {
    # Follow tibble's lead and don't support base::cbind

    # works on compatible arrays (1D)
    compat_cbind <- ex$cat_arr1 |>
        dplyr::rename_with(~paste0(., "x")) |>
        dplyr::mutate(dplyr::across(dplyr::everything(), ~set_label(., paste0(label(.), "X"))))

    actual <- dplyr::bind_cols(ex$cat_arr1, compat_cbind)
    expect_s3_class(actual, "crunch_categorical_array_variable")
    expect_equal(label(actual), label(ex$cat_arr1))
    expect_equal(actual[1:2], ex$cat_arr1)
    expect_equal(actual[3:4], compat_cbind)


    # works on compatible arrays (2D)
    compat_cbind <- ex$cat_arr3 |>
        dplyr::select(1) |>
        dplyr::rename_with(~paste0(., "x")) |>
        dplyr::mutate(dplyr::across(dplyr::everything(), ~set_label(., paste0(label(.), "X"))))

    actual <- dplyr::bind_cols(ex$cat_arr3, compat_cbind)
    expect_s3_class(actual, "crunch_categorical_array_variable")
    expect_equal(label(actual), label(ex$cat_arr3))
    expect_equal(actual[1:3], ex$cat_arr3)
    expect_equal(actual[4], compat_cbind)

    # But can't bind incompatible axes
    dplyr::bind_cols(ex$cat_arr1, ex$cat_arr3) |> expect_snapshot_error()

    # Nor incompatible values
    dplyr::bind_cols(ex$cat_arr1, ex$cat_arr2) |> expect_snapshot_error()
})
