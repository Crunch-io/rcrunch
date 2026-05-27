.cat_arr1_values <- tibble::tibble(
    value = c(1, 2, 3, 99),
    label = c("Once", "Twice", "More than twice", "Don't Know"),
    missing = c(FALSE, FALSE, FALSE, TRUE),
    scale = c(1, 2, 5, NA),
    selected = NA,
    date = NA
)

ex <- list(
    # Cat with scale
    cat1 = crunch_categorical_variable(
        c(1, 2, 3, 99, NA),
        label = "Cat 1",
        values = .cat_arr1_values
    ),

    # Cat with dates
    cat2 = crunch_categorical_variable(
        c(1, 2, 3, 99, NA),
        label = "Cat 2",
        values = tibble::tibble(
            value = c(1, 2, 3, 99),
            label = c("Wave 1", "Wave 2", "Wave 3", "Unknown"),
            missing = c(FALSE, FALSE, FALSE, TRUE),
            scale = NA,
            selected = NA,
            date = c("2025", "2026", "2027", NA)
        )
    ),

    # 1D categorical array with scale
    cat_arr1 = crunch_categorical_array_variable(
        tibble::tibble(
            cat1 = c(1, 2, 3, 99, NA),
            cat2 = c(2, 2, 2, 1, 1)
        ),
        label = "Cat Array 1",
        axes = list(tibble::tibble(
            value = c("cat1", "cat2"),
            label = c("ARR 1", "ARR 2"),
            description = NA
        )),
        values = .cat_arr1_values
    ),

    # 1D categorical array with selections
    cat_arr2 = crunch_categorical_array_variable(
        tibble::tibble(
            mr1 = c(1, 1, 2, 2, NA),
            mr2 = c(2, NA, 2, 99, 1),
            mr3 = NA,
        ),
        label = "Cat Array 2",
        axes = list(tibble::tibble(
            value = c("mr1", "mr2", "mr3"),
            label = c("RESP 1", "RESP 2", "RESP 3"),
            description = NA
        )),
        values = tibble::tibble(
            value = c(1, 2, 99),
            label = c("Yes", "No", "Refused"),
            missing = c(FALSE, FALSE, TRUE),
            scale = NA,
            selected = c(TRUE, NA, NA),
            date = NA
        )
    ),

    cat_arr3 = crunch_categorical_array_variable(
        tibble::tibble(
            x = crunch_categorical_array_variable(
                tibble::tibble(
                    cat1 = crunch_categorical_variable(c(2, 3, 1, 99, NA), label = "ARR 1", value = .cat_arr1_values),
                    cat2 = crunch_categorical_variable(c(3, 3, 3, 3, 3), label = "ARR 2", value = .cat_arr1_values)
                ),
                label = "ARR X"
            ),
            y = crunch_categorical_array_variable(
                tibble::tibble(
                    cat1 = crunch_categorical_variable(c(3, 2, 1, 99, NA), label = "ARR 1", value = .cat_arr1_values),
                    cat2 = crunch_categorical_variable(c(2, 2, 2, 1, 1), label = "ARR 2", value = .cat_arr1_values)
                ),
                label = "ARR Y"
            ),
            z = crunch_categorical_array_variable(
                tibble::tibble(
                    cat1 = crunch_categorical_variable(c(1, NA, NA, NA, NA), label = "ARR 1", value = .cat_arr1_values),
                    cat2 = crunch_categorical_variable(c(3, NA, NA, NA, NA), label = "ARR 2", value = .cat_arr1_values)
                ),
                label = "ARR Z"
            )
        ),
        label = "Cat Array 3"
    ),

    # Numeric variable
    num1 = crunch_numeric_variable(
        c(1, 2, 3, 4),
        label = "Numeric 1"
    ),

    # 1D numeric array
    num_arr1 = crunch_numeric_array_variable(
        tibble::tibble(
            num1 = c(1.25, 35, 1.4),
            num2 = c(NA, 100, 9)
        ),
        label = "Numeric Array 1",
        axes = list(tibble::tibble(
            value = c("num1", "num2"),
            label = c("X 1", "X 2"),
            description = NA
        ))
    ),

    # 2D numeric array
    num_arr2 = crunch_numeric_array_variable(
        tibble::tibble(
            a = crunch_numeric_array_variable(
                tibble::tibble(
                    x = crunch_numeric_variable(c(22, 42, 43), label = "AX X"),
                    y = crunch_numeric_variable(c(-1, -2, -3), label = "AX Y")
                ),
                label = "AX A"
            ),
            b = crunch_numeric_array_variable(
                tibble::tibble(
                    x = crunch_numeric_variable(c(99, 98, 97), label = "AX X"),
                    y = crunch_numeric_variable(c(-2, -2, -2), label = "AX Y")
                ),
                label = "AX B"
            ),
            c = crunch_numeric_array_variable(
                tibble::tibble(
                    x = crunch_numeric_variable( c(50, 51, 50), label = "AX X"),
                    y = crunch_numeric_variable(c(-5, -19, -20), label = "AX Y")
                ),
                label = "AX C"
            )
        ),
        label = "Numeric Array 2"
    )
)
