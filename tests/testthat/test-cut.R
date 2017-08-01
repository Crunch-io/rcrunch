context("cut function")

with_mock_crunch({
  ds <- loadDataset("test ds")
  gen <- ds$gender
  test_that("cut returns expected output", {
    
    ##################### EXPECTED OUTPUT ######################################
    basic_output <- structure(list(name = "new_var", derivation = structure(list(
    `function` = "case", args = list(structure(list(column = structure(1:3, class = "AsIs"), 
        type = structure(list(value = structure(list(class = "categorical", 
            categories = list(structure(list(id = 1L, name = "one", 
                numeric_value = NULL, missing = FALSE), .Names = c("id", 
            "name", "numeric_value", "missing")), structure(list(
                id = 2L, name = "two", numeric_value = NULL, 
                missing = FALSE), .Names = c("id", "name", "numeric_value", 
            "missing")), structure(list(id = 3L, name = "three", 
                numeric_value = NULL, missing = FALSE), .Names = c("id", 
            "name", "numeric_value", "missing")))), .Names = c("class", 
        "categories"))), .Names = "value")), .Names = c("column", 
    "type")), structure(list(`function` = "and", args = list(
        structure(list(`function` = "<=", args = list(structure(list(
            value = -1.4998629), .Names = "value"), structure(list(
            variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"), .Names = "variable"))), .Names = c("function", 
        "args")), structure(list(`function` = "<", args = list(
            structure(list(variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"), .Names = "variable"), 
            structure(list(value = -0.4424), .Names = "value"))), .Names = c("function", 
        "args")))), .Names = c("function", "args")), structure(list(
        `function` = "and", args = list(structure(list(`function` = "<=", 
            args = list(structure(list(value = -0.4424), .Names = "value"), 
                structure(list(variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"), .Names = "variable"))), .Names = c("function", 
        "args")), structure(list(`function` = "<", args = list(
            structure(list(variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"), .Names = "variable"), 
            structure(list(value = 0.6119), .Names = "value"))), .Names = c("function", 
        "args")))), .Names = c("function", "args")), structure(list(
        `function` = "and", args = list(structure(list(`function` = "<=", 
            args = list(structure(list(value = 0.6119), .Names = "value"), 
                structure(list(variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"), .Names = "variable"))), .Names = c("function", 
        "args")), structure(list(`function` = "<", args = list(
            structure(list(variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"), .Names = "variable"), 
            structure(list(value = 1.6693629), .Names = "value"))), .Names = c("function", 
        "args")))), .Names = c("function", "args")))), .Names = c("function", 
"args"))), .Names = c("name", "derivation"), class = "VariableDefinition")
    
     right_false <- structure(list(name = "new_var", derivation = structure(list(
    `function` = "case", args = list(structure(list(column = structure(1:3, class = "AsIs"), 
        type = structure(list(value = structure(list(class = "categorical", 
            categories = list(structure(list(id = 1L, name = "one", 
                numeric_value = NULL, missing = FALSE), .Names = c("id", 
            "name", "numeric_value", "missing")), structure(list(
                id = 2L, name = "two", numeric_value = NULL, 
                missing = FALSE), .Names = c("id", "name", "numeric_value", 
            "missing")), structure(list(id = 3L, name = "three", 
                numeric_value = NULL, missing = FALSE), .Names = c("id", 
            "name", "numeric_value", "missing")))), .Names = c("class", 
        "categories"))), .Names = "value")), .Names = c("column", 
    "type")), structure(list(`function` = "and", args = list(
        structure(list(`function` = "<", args = list(structure(list(
            value = -1.4998629), .Names = "value"), structure(list(
            variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"), .Names = "variable"))), .Names = c("function", 
        "args")), structure(list(`function` = "<=", args = list(
            structure(list(variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"), .Names = "variable"), 
            structure(list(value = -0.4424), .Names = "value"))), .Names = c("function", 
        "args")))), .Names = c("function", "args")), structure(list(
        `function` = "and", args = list(structure(list(`function` = "<", 
            args = list(structure(list(value = -0.4424), .Names = "value"), 
                structure(list(variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"), .Names = "variable"))), .Names = c("function", 
        "args")), structure(list(`function` = "<=", args = list(
            structure(list(variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"), .Names = "variable"), 
            structure(list(value = 0.6119), .Names = "value"))), .Names = c("function", 
        "args")))), .Names = c("function", "args")), structure(list(
        `function` = "and", args = list(structure(list(`function` = "<", 
            args = list(structure(list(value = 0.6119), .Names = "value"), 
                structure(list(variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"), .Names = "variable"))), .Names = c("function", 
        "args")), structure(list(`function` = "<=", args = list(
            structure(list(variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"), .Names = "variable"), 
            structure(list(value = 1.6693629), .Names = "value"))), .Names = c("function", 
        "args")))), .Names = c("function", "args")))), .Names = c("function", 
"args"))), .Names = c("name", "derivation"), class = "VariableDefinition")
     
      set_breaks <- structure(list(name = "new_var", derivation = structure(list(
    `function` = "case", args = list(structure(list(column = structure(1:2, class = "AsIs"), 
        type = structure(list(value = structure(list(class = "categorical", 
            categories = list(structure(list(id = 1L, name = "one", 
                numeric_value = NULL, missing = FALSE), .Names = c("id", 
            "name", "numeric_value", "missing")), structure(list(
                id = 2L, name = "two", numeric_value = NULL, 
                missing = FALSE), .Names = c("id", "name", "numeric_value", 
            "missing")))), .Names = c("class", "categories"))), .Names = "value")), .Names = c("column", 
    "type")), structure(list(`function` = "and", args = list(
        structure(list(`function` = "<", args = list(structure(list(
            value = -1.4967), .Names = "value"), structure(list(
            variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"), .Names = "variable"))), .Names = c("function", 
        "args")), structure(list(`function` = "<=", args = list(
            structure(list(variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"), .Names = "variable"), 
            structure(list(value = 0), .Names = "value"))), .Names = c("function", 
        "args")))), .Names = c("function", "args")), structure(list(
        `function` = "and", args = list(structure(list(`function` = "<", 
            args = list(structure(list(value = 0), .Names = "value"), 
                structure(list(variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"), .Names = "variable"))), .Names = c("function", 
        "args")), structure(list(`function` = "<=", args = list(
            structure(list(variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"), .Names = "variable"), 
            structure(list(value = 1.6662), .Names = "value"))), .Names = c("function", 
        "args")))), .Names = c("function", "args")))), .Names = c("function", 
"args"))), .Names = c("name", "derivation"), class = "VariableDefinition")
    
   dig_lab <- structure(list(name = "new_var", derivation = structure(list(
    `function` = "case", args = list(structure(list(column = structure(1:3, class = "AsIs"), 
        type = structure(list(value = structure(list(class = "categorical", 
            categories = list(structure(list(id = 1L, name = "(-1.5,-0.44]", 
                numeric_value = NULL, missing = FALSE), .Names = c("id", 
            "name", "numeric_value", "missing")), structure(list(
                id = 2L, name = "(-0.44,0.61]", numeric_value = NULL, 
                missing = FALSE), .Names = c("id", "name", "numeric_value", 
            "missing")), structure(list(id = 3L, name = "(0.61,1.7]", 
                numeric_value = NULL, missing = FALSE), .Names = c("id", 
            "name", "numeric_value", "missing")))), .Names = c("class", 
        "categories"))), .Names = "value")), .Names = c("column", 
    "type")), structure(list(`function` = "and", args = list(
        structure(list(`function` = "<=", args = list(structure(list(
            value = -1.4998629), .Names = "value"), structure(list(
            variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"), .Names = "variable"))), .Names = c("function", 
        "args")), structure(list(`function` = "<", args = list(
            structure(list(variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"), .Names = "variable"), 
            structure(list(value = -0.4424), .Names = "value"))), .Names = c("function", 
        "args")))), .Names = c("function", "args")), structure(list(
        `function` = "and", args = list(structure(list(`function` = "<=", 
            args = list(structure(list(value = -0.4424), .Names = "value"), 
                structure(list(variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"), .Names = "variable"))), .Names = c("function", 
        "args")), structure(list(`function` = "<", args = list(
            structure(list(variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"), .Names = "variable"), 
            structure(list(value = 0.6119), .Names = "value"))), .Names = c("function", 
        "args")))), .Names = c("function", "args")), structure(list(
        `function` = "and", args = list(structure(list(`function` = "<=", 
            args = list(structure(list(value = 0.6119), .Names = "value"), 
                structure(list(variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"), .Names = "variable"))), .Names = c("function", 
        "args")), structure(list(`function` = "<", args = list(
            structure(list(variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"), .Names = "variable"), 
            structure(list(value = 1.6693629), .Names = "value"))), .Names = c("function", 
        "args")))), .Names = c("function", "args")))), .Names = c("function", 
"args"))), .Names = c("name", "derivation"), class = "VariableDefinition")
     

    ##############################################################################
    expect_identical(
      basic_output,
      cut(ds$birthyr, 3, variableName = "new_var", label = c("one", "two", "three"))
    )
    
    expect_identical(
      right_false,
      cut(ds$birthyr,
                     3,
                     variableName = "new_var",
                     label = c("one", "two", "three"),
                     right = FALSE)
    )
   
    expect_identical(
      set_breaks,
      cut(ds$birthyr,
                     c(-1.4967, 0, 1.6662),
                     variableName = "new_var",
                     label = c("one", "two"),
                     right = FALSE)
    )
    expect_identical(
      dig_lab,
      cut(ds$birthyr,
                     3,
                     dig.lab = 2,
                     variableName = "new_var")
    )
  })
})

with_test_authentication({
  ds <- newDataset(df)
  #as.character(cut(...)) is used because as.vector(CrunchVariable) returns
  # a character, while cut returns a factor by default. 
  test_that("cut returns the same thing for crunch variables and identical vectors", {
    expect_identical(
      as.character(cut(df$v1, 3, label = c("one", "two", "three"))),
      as.vector(cut(df$v1, 3, variableName = "new_var", label = c("one", "two", "three")))
    )
    
    expect_identical(
      as.character(cut(df$v1, 3, label = c("one", "two", "three"), right = FALSE)),
      as.vector( cut(df$v1, 
          3, 
          variableName = "new_var", 
          label = c("one", "two", "three"),
          right = FALSE))
    )
   
    expect_identical(
      as.character(cut(df$v1, c(-1.4967, 0, 1.6662), label = c("one", "two"), right = FALSE)),
      as.vector(cut(df$v1,
                     c(-1.4967, 0, 1.6662),
                     variableName = "new_var",
                     label = c("one", "two"),
                     right = FALSE))
    )
    expect_identical(
      as.character(cut(df$v1, 3, dig.lab = 2)),
      as.vector(cut(df$v1,
                     3,
                     dig.lab = 2,
                     variableName = "new_var"))
    )

  })
  
})
  