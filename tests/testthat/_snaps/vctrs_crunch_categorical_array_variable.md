# Setting values runs validation

    Invalid values
    x data not entirely in `values$value` (extra items: 2, 3, 99)

---

    Invalid values
    x `values$label` has duplicated items (a)

# Validation messages in crunch_categorical_array_variable make sense 

    Invalid crunch_categorical_array_variable
    x `label` must be <character> (got NULL)
    x Invalid values
      x `values` must be <data.frame> (got a number)
    x Invalid axes
      x `axes` must be <list> (got a number)

# validate_array_category_data validation messages: NULL values

    Invalid data for creating a categorical array variable without specifying values
    x All columns must be crunch variables of type `crunch_categorical_variable` or `crunch_categorical_array_variable`

---

    Invalid data for creating a categorical array variable without specifying values
    x All columns must have identical category values

# validate_array_category_data validation messages: values with crunch variables

    Invalid data for creating a categorical array from existing crunch variables
    x All columns must be crunch variables of type crunch_categorical_variable or crunch_categorical_array_variable
    x i In index: 1.
    i With name: x.
    Caused by error in `UseMethod()`:
    ! no applicable method for 'values' applied to an object of class "c('integer', 'numeric')"

---

    Invalid data for creating a categorical array from existing crunch variables
    x All columns must have category values that match the values argument

# validate_array_category_data validation messages: values with base types

    Invalid values
    x data not entirely in `values$value` (extra items: 4, 5, 6, ...)

# list dimensional subsets work in `[[`

    array list subscript must have the same length as number of axes (1, but got 2)

---

    array subscript list can't be NULL for all dimensions in `[[`

---

    array list subscript items can only be length 1 or 0 for each dimension in `[[`

---

    array list subscript items can only be logical, numeric, character or NULL in `[[`

---

    array list subscript must have the same length as number of axes (2, but got 1)

---

    array subscript list can't be NULL for all dimensions in `[[`

---

    array list subscript items can only be length 1 or 0 for each dimension in `[[`

---

    array list subscript items can only be logical, numeric, character or NULL in `[[`

# `[[<-` & `$<-` prevent adding columns with values or axes that don't work or duplicate labels

    Can't convert `ex$cat_arr2$mr1` <crunch_categorical_variable> to <crunch_categorical_variable>.
    Category values do not match
    1 match on everything but label (99: label= Refused vs Don't Know)
    2 in `ex$cat_arr2$mr1` have no match (1 - Yes, 2 - No)
    3 in `` have no match (1 - Once, 2 - Twice, 3 - More than twice)

---

    Can't convert `value` <character> to <crunch_categorical_variable>.
    Values not found in categories: 100, 101, 102, ...

---

    Can't convert `value` <crunch_numeric_variable> to <crunch_categorical_variable>.

---

    Can't convert `value` <crunch_categorical_array_variable> to <crunch_categorical_variable>.

---

    Can't convert `value` <crunch_categorical_variable> to <crunch_categorical_array_variable>.

---

    Cannot convert to comparable crunch_categorical_array_variable
    x Invalid axes
      x axes[[1]]
        x Axis values must match column names

---

    Cannot convert to comparable crunch_categorical_array_variable
    x Invalid axes
      x axes[[1]]
        x Axis values must match column names

---

    Cannot convert to comparable crunch_categorical_array_variable
    x Invalid axes
      x axes[[1]]
        x Axis values must match column names

---

    Invalid inferred axis structure
    x `axis item labels` has duplicated items (ARR 1)

# and list subsets with `[[<-` give good errors

    array list subscript must have the same length as number of axes (2, but got 1)

---

    array list subscript items can only be length 1 or 0 for each dimension in `[[`

---

    array subscript list can't be NULL for all dimensions in `[[`

---

    Can't assign to columns beyond the end with non-consecutive locations.
    i Input has size 2.
    x Subscript `i` contains non-consecutive location 1000.

---

    Subscript can't be missing for tibbles in `[[<-`.

# `dplyr::mutate()` Prevents adding columns with values that don't work and duplicate labels

    Invalid array
    x Invalid data for creating a categorical array variable without specifying values
      x All columns must have identical category values

---

    Can't convert `cat3` <character> to <crunch_categorical_variable>.
    Values not found in categories: 100, 101, 102, ...

---

    Invalid array
    x Invalid data for creating a categorical array variable without specifying values
      x All columns must be crunch variables of type `crunch_categorical_variable` or `crunch_categorical_array_variable`
    x Invalid inferred axis structure
      x All columns must be crunch variables of type crunch_categorical_variable or crunch_categorical_array_variable

---

    Invalid array
    x Invalid inferred axis structure
      x All columns must be of the same type

---

    Invalid array
    x Invalid inferred axis structure
      x All columns must be of the same type

---

    Invalid array
    x Invalid inferred axis structure
      x Second dimension of component arrays must exactly match

---

    Invalid array
    x Invalid inferred axis structure
      x Second dimension of component arrays must exactly match

---

    Invalid array
    x Invalid inferred axis structure
      x Second dimension of component arrays must exactly match

---

    Invalid array
    x Invalid inferred axis structure
      x `axis item labels` has duplicated items (ARR 1)

# `[` works for list subsetting

    array list subscript must have the same length as number of axes (1, but got 2)

---

    array list subscript items can only be logical, numeric, character or NULL in `[`

---

    array list subscript must have the same length as number of axes (2, but got 1)

# can set values of categorical array

    Invalid values
    x data not entirely in `values$value` (extra items: 2, 3, 99)

# rbind and bind_rows work as expected

    numbers of columns of arguments do not match

---

    Invalid array
    x Invalid data for creating a categorical array variable without specifying values
      x All columns must have identical category values

---

    Can't convert `value` <crunch_categorical_variable> to <crunch_categorical_variable>.
    Category values do not match
    1 match on everything but label (99: label= Refused vs Don't Know)
    2 in `value` have no match (1 - Yes, 2 - No)
    3 in `` have no match (1 - Once, 2 - Twice, 3 - More than twice)

---

    Can't combine `..1$cat1` <crunch_categorical_variable> and `..2$cat1` <crunch_categorical_variable>.
    Category values do not match
    1 match on everything but label (99: label= Don't Know vs Refused)
    3 in `..1$cat1` have no match (1 - Once, 2 - Twice, 3 - More than twice)
    2 in `..2$cat1` have no match (1 - Yes, 2 - No)

# bind_cols works as expected

    Invalid array
    x Invalid inferred axis structure
      x All columns must be of the same type

---

    Invalid array
    x Invalid data for creating a categorical array variable without specifying values
      x All columns must have identical category values

