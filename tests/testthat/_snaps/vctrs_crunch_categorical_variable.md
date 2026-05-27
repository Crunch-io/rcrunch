# validation runs on creation

    Invalid crunch_categorical_variable
    x `label` must be <character> (got a number)
    x `description` must be <character> (got a <formula> object)
    x `path` must be <character> (got a function)
    x category values must be <character> or base <numeric> types (got a <Date> object)
    x Invalid values
      x `values` must be <data.frame> (got a number)

# validate_category_value_data coerces to character

    Can't convert from `category code` <double> to <integer> due to loss of precision.
    * Locations: 1

---

    category values must be <character> or base <numeric> types (got a list)

# validate_category_values validates and converts

    Invalid values
    x `values` must be <data.frame> (got a number)

---

    Invalid values
    x `values` does not have column named value
    x `values` does not have column named label
    x `values` does not have column named missing
    x `values` does not have column named scale
    x `values` does not have column named date

---

    Invalid values
    x Can't convert from `category code` <double> to <integer> due to loss of precision.
    * Locations: 1
    x Can't convert `values$label` <double> to <character>.
    x Can't convert `values$missing` <character> to <logical>.
    x `values` does not have column named scale
    x Can't convert `values$date` <double> to <character>.

---

    Invalid values
    x `values$value` has duplicated items (1)
    x data not entirely in `values$value` (extra items: 2, 3)
    x `values$label` has duplicated items (a)
    x `values$missing` not allowed to have missing values
    x `values$date` has duplicated items (ZZ)
    x `values$date` contains incorrectly formatted date strings (ZZ)

# can set values of categorical variable

    Invalid values
    x data not entirely in `values$value` (extra items: 2, 3, 99)

# vec_ptype successes and validation

    Can't combine `ex$cat1` <crunch_categorical_variable> and `ex$cat2` <crunch_categorical_variable>.
    Category values do not match
    1 match on everything but label (99: label= Don't Know vs Unknown)
    3 in `ex$cat1` have no match (1 - Once, 2 - Twice, 3 - More than twice)
    3 in `ex$cat2` have no match (1 - Wave 1, 2 - Wave 2, 3 - Wave 3)

---

    Can't combine `ex$cat1` <crunch_categorical_variable> and `ex$num1` <crunch_numeric_variable>.

# vec_cast successes and validation

    Can't convert `"1000"` <character> to <crunch_categorical_variable>.
    Values not found in categories: 1000

---

    Can't convert `ex$cat2` <crunch_categorical_variable> to <crunch_categorical_variable>.
    Category values do not match
    1 match on everything but label (99: label= Unknown vs Don't Know)
    3 in `ex$cat2` have no match (1 - Wave 1, 2 - Wave 2, 3 - Wave 3)
    3 in `` have no match (1 - Once, 2 - Twice, 3 - More than twice)

---

    Can't convert `ex$cat1` <crunch_categorical_variable> to <crunch_numeric_variable>.

# Cannot compare categoricals

    <crunch_categorical_variable> compare <crunch_categorical_variable> is not permitted
    Cannot compare categorical values

---

    <crunch_categorical_variable> compare <crunch_categorical_variable> is not permitted
    Cannot compare categorical values

---

    <crunch_categorical_variable> compare <crunch_categorical_variable> is not permitted
    Cannot compare categorical values

# equality and inequality only work when values match

    Can't combine `ex$cat1[1]` <crunch_categorical_variable> and `ex$cat2[1]` <crunch_categorical_variable>.
    Category values do not match
    1 match on everything but label (99: label= Don't Know vs Unknown)
    3 in `ex$cat1[1]` have no match (1 - Once, 2 - Twice, 3 - More than twice)
    3 in `ex$cat2[1]` have no match (1 - Wave 1, 2 - Wave 2, 3 - Wave 3)

---

    Can't combine `ex$cat1[1]` <crunch_categorical_variable> and `ex$cat2[1]` <crunch_categorical_variable>.
    Category values do not match
    1 match on everything but label (99: label= Don't Know vs Unknown)
    3 in `ex$cat1[1]` have no match (1 - Once, 2 - Twice, 3 - More than twice)
    3 in `ex$cat2[1]` have no match (1 - Wave 1, 2 - Wave 2, 3 - Wave 3)

# regular & pillar printing

    # A tibble: 5 x 2
      cat1                   cat2               
      <cr_cat>               <cr_cat>           
    1    1 [Once]               1 [Wave 1]      
    2    2 [Twice]              2 [Wave 2]      
    3    3 [More than twice]    3 [Wave 3]      
    4   99 (NA) [Don't Know]   99 (NA) [Unknown]
    5 <NA>                   <NA>               

---

    <crunch_categorical_variable[5]>: Cat 1
    [1] 1  2  3  99 NA
    
    Categories:
     value           label missing scale selected date
         1            Once   FALSE     1       NA <NA>
         2           Twice   FALSE     2       NA <NA>
         3 More than twice   FALSE     5       NA <NA>
        99      Don't Know    TRUE    NA       NA <NA>

---

    <crunch_categorical_variable[5]>: Cat 2
    [1] 1  2  3  99 NA
    
    Categories:
     value   label missing scale selected date
         1  Wave 1   FALSE    NA       NA 2025
         2  Wave 2   FALSE    NA       NA 2026
         3  Wave 3   FALSE    NA       NA 2027
        99 Unknown    TRUE    NA       NA <NA>

# coalesce_categorical_values error messages

    Can't combine `x arg` <crunch_categorical_variable> and `y arg` <crunch_categorical_variable>.
    Category values do not match
    1 match on everything but label (99: label= Don't Know vs Unknown)
    3 in `x arg` have no match (1 - Once, 2 - Twice, 3 - More than twice)
    3 in `y arg` have no match (1 - Wave 1, 2 - Wave 2, 3 - Wave 3)

---

    Can't combine `c1` <crunch_categorical_variable> and `c2` <crunch_categorical_variable>.
    Category values do not match
    2 match exactly
    4 categeories are only in `c2` (3 - c, 4 - d, 5 - e, ...)

---

    Can't combine `c2` <crunch_categorical_variable> and `c1` <crunch_categorical_variable>.
    Category values do not match
    2 match exactly
    4 categeories are only in `c2` (3 - c, 4 - d, 5 - e, ...)

---

    Can't combine `c1` <crunch_categorical_variable> and `c2` <crunch_categorical_variable>.
    Category values do not match
    2 match exactly
    Category ordering is different

---

    Can't combine `c1` <crunch_categorical_variable> and `c2` <crunch_categorical_variable>.
    Category values do not match
    2 match on value and label, but not on missing or scale (1 - a: missing= FALSE vs TRUE scale= 1 vs NA, 2 - b: missing= FALSE vs TRUE scale= 2 vs NA)

---

    Can't combine `c1` <crunch_categorical_variable> and `c2` <crunch_categorical_variable>.
    Category values do not match
    2 match on value and label, but not on selected or date (1 - a: selected= TRUE vs FALSE date= 2026 vs 2027, 2 - b: selected= FALSE vs TRUE)

---

    Can't combine `c1` <crunch_categorical_variable> and `c2` <crunch_categorical_variable>.
    Category values do not match
    2 match on everything but value (a: value= 1 vs 1X, b: value= 2 vs 2X)

---

    Can't combine `c1` <crunch_categorical_variable> and `c2` <crunch_categorical_variable>.
    Category values do not match
    2 match on everything but label (1: label= a vs aX, 2: label= b vs bX)

# code_to_str works as expected

    Can't convert from `category code` <double> to <integer> due to loss of precision.
    * Locations: 1

---

    Can't convert `category code` <list> to <integer>.

# catdate_str_to_date works as expected

    Unexpected categorical date formats found: XYZ, AB

# check_catdate_string works

    `c("l;kajg", "25-01")` contains incorrectly formatted date strings (l;kajg, 25-01)

