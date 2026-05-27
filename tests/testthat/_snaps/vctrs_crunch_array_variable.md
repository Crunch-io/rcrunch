# validate_axes failure messages when axes is NULL

    Invalid inferred axis structure
    x All columns must be crunch variables of type crunch_numeric_variable or crunch_numeric_array_variable

---

    Invalid inferred axis structure
    x All columns must be of the same type

---

    Invalid inferred axis structure
    x `axis item labels` has duplicated items (Numeric Array 1)
    x Second dimension of component arrays must exactly match

---

    Invalid inferred axis structure
    x Arrays cannot have 3rd dimension

# validate_axes failure messages when axes is provided

    Invalid axes
    x `axes` must be <list> (got a number)

---

    Invalid axes
    x axes can only be 1 or 2 dimensional

---

    Invalid axes
    x axes[[1]]
      x Can't convert `axis$value` <double> to <character>.
      x Can't convert `axis$label` <list> to <character>.
      x Can't convert `axis$description` <list> to <character>.
    x axes[[2]]
      x `axis` does not have column named value
      x `axis` does not have column named label
      x `axis` does not have column named description

---

    Invalid axes
    x axes[[1]]
      x Axis values must match column names

---

    Invalid axes
    x axes[[1]]
      x Axis values must match column names
    x axes[[2]]
      x Axis values in 2nd dimension must match inner column names

---

    Invalid axes
    x axes[[1]]
      x `axis$value` has duplicated items (a)
      x `axis$label` has duplicated items (b)

# names validates & can set names on array

    `crunch_array_variable`s must have names

---

    `value` has duplicated items (a)

---

    `crunch_array_variable`s must have names for each element (got names of length 1 for 2 values)

# pillar print methods snapshot - cat_arr1

    # A tibble: 5 x 2
      cat1                   cat2     
    * <cr_cat>               <cr_cat> 
    1    1 [Once]            2 [Twice]
    2    2 [Twice]           2 [Twice]
    3    3 [More than twice] 2 [Twice]
    4   99 (NA) [Don't Know] 1 [Once] 
    5 <NA>                   1 [Once] 

# pillar print methods snapshot - cat_arr2

    # A tibble: 5 x 3
      mr1        mr2                 mr3     
    * <cr_cat>   <cr_cat>            <cr_cat>
    1  v 1 [Yes]  x 2 [No]           <NA>    
    2  v 1 [Yes] <NA>                <NA>    
    3  x 2 [No]   x 2 [No]           <NA>    
    4  x 2 [No]    99 (NA) [Refused] <NA>    
    5 <NA>        v 1 [Yes]          <NA>    

# pillar print methods snapshot - cat_arr3

    # A tibble: 5 x 3
      x$cat1                 $cat2               y$cat1               z$cat1     
    * <cr_cat>               <cr_cat>            <cr_cat>             <cr_cat>   
    1    2 [Twice]           3 [More than twice]    3 [More than twi~    1 [Once]
    2    3 [More than twice] 3 [More than twice]    2 [Twice]         <NA>       
    3    1 [Once]            3 [More than twice]    1 [Once]          <NA>       
    4   99 (NA) [Don't Know] 3 [More than twice]   99 (NA) [Don't Kn~ <NA>       
    5 <NA>                   3 [More than twice] <NA>                 <NA>       
    # i 2 more variables: y$cat2 <cr_cat>, z$cat2 <cr_cat>

