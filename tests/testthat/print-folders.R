## These are sadly unreadable thanks to R's archaic restriction on UTF-8
testthat::expect_output(print(folders(ds), depth=2),
"/
\u251C\u2500\u2500 Group 1/
\u2502   \u251C\u2500\u2500 Birth Year
\u2502   \u251C\u2500\u2500 Nested/
\u2502   \u2502   \u251C\u2500\u2500 Gender
\u2502   \u2502   \u251C\u2500\u2500 Categorical Location
\u2502   \u2502   \u2514\u2500\u2500 mymrset
\u2502   \u2514\u2500\u2500 Text variable ftw
\u2514\u2500\u2500 Group 2/
    \u251C\u2500\u2500 starttime
    \u2514\u2500\u2500 Cat Array", fixed=TRUE
)

testthat::expect_output(print(folders(ds), depth=1),
"/
\u251C\u2500\u2500 Group 1/
\u2502   \u251C\u2500\u2500 Birth Year
\u2502   \u251C\u2500\u2500 Nested/
\u2502   \u2514\u2500\u2500 Text variable ftw
\u2514\u2500\u2500 Group 2/
    \u251C\u2500\u2500 starttime
    \u2514\u2500\u2500 Cat Array", fixed=TRUE
)

testthat::expect_output(print(folders(ds), pretty=TRUE),
"/
\u251C\u2500\u2500 Group 1/
\u2514\u2500\u2500 Group 2/", fixed=TRUE
)

testthat::expect_output(print(folders(ds)[["Group 1/Nested"]], pretty=TRUE),
"/Group 1/Nested/
\u251C\u2500\u2500 Gender
\u251C\u2500\u2500 Categorical Location
\u2514\u2500\u2500 mymrset", fixed=TRUE
)

with(temp.option(crunch.delimiter="|"), {
    testthat::expect_output(print(folders(ds), depth=2),
"|
\u251C\u2500\u2500 Group 1|
\u2502   \u251C\u2500\u2500 Birth Year
\u2502   \u251C\u2500\u2500 Nested|
\u2502   \u2502   \u251C\u2500\u2500 Gender
\u2502   \u2502   \u251C\u2500\u2500 Categorical Location
\u2502   \u2502   \u2514\u2500\u2500 mymrset
\u2502   \u2514\u2500\u2500 Text variable ftw
\u2514\u2500\u2500 Group 2|
    \u251C\u2500\u2500 starttime
    \u2514\u2500\u2500 Cat Array", fixed=TRUE
    )
})
