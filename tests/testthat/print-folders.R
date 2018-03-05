## These are moved from test-variable-folder.R thanks to R's archaic restriction on UTF-8
expect_output(print(folders(ds), depth=2),
"/
├── Group 1/
│   ├── Birth Year
│   ├── Nested/
│   │   ├── Gender
│   │   ├── Categorical Location
│   │   └── mymrset
│   └── Text variable ftw
└── Group 2/
    ├── starttime
    └── Cat Array", fixed=TRUE
)

expect_output(print(folders(ds), depth=1),
"/
├── Group 1/
│   ├── Birth Year
│   ├── Nested/
│   └── Text variable ftw
└── Group 2/
    ├── starttime
    └── Cat Array", fixed=TRUE
)

expect_output(print(folders(ds), pretty=TRUE),
"/
├── Group 1/
└── Group 2/", fixed=TRUE
)

expect_output(print(folders(ds)[["Group 1/Nested"]], pretty=TRUE),
"/Group 1/Nested/
├── Gender
├── Categorical Location
└── mymrset", fixed=TRUE
)

with(temp.option(crunch.delimiter="|"), {
    expect_output(print(folders(ds), depth=2),
"|
├── Group 1|
│   ├── Birth Year
│   ├── Nested|
│   │   ├── Gender
│   │   ├── Categorical Location
│   │   └── mymrset
│   └── Text variable ftw
└── Group 2|
    ├── starttime
    └── Cat Array", fixed=TRUE
    )
})
