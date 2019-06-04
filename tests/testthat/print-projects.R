## These are moved from test-variable-folder.R thanks to R's archaic restriction on UTF-8
expect_output(print(projects(), depth = 2),
    "/
├── Project One/
│   ├── an archived dataset
│   ├── Project Two/
│   │   ├── ECON.sav
│   │   └── streaming test ds
│   ├── test ds
│   ├── Project Five/
│   └── streaming no messages
└── Project Three/",
    fixed = TRUE
)

expect_output(print(projects(), depth = 1),
    "/
├── Project One/
│   ├── an archived dataset
│   ├── Project Two/
│   ├── test ds
│   ├── Project Five/
│   └── streaming no messages
└── Project Three/",
    fixed = TRUE
)

expect_output(print(projects(), pretty = TRUE),
    "/
├── Project One/
└── Project Three/",
    fixed = TRUE
)

expect_output(print(cd(projects(), "Project One"), depth = 1),
    "/Project One/
├── an archived dataset
├── Project Two/
│   ├── ECON.sav
│   └── streaming test ds
├── test ds
├── Project Five/
└── streaming no messages",
    fixed = TRUE
)
