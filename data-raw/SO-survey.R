stack_df <- read.csv("data-raw/survey_results_public.csv") ## This file is big and not checked into git

r_users <- grep("R;|R$", stack_df$HaveWorkedLanguage)
keepvars <- c(
    "Respondent",
    "Professional",
    "CompanySize",
    "CareerSatisfaction",
    "JobSatisfaction",
    grep("^ImportantHiring", names(stack_df), value=TRUE),
    "Gender",
    "Race",
    "Country",
    "Salary",
    "ExpectedSalary",
    "TabsSpaces",
    "WantWorkLanguage",
    "HaveWorkedLanguage"
)

SO_survey <- stack_df[r_users, keepvars]
SO_survey$WantWorkLanguage <- as.character(SO_survey$WantWorkLanguage)
SO_survey$HaveWorkedLanguage <- as.character(SO_survey$HaveWorkedLanguage)
## Fix some non-ASCII
factors <- vapply(SO_survey, is.factor, logical(1))
SO_survey[factors] <- lapply(SO_survey[factors], function (x) {
    levels(x) <- gsub("don.t know", "don't know", levels(x))
    return(x)
})

save(SO_survey, file = "data/SO_survey.rda")


### Schema

SO_schema <- read.csv("data/survey_results_schema.csv", stringsAsFactors = FALSE) #not checked in
SO_schema <- SO_schema[SO_schema$Column %in% names(SO_survey), ]
SO_schema <- SO_schema[match(names(SO_survey), SO_schema$Column), ]
save(SO_schema, file = "data/SO_schema.rda")
