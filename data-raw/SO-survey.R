stack_df <- read.csv("survey_results_public.csv") ## This file is big and not checked into git

r_users <- grep("R;|R$", df$HaveWorkedLanguage)
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
## Fix some non-ASCII
factors <- vapply(SO_survey, is.factor, logical(1))
SO_survey[factors] <- lapply(SO_survey[factors], function (x) {
    levels(x) <- gsub("don.t know", "don't know", levels(x))
    return(x)
})

save(SO_survey, file = "../data/SO_survey.rda")
