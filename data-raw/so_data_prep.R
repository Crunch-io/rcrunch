library(tidyverse)
library(stringr)
stack_df <- read_csv("data-raw/survey_results_public.csv")

uses_r <- stack_df$HaveWorkedLanguage %>%
    str_detect("R\\;|R$" )

r_df <- stack_df[uses_r, ] %>%
    filter(!is.na(HaveWorkedLanguage))

classes <- data_frame(name = names(r_df),
    class = map_chr(r_df, class))
SO_survey <- r_df %>%
    select(
        Respondent,
        Professional,
        Country,
        CompanySize,
        CareerSatisfaction,
        JobSatisfaction,
        starts_with("Important"),
        Gender,
        Race,
        Country,
        Salary,
        ExpectedSalary,
        TabsSpaces,
        WantWorkLanguage,
        HaveWorkedLanguage
    )

save(SO_survey, file = "data/SO_survey.rda")
