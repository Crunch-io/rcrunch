library(tidyverse)
library(stringr)
stack_df <- read_csv("data-raw/survey_results_public.csv")

uses_r <- stack_df$HaveWorkedLanguage %>%
    str_detect("R\\;|R$" )

r_df <- stack_df[uses_r, ] %>%
    filter(!is.na(HaveWorkedLanguage))

classes <- data_frame(name = names(r_df),
    class = map_chr(r_df, class))
crunch_exemplar <- r_df %>%
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

mock_lang_percents <- function(x){
    langs <- unlist(strsplit(x, "; "))
    out <- data.frame(matrix(nrow = 1, ncol = min(6, length(langs))))
    names(out) <- paste0("TimeSpentLang", 1:ncol(out))
    x <- runif(ncol(out))
    out[1, ] <- round(x/sum(x), 2)
    out
}

percent_df <- map_df(crunch_exemplar$HaveWorkedLanguage, mock_lang_percents)
SO_survey <- cbind(crunch_exemplar, percent_df)

save(SO_survey, file = "data/SO_survey.rda")
