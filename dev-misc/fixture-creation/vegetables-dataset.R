# Make a dataset with a wide variety of metadata that also has
# some trends in it that can be interpreted.
# Fictitious data about a survey on attitudes towards vegetables.
# - Variable types: Categorical, Numeric, Text, Date,
#   Categorical Array, Multiple Response, Numeric Array &
#   Categorical Datetime
# - Folder locations: Root, in folder, Hidden & Secure
# - Has a weight

# TODO: Get this metadata into `newExampleDataset`?

set.seed(2020-12-22)
num_waves <- 7
people_per_wave <- 30
pct_missing <- 0.05

library(dplyr)
library(crunch)
login()

# Helpers ----
random_gen_func <- function(...) {
    dots <- list(...)
    coefs <- purrr::map_dbl(seq(1, length(dots), by = 2), ~dots[[.]])
    terms <- purrr::map(seq(1, length(dots), by = 2), ~dots[[. + 1]])

    explained <- purrr::map2(coefs, terms, ~.x * ((.y - mean(.y)) / sd(.y)))
    explained <- purrr::reduce(explained, `+`)
    explained <- rank(explained, ties.method = "random") / length(explained)

    # convuluted way to get random draws that are associated with
    # our explained variables but still have some variation
    # It's designed to set modal value be the rank in explained,
    # but to get variance looking good, I did trial and error
    alpha <- 1.2
    beta_draws <- rbeta(
        length(explained),
        alpha,
        (alpha - (explained * alpha) + (2 * explained) - 1) / explained
    )
    beta_draws
}

random_cat_gen <- function(..., levels, breaks) {
    values <- random_gen_func(...)
    cut(values, breaks = c(-Inf, breaks, Inf), labels = levels)
}

random_num_gen <- function(..., min = 0, max = 100) {
    values <- random_gen_func(...)
    round(values * (max - min) + min)
}

set_var_meta <- function(var, name = NULL, description = NULL, notes = NULL) {
    if (!is.null(name)) name(var) <- name
    if (!is.null(description)) description(var) <- description
    if (!is.null(notes)) notes(var) <- notes
    invisible(var)
}


# Make R data.frame -----
total_n <- num_waves * people_per_wave

vegetables <- tibble(
    wave = factor(
        rep(paste0("wave", seq_len(num_waves)), each = people_per_wave),
        levels = paste0("wave", seq_len(num_waves))
    ),
    age = sample(18:65, total_n, TRUE),
    healthy_eater = factor(sample(c("Yes", "No"), total_n, TRUE, c(0.6, 0.4))),
    enjoy_savory_food = random_cat_gen(
        2, age,
        1, healthy_eater == "Yes",
        levels = c("Yes", "No"),
        breaks = 0.4
    ),
    enjoy_spicy_food = random_cat_gen(
        -10, age,
        levels = c("Yes", "No"),
        breaks = 0.6
        ),
    enjoy_sweet_food = random_cat_gen(
        -2, age,
        5, healthy_eater == "Yes",
        levels = c("Yes", "No"),
        breaks = 0.8
    ),
    veg_healthy = random_cat_gen(
        4, healthy_eater == "Yes",
        -2, enjoy_sweet_food == "Yes",
        levels = c("Strongly Disagree", "Disagree",  "Neither", "Agree", "Strongly Agree"),
        breaks = c(0.2, 0.3, 0.4, 0.9)
    ),
    veg_tasty = random_cat_gen(
        -4, age,
        4, healthy_eater == "Yes",
        1, enjoy_savory_food == "Yes",
        levels = c("Strongly Disagree", "Disagree",  "Neither", "Agree", "Strongly Agree"),
        breaks = c(0.3, 0.4, 0.5, 0.8)
    ),
    veg_filling = random_cat_gen(
        2, healthy_eater == "Yes",
        1, enjoy_savory_food == "Yes",
        levels = c("Strongly Disagree", "Disagree",  "Neither", "Agree", "Strongly Agree"),
        breaks = c(0.1, 0.3, 0.6, 0.7)
    ),
    veg_environmental = random_cat_gen(
        -1, age,
        4, as.numeric(wave),
        levels = c("Strongly Disagree", "Disagree",  "Neither", "Agree", "Strongly Agree"),
        breaks = c(0.1, 0.2, 0.4, 0.9)
    ),
    rating_avocado = random_num_gen(
        -4, age, 3, as.numeric(veg_filling),  min = 60, max = 90
    ),
    rating_brussel_sprout = random_num_gen(
        4, as.numeric(wave), 2, enjoy_savory_food == "Yes", min = 30, max = 100
    ),
    rating_carrot = random_num_gen(
        -3, healthy_eater == "Yes", 3, rating_avocado, min = 60, max = 95
    ),
    rating_daikon = random_num_gen(
        1, healthy_eater == "Yes", 4, enjoy_spicy_food == "Yes", min = 50, max = 95
    ),
    rating_eggplant = random_num_gen(
        -3, as.numeric(wave), 2, as.numeric(veg_tasty), min = 50, max = 90
    ),
    rating_fennel = random_num_gen(
        2, enjoy_spicy_food == "Yes", 4, as.numeric(veg_tasty), min = 80, max = 95
    ),
    last_vegetable = sample(
        c("Avocado", "Carrot", "Green beans", "Lettuce", "Onion", "Pepper", "Tomato"),
        total_n,
        TRUE
    ),
    last_vegetable_date = sample(
        seq(as.Date("2019-01-01"), as.Date("2019-12-31"), by = "day"),
        total_n,
        TRUE
    ),
    resp_id = seq_along(wave),
    weight = ifelse(healthy_eater == "Yes", 0.8, 1.2)
) %>%
    mutate(across(c(-wave, -weight), function(x) {
        missings <- runif(length(x)) < pct_missing
        x[missings] <- NA
        x
    }))


# Setup crunch dataset ----
ds <- newDataset(vegetables, "Vegetables example")

# Set variable metadata ----
set_var_meta(
    ds$wave,
    name = "Survey Wave",
    description = "Wave of survey that participant was in"
)
set_var_meta(
    ds$age,
    name = "Age",
    description = "Age of respondent",
    notes = "How old are you?"
)
set_var_meta(
    ds$healthy_eater,
    name = "Healthy Eater",
    description = "Self identified 'Healthy eater' (binary)" ,
    notes = "Do you consider yourself a 'Healthy Eater' (eg someone who enjoys eating healthy food)?"
)
set_var_meta(
    ds$enjoy_savory_food,
    name = "Enjoy Savory Food",
    description = "Enjoyment of savory food (binary)",
    notes = "Do you typically enjoy food that is savory?"
)
set_var_meta(
    ds$enjoy_spicy_food,
    name = "Enjoy Spicy Food",
    description = "Enjoyment of spicy food (binary)",
    notes = "Do you typically enjoy food that is spicy?"
)
set_var_meta(
    ds$enjoy_sweet_food,
    name = "Enjoy Sweet Food",
    description = "Enjoyment of sweet food (binary)",
    notes = "Do you typically enjoy food that is sweet?"
)
set_var_meta(
    ds$veg_healthy,
    name = "Vegetable Reason: Healthy",
    description = "Vegetables are healthy (5 point scale)",
    notes = "Do you eat vegetables because: they are healthy"
)
set_var_meta(
    ds$veg_tasty,
    name = "Vegetable Reason: Tasty",
    description = "Vegetables are Tasty (5 point scale)",
    notes = "Do you eat vegetables because: they taste good"
)
set_var_meta(
    ds$veg_filling,
    name = "Vegetable Reason: Filling",
    description = "Vegetables are filling (5 point scale)",
    notes = "Do you eat vegetables because: they are filling"
)
set_var_meta(
    ds$veg_environmental,
    name = "Vegetable Reason: Environmental",
    description = "Vegetables are environmental (5 point scale)",
    notes = "Do you eat vegetables because: eating them is good for the environment"
)
set_var_meta(
    ds$rating_avocado,
    name = "Rating: Avocado",
    description = "Avocado Rating (100 point scale)",
    notes = "What is your rating between 0 and 100 of: Avocado"
)
set_var_meta(
    ds$rating_brussel_sprout,
    name = "Rating: Brussel Sprout",
    description = "Brussel Sprout Rating (100 point scale)",
    notes = "What is your rating between 0 and 100 of: Brussel Sprout"
)
set_var_meta(
    ds$rating_carrot,
    name = "Rating: Carrot",
    description = "Carrot Rating (100 point scale)",
    notes = "What is your rating between 0 and 100 of: Carrot"
)
set_var_meta(
    ds$rating_daikon,
    name = "Rating: Daikon",
    description = "Daikon Rating (100 point scale)",
    notes = "What is your rating between 0 and 100 of: Daikon"
)
set_var_meta(
    ds$rating_eggplant,
    name = "Rating: Eggplant",
    description = "Eggplant Rating (100 point scale)",
    notes = "What is your rating between 0 and 100 of: Eggplant"
)
set_var_meta(
    ds$rating_fennel,
    name = "Rating: Fennel",
    description = "Fennel Rating (100 point scale)",
    notes = "What is your rating between 0 and 100 of: Fennel"
)
set_var_meta(
    ds$last_vegetable,
    name = "Last Vegetable Eaten",
    description = "Name of last vegetable eaten (free text)",
    notes = "What is the last vegetable you ate?"
)
set_var_meta(
    ds$last_vegetable_date,
    name = "Last Vegetable Date",
    description = "Date when saw ate vegetable",
    notes = "On what day did you last ate a vegetable?"
)
set_var_meta(
    ds$weight,
    name = "Survey Weight",
    description = "Downweight the healthy eaters",
    notes = "survey weight note"
)
ds <- refresh(ds)

# Derive arrays ----
ds$enjoy_mr <- deriveArray(
    list(
        VarDef(ds$enjoy_savory_food, alias = "enjoy_mr_savory", name = "Savory"),
        VarDef(ds$enjoy_spicy_food, alias = "enjoy_mr_spicy", name = "Spicy"),
        VarDef(ds$enjoy_sweet_food, alias = "enjoy_mr_sweet", name = "Sweet")
    ),
    "Enjoy Food Flavors",
    selections = "Yes",
    numeric = FALSE,
    description = "Typically enjoy foods with flavors",
    notes = "Do you typically enjoy foods that taste..?"
)

hideVariables(ds, c("enjoy_savory_food", "enjoy_spicy_food", "enjoy_sweet_food"))

ds$veg_enjoy_ca <- deriveArray(
    list(
        VarDef(ds$veg_healthy, alias = "veg_enjoy_ca_healthy", name = "Healthy"),
        VarDef(ds$veg_tasty, alias = "veg_enjoy_ca_tasty", name = "Tasty"),
        VarDef(ds$veg_filling, alias = "veg_enjoy_ca_filling", name = "Filling"),
        VarDef(ds$veg_environmental, alias = "veg_enjoy_ca_env", name = "Environmental")
    ),
    "Reasons for Enjoying Vegetables",
    description = "Enjoy vegetables reasons (5 point scale)",
    notes = "To what extent do you enjoy vegetables because of...?",
    numeric = FALSE
)
hideVariables(ds, c("veg_healthy", "veg_tasty", "veg_filling", "veg_environmental"))

ds$ratings_numa <- deriveArray(
    list(
        VarDef(ds$rating_avocado, alias = "ratings_numa_avocado", name = "Avocado"),
        VarDef(ds$rating_brussel_sprout, alias = "ratings_numa_brussel_sprout", name = "Brussel Sprout"),
        VarDef(ds$rating_carrot, alias = "ratings_numa_carrot", name = "Carrot"),
        VarDef(ds$rating_daikon, alias = "ratings_numa_daikon", name = "Daikon"),
        VarDef(ds$rating_eggplant, alias = "ratings_numa_eggplant", name = "Eggplant"),
        VarDef(ds$rating_fennel, alias = "ratings_numa_fennel", name = "Fennel")
    ),
    "Vegetable Ratings",
    description = "Rating of Vegetables: Scale of 0-100",
    notes = "On a scale of 0-100, how would you rate...?",
    numeric = TRUE
)

hideVariables(ds, c(
    "rating_avocado", "rating_brussel_sprout", "rating_carrot", "rating_daikon",
    "rating_eggplant", "rating_fennel"
))

# Other metadata ----
values(categories(ds$wave)) <- NA
dates(categories(ds$wave)[!is.na(categories(ds$wave))]) <- format(seq(as.Date("2019-01-01"), length.out = sum(!is.na(categories(ds$wave))), by = "month"), "%Y-%m")

weightVariables(ds) <- ds$weight

privatizeVariables(ds, "resp_id")
mv(ds, c("weight", "last_vegetable", "last_vegetable_date"), "Survey variables")

ds <- refresh(ds)

