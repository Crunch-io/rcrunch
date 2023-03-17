# Make a dataset with a wide variety of metadata that also has
# some trends in it that can be interpreted.
# Fictitious data about a survey on attitudes towards vegetables.
# - Variable types: Categorical, Numeric, Text, Date,
#   Categorical Array, Multiple Response, Numeric Array &
#   Categorical Datetime
# - Folder locations: Root, in folder, Hidden & Secure
# - Has a weight

# TODO: Get this metadata into `newExampleDataset`?
options(crunch.stabilize.query = TRUE)
set.seed(2020-12-22)
num_waves <- 7
people_per_wave <- 30
pct_missing <- 0.05

library(dplyr)
library(crunch)
library(httptest)
library(fs)
library(here)
library(purrr)

setupCrunchAuth("team")

source(here("dev-misc/fixture-creation/redactors.R"))

if (!"Vegetables fixture" %in% names(projects())) {
    stop("Must have project named 'Vegetables fixture' with correct palette")
}

if ("Vegetables example" %in% listDatasets()) {
    stop("Must not already have a dataset named 'Vegetables example'")
}

# Setup dataset ----
## Data generation helpers ----
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


## Make R data.frame -----
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
    ) %>% if_else(. == "Disagree", factor("Neither", levels = levels(.)), .),
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

# --- 2021-08-18 add a funnel-style set of MR variables
vegetables <- vegetables %>%
    mutate(
        funnel_aware_1 = random_cat_gen(
            1, healthy_eater == "Yes",
            levels = c("Yes", "No"),
            breaks = 0.7
        ),
        funnel_aware_2 = random_cat_gen(
            -2, age,
            levels = c("Yes", "No"),
            breaks = 0.4
        ),
        funnel_consider_1 = random_cat_gen(
            1, healthy_eater == "Yes",
            levels = c("Yes", "No"),
            breaks = 0.5
        ),
        funnel_consider_2 = random_cat_gen(
            -2, age,
            levels = c("Yes", "No"),
            breaks = 0.85
        ),
        funnel_buy_1 = random_cat_gen(
            1, healthy_eater == "Yes",
            levels = c("Yes", "No"),
            breaks = 0.5
        ),
        funnel_buy_2 = random_cat_gen(
            -2, age,
            levels = c("Yes", "No"),
            breaks = 0.8
        ),
    ) %>%
    mutate(across(starts_with("funnel"), function(x) {
        missings <- runif(length(x)) < pct_missing
        x[missings] <- NA
        x
    })) %>%
    mutate(
        funnel_aware_1 = factor(funnel_aware_1, levels = c("Yes", "No", "N/A")),
        funnel_aware_2 = factor(funnel_aware_2, levels = c("Yes", "No", "N/A")),
        funnel_consider_1 = factor(
            case_when(
                funnel_aware_1 %in% c("No", "N/A") ~ "N/A",
                is.na(funnel_aware_1) ~ NA_character_,
                TRUE ~ as.character(funnel_consider_1)
            ),
            levels = c("Yes", "No", "N/A")
        ),
        funnel_consider_2 = factor(
            case_when(
                funnel_aware_2  %in% c("No", "N/A") ~ "N/A",
                is.na(funnel_aware_2) ~ NA_character_,
                TRUE ~ as.character(funnel_consider_2)
            ),
            levels = c("Yes", "No", "N/A")
        ),
        funnel_buy_1 = factor(
            case_when(
                funnel_consider_1 %in% c("No", "N/A") ~ "N/A",
                is.na(funnel_consider_1) ~ NA_character_,
                TRUE ~ as.character(funnel_buy_1)
            ),
            levels = c("Yes", "No", "N/A")
        ),
        funnel_buy_2 = factor(
            case_when(
                funnel_consider_2 %in% c("No", "N/A") ~ "N/A",
                is.na(funnel_consider_2) ~ NA_character_,
                TRUE ~ as.character(funnel_buy_2)
            ),
            levels = c("Yes", "No", "N/A")
        )
    )

## Setup crunch dataset ----
ds <- newDataset(vegetables, "Vegetables example")

## Set variable metadata ----
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
set_var_meta(
    ds$funnel_aware_1,
    name = "Aware of Jicama",
    description = "Awareness MR - Jicama",
    notes = "Have you ever heard of the vegetable Jicama?"
)
is.na(categories(ds$funnel_aware_1)["N/A"]) <- TRUE
set_var_meta(
    ds$funnel_aware_2,
    name = "Aware of Kohlrabi",
    description = "Awareness MR - Kohlrabi",
    notes = "Have you ever heard of the vegetable Kohlrabi?"
)
is.na(categories(ds$funnel_aware_2)["N/A"]) <- TRUE
set_var_meta(
    ds$funnel_consider_1,
    name = "Consider Jicama",
    description = "Consideration MR - Jicama",
    notes = "Have you ever consdidered buying Jicama?"
)
is.na(categories(ds$funnel_consider_1)["N/A"]) <- TRUE
set_var_meta(
    ds$funnel_consider_2,
    name = "Consider of Kohlrabi",
    description = "Consideration MR - Kohlrabi",
    notes = "Have you ever consdidered buying Kohlrabi"
)
is.na(categories(ds$funnel_consider_2)["N/A"]) <- TRUE
set_var_meta(
    ds$funnel_buy_1,
    name = "Bought Jicama",
    description = "Purchase MR - Jicama",
    notes = "Have you ever bought Jicama?"
)
is.na(categories(ds$funnel_buy_1)["N/A"]) <- TRUE
set_var_meta(
    ds$funnel_buy_2,
    name = "Bought of Kohlrabi",
    description = "Purchase MR - Kohlrabi",
    notes = "Have you ever bought Kohlrabi?"
)
is.na(categories(ds$funnel_buy_2)["N/A"]) <- TRUE
ds <- refresh(ds)

## Derive arrays ----
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


ds$funnel_aware_mr <- deriveArray(
    list(
        VarDef(ds$funnel_aware_1, alias = "funnel_aware_mr_1", name = "Jicama"),
        VarDef(ds$funnel_aware_2, alias = "funnel_aware_mr_2", name = "Kohlrabi")
    ),
    "Awareness of Vegetables",
    description = "Awareness of Vegetables: Funnel",
    notes = "Have you ever heard of the vegetable...?",
    selections = "Yes",
    numeric = FALSE
)
subtotals(ds$funnel_aware_mr) <- list(
    Subtotal("Jicama or Kohlrabi", c("funnel_aware_mr_1", "funnel_aware_mr_2"), position = "top")
)

ds$funnel_consider_mr <- deriveArray(
    list(
        VarDef(ds$funnel_consider_1, alias = "funnel_consider_mr_1", name = "Jicama"),
        VarDef(ds$funnel_consider_2, alias = "funnel_consider_mr_2", name = "Kohlrabi")
    ),
    "Consider Vegetables",
    description = "Consideration of Vegetables: Funnel",
    notes = "Have you ever considered buying...?",
    selections = "Yes",
    numeric = FALSE
)
subtotals(ds$funnel_consider_mr) <- list(
    Subtotal("Jicama or Kohlrabi", c("funnel_consider_mr_1", "funnel_consider_mr_2"), position = "top")
)

ds$funnel_buy_mr <- deriveArray(
    list(
        VarDef(ds$funnel_buy_1, alias = "funnel_buy_mr_1", name = "Jicama"),
        VarDef(ds$funnel_buy_2, alias = "funnel_buy_mr_2", name = "Kohlrabi")
    ),
    "Buy Vegetables",
    description = "Purchased Vegetables: Funnel",
    notes = "Have you ever bought...?",
    selections = "Yes",
    numeric = FALSE
)
subtotals(ds$funnel_buy_mr) <- list(
    Subtotal("Jicama or Kohlrabi", c("funnel_buy_mr_1", "funnel_buy_mr_2"), position = "top")
)

hideVariables(ds, c(
    "funnel_aware_1", "funnel_aware_2", "funnel_consider_1", "funnel_consider_2",
    "funnel_buy_1", "funnel_buy_2"
))


## Other metadata ----
values(categories(ds$wave)) <- NA
dates(categories(ds$wave)[!is.na(categories(ds$wave))]) <- format(seq(as.Date("2019-01-01"), length.out = sum(!is.na(categories(ds$wave))), by = "month"), "%Y-%m")

weightVariables(ds) <- ds$weight

privatizeVariables(ds, "resp_id")
mv(ds, c("weight", "last_vegetable", "last_vegetable_date"), "Survey variables")

# Move to correct project so that it inherits the right palettes
mv(projects()[["Vegetables fixture"]], ds, projects()[["Vegetables fixture"]])
ds <- refresh(ds)

## Multitables ----
mt <- newMultitable(
    ~ds$enjoy_mr + ds$healthy_eater,
    ds,
    "cat + mr multitable"
)
## Decks ----
deck <- newDeck(ds, "1 deck about transforms")
slide1 <- newSlide(deck, ~ds$healthy_eater, title = "No transforms")
slide2 <- newSlide(
    deck,
    ~ds$healthy_eater,
    title = "Yes transforms",
    transform = list(rows_dimension = makeDimTransform(hide = "No"))
)

deck <- newDeck(ds, "2 deck about printing")
slide1 <- newSlide(
    deck,
    ~ds$healthy_eater,
    title = "donut",
    display_settings = list(vizType = "donut")
)

slide2 <- newSlide(
    deck,
    ~subvariables(ds$veg_enjoy_ca) + categories(ds$veg_enjoy_ca),
    title = "table with filter and weight",
    subtitle = "and a subtitle",
    filter = ds$age > 18,
    weight = ds$weight
)

slide3 <- newMarkdownSlide(
    deck,
    "*markdown goes here*",
    subtitle = "markdown slide"
)

# Capture fixtures ----
set_redactor(response_redactor(ds, "veg"))
set_requester(request_redactor(ds, "veg"))
## Capture dataset fixtures ----
### General dataset capture ----
temp_dir <- tempfile()
httpcache::clearCache()
dir_create(temp_dir)

start_capturing(temp_dir)
ds <- loadDataset("Vegetables example")
mt <- multitables(ds)[[1]]
tb <- tabBook(mt, ds[c("healthy_eater", "veg_enjoy_ca", "enjoy_mr", "age", "ratings_numa")])

deck <- decks(ds)[[1]]
slide1 <- slides(deck)[[1]]
analyses1 <- analyses(slide1)
cube1 <- cube(analyses1[[1]])
slide2 <- slides(deck)[[2]]
analyses2 <- analyses(slide2)
cube2 <- cube(analyses2[[1]])

deck <- decks(ds)[[2]]
slide1 <- deck[[1]]
slide2 <- deck[[2]]
slide3 <- deck[[3]]
wt <- crGET(self(ds$weight)) # needed for printing of slide with weights

# newSlide() uses a slightly different query (I think it has filter=NULL instead of no filter)
# than getting the cube from a slide. So for the newSlide test, we make this request explicitly
cube3 <- crtabs(~ds$healthy_eater, ds)

# as.vector and as.data.frame captures
ds_dim <- dim(ds)
cat <- as.vector(ds$healthy_eater)
cat_names <- names(categories(ds$healthy_eater))
num <- as.vector(ds$age)
ca <- as.vector(ds$veg_enjoy_ca)
mr <- as.vector(ds$enjoy_mr)
mr_sub <- as.vector(ds$enjoy_mr$enjoy_mr_savory)
mr_id <- mr <- as.vector(ds$enjoy_mr, mode = "id")
mr_id_sub <- mr <- as.vector(ds$enjoy_mr$enjoy_mr_savory, mode = "id")
numa <- as.vector(ds$ratings_numa)

cdf <- as.data.frame(ds, include.hidden = TRUE)

# Don't actually export because we'll save the fixture somewhere else,
# but we do need the export views
exporters <- crGET(shojiURL(ds, "views", "export"))
var_meta <- variableMetadata(ds)

# MR insertion info
subtotals(ds$funnel_aware_mr)

stop_capturing()

### Cleanup and move dataset capture ----
# File level modifications needed to scrub attributes that change over time
stabilize_json_files(
    temp_dir,
    list(
        "app.crunch.io/api/datasets/by_name/Vegetables%20example.json",
        list(list("index", 1, "current_editor_name"), "User"),
        list(list("index", 1, "owner_name"), "User"),
        list(list("index", 1, "creation_time"), "2021-01-01T21:25:59.791000"),
        list(list("index", 1, "modification_time"), "2021-01-01T21:26:43.038000"),
        list(list("index", 1, "access_time"), "2021-01-01T21:26:43.038000")
    ),
    list(
        "app.crunch.io/api/datasets/veg.json",
        list(list("body", "current_editor_name"), "User"),
        list(list("body", "owner_name"), "User"),
        list(list("body", "creation_time"), "2021-01-01T21:25:59.791000"),
        list(list("body", "modification_time"), "2021-01-01T21:26:43.038000"),
        list(list("body", "access_time"), "2021-01-01T21:26:43.038000"),
        list(
            # --- Only keep the palettes from the project folder so changes to crunch org
            # --- don't affect fixtures. Maybe it'd be better to ask for a rcrunch test
            # --- account, but this is okay for now
            list("body", "palette", "analysis"),
            function(x) {
                purrr::keep(x, ~.$name %in% c("Default green palette for fixture", "purple palette for fixture"))
            }
        ),
        list(list("urls", "owner_url"), "https://app.crunch.io/api/projects/pid/")

    ),
    list(
        "app.crunch.io/api/datasets/veg/decks.json",
        list(list("index", 1, "creation_time"), "2021-01-01T21:29:59.791000"),
        list(list("index", 2, "creation_time"), "2021-01-02T21:29:59.792000")
    ),
    list(
        "app.crunch.io/api/datasets/veg/decks/dk01.json",
        list(list("body", "creation_time"), "2021-01-01T21:29:59.791000")
    ),
    list(
        "app.crunch.io/api/datasets/veg/decks/dk02.json",
        list(list("body", "creation_time"), "2021-01-02T21:29:59.792000")
    )
)

# POST files contain lots of info that changes between runs, so delete it
# and use `with_POST()`
path(temp_dir, "app.crunch.io/api/datasets/veg/multitables/mt_01") %>%
    dir_ls(glob = "*POST.R") %>%
    file_delete()

# Now move to the mocks folder
file_copy(
    path(temp_dir, "app.crunch.io/api/datasets/by_name/Vegetables%20example.json"),
    here("mocks/app.crunch.io/api/datasets/by_name/Vegetables%20example.json"),
    overwrite = TRUE
)

file_copy(
    path(temp_dir, "app.crunch.io/api/datasets/veg.json"),
    here("mocks/app.crunch.io/api/datasets/veg.json"),
    overwrite = TRUE
)

dir_delete(here("mocks/app.crunch.io/api/datasets/veg/"))
dir_copy(
    path(temp_dir, "app.crunch.io/api/datasets/veg/"),
    here("mocks/app.crunch.io/api/datasets/veg/"),
    overwrite = TRUE
)

file_copy(
    dir_ls(temp_dir, glob = "*player-crunch-io.s3.amazonaws.com*.json", recurse = TRUE),
    here("mocks/app.crunch.io/api/datasets/veg/multitables/mt_01/cat-mr-tabbook.json"),
    overwrite = TRUE
)

dir_delete(temp_dir)

## Generate data.frame csv ----
### httptest balks when trying to capture `as.data.frame()` so
### use `write.csv` directly
write.csv(
    ds,
    here("mocks", "dataset-fixtures", "veg.csv"),
    categorical = "id",
    include.hidden = TRUE
)

write.csv(
    ds,
    here("mocks", "dataset-fixtures", "veg-no-hidden.csv"),
    categorical = "id",
    include.hidden = FALSE
)

## Generate cube fixtures ----
### Numeric array alone (numa.json) ----
httpcache::clearCache()
dir_create(temp_dir)

start_capturing(temp_dir)
cube <- crtabs(mean(ds$ratings_numa) ~ 1, ds)
stop_capturing()

cube_path <- dir_ls(
    temp_dir,
    regexp = "app.crunch.io/api/datasets/veg/cube-.{6}\\.json",
    recurse = TRUE
)
stabilize_json_files(path_dir(cube_path))
file_copy(cube_path, here("mocks", "cubes", "numa.json"), overwrite = TRUE)

dir_delete(temp_dir)


### Numeric array by categorical (numa-x-cat.json) ----
httpcache::clearCache()
dir_create(temp_dir)

start_capturing(temp_dir)
cube <- crtabs(mean(ds$ratings_numa) ~ ds$healthy_eater, ds)
stop_capturing()

cube_path <- dir_ls(
    temp_dir,
    regexp = "app.crunch.io/api/datasets/veg/cube-.{6}\\.json",
    recurse = TRUE
)
stabilize_json_files(path_dir(cube_path))
file_copy(cube_path, here("mocks", "cubes", "numa-x-cat.json"), overwrite = TRUE)

dir_delete(temp_dir)


### Numeric array by categorical (numa-x-mr.json) ----
httpcache::clearCache()
dir_create(temp_dir)

start_capturing(temp_dir)
cube <- crtabs(mean(ds$ratings_numa) ~ ds$enjoy_mr, ds)
stop_capturing()

cube_path <- dir_ls(
    temp_dir,
    regexp = "app.crunch.io/api/datasets/veg/cube-.{6}\\.json",
    recurse = TRUE
)
stabilize_json_files(path_dir(cube_path))

file_copy(cube_path, here("mocks", "cubes", "numa-x-mr.json"), overwrite = TRUE)

dir_delete(temp_dir)


### Generate failed async crunch automation failure
httpcache::clearCache()
dir_create(temp_dir)

start_capturing(temp_dir)
failed <- try(runCrunchAutomation(ds, "NOT A COMMAND", async = TRUE), silent = TRUE)
stop_capturing()

progress_path <- path(temp_dir, "app.crunch.io/api/progress.json")

file_copy(
    progress_path,
    here("mocks", "app.crunch.io", "api", "progress-failed-async-script.json"),
    overwrite = TRUE
)

sort_json_keys(path(here("mocks", "app.crunch.io", "api", "progress-failed-async-script.json")))
dir_delete(temp_dir)


# Cleanup ----
rm(deck) # Hopefully gets rid of weird message after sourcing script
with_consent(delete(ds))
