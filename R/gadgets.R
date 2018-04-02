# List Datasets ----
#' Open dataset selector
#'
#' @inheritParams listDatasets
#' @return A `loadDataset()` call is pasted into your RStudio session`
listDatasetGadget <- function (kind=c("active", "all", "archived"),
    refresh = FALSE){
    projects <- c("Personal Project", names(projects()))
    personal_datasets <- listDatasets(kind = kind, refresh = refresh)

    ui <- miniUI::miniPage(
        miniUI::gadgetTitleBar("Select Dataset"),
        miniUI::miniContentPanel(
            shiny::column(width = 3,
                shiny::selectInput("project",
                    "Select Project",
                    projects)),
            shiny::column(width = 3,
                shiny::uiOutput("dataset")
            ),
            shiny::column(width = 3,
                shiny::textInput("obj_name", "Object Name (Optional)")
            )
        )
    )

    server <- function (input, output, session) {
        output$dataset <- shiny::renderUI({
            if (input$project == "Personal Project") {
                selections <- personal_datasets
            } else {
                selections <- listDatasets(project = input$project, kind = kind, refresh = refresh)
            }
            shiny::selectInput("dataset",
                "Select Dataset",
                selections)
        })
        shiny::observeEvent(input$done, {
            code <- buildLoadDatasetCall(project = input$project,
                dataset = input$dataset,
                ds_name = input$ds_name)
            shiny::stopApp(returnValue = rstudioapi::insertText(text = code))
        })
    }
    shiny::runGadget(ui, server)
}

buildLoadDatasetCall <- function (project, dataset, ds_name = "") {
    dataset <- escapeQuotes(dataset)
    assignment <- ifelse(nchar(ds_name) > 0,
        paste0(ds_name, " <- "),
        "")
    if (project == "Personal Project") {
        code <- paste0(assignment, "loadDataset('", dataset,"')")
    } else {
        code <- paste0(assignment, "loadDataset('",
            dataset,
            "', project = '",
            escapeQuotes(project), "')" )
    }
    return(code)
}
# Array builder ----

#' Launch array builder gadget
#'
#' Categorical Array and Multiple Response variables can be difficult to
#' construct without being able to investigate the available variables, and
#' their categories. This shiny gadget lets you select subvariables from the
#' dataset list, and ensures that those variables have consistent categories. To
#' use the gadget you must have at least one CrunchDataset loaded into the global
#' environment.
#'
#' @return a valid call to `makeArray()` or `makeMR()`
#' @export
makeArrayGadget <- function(env = globalenv()){
    shiny::runGadget(app = .makeArrayGadget(env),
        viewer = shiny::dialogViewer("MR Builder", width = 800)
    )

}

.makeArrayGadget <- function(env) {
    crGET(getOption("crunch.api")) # check login status
    dataset_list <- getCrunchDatasets(env)
    ui <- miniUI::miniPage(
        shiny::tags$head(
            shiny::tags$style(shiny::HTML("
                 .multicol {
                   -webkit-column-count: 3; /* Chrome, Safari, Opera */
                   -moz-column-count: 3; /* Firefox */
                   column-count: 3;
                 }
               "
            )
            )
        ),
        miniUI::miniTabstripPanel(id = "tabstrip",
            miniUI::miniTabPanel(id = "var_select",
                title = "Select Subvariables",
                icon = shiny::icon("check-square"),
                miniUI::gadgetTitleBar("Select SubVariables",
                    right = NULL,
                    left = NULL),
                miniUI::miniContentPanel(
                    shiny::fluidRow(
                        shiny::column(width = 4,
                            shiny::selectInput("dataset",
                                "Select Dataset",
                                choices = names(dataset_list)
                            )
                        ),
                        shiny::column(width = 4,
                            shiny::textInput("search", "Search")
                        )
                    ),
                    shiny::h6("Select Subvariables"),
                    shiny::fluidRow(
                        shiny::column(width = 4,
                            shiny::actionButton("select_all", "Select All"),
                            shiny::actionButton("clear_all", "Clear")
                        )
                    ),
                    shiny::fluidRow(
                        shiny::tags$div(class = "multicol",
                            shiny::column(width = 12,
                                shiny::uiOutput("variables")
                            )
                        )
                    )
                )
            ),
            miniUI::miniTabPanel(id = "builder",
                title = "Build Array Variable",
                icon = shiny::icon("edit"),
                miniUI::gadgetTitleBar("Build Variable"),
                miniUI::miniContentPanel(
                    shiny::textInput("obj_name", "Object Name (Optional)"),
                    shiny::textInput("var_name", "Variable Name"),
                    shiny::radioButtons("array_type", "Variable Type",
                        choices = c("Categorical Array", "Multiple Response")),
                    shiny::uiOutput("select_categories")
                )
            )
        )
    )

    server <- function (input, output, session) {
        ds <- shiny::reactive(dataset_list[[shiny::req(input$dataset)]])
        vars <- shiny::reactive (
            names(variables(ds()))[vapply(ds(), is.Categorical, FUN.VALUE = logical(1))]
        )
        var_subset <- shiny::reactive(
            vars()[grep(input$search, vars())]
        )

        # We need to keep track of which values have been selected even when the
        # variables which can be selected change because of the search bar. This
        # observer creates a list which keeps track of whatever has been
        # selected or intentionally deseledcted.
        values <- shiny::reactiveValues(currently_selected = character(0))
        shiny::observe({
            values$currently_selected <- union(
                shiny::isolate(
                    values$currently_selected[!(values$currently_selected %in% var_subset())]
                ),
                input$selected_vars
            )
        })
        shiny::observeEvent(input$select_all,
            shiny::updateCheckboxGroupInput(session, "selected_vars", selected = var_subset())
        )
        shiny::observeEvent(input$clear_all,
            shiny::updateCheckboxGroupInput(session, "selected_vars", selected = "")
        )

        output$variables <- shiny::renderUI({
            shiny::checkboxGroupInput("selected_vars",
                NULL,
                choices = var_subset(),
                selected =  shiny::isolate(values$currently_selected)
            )
        })

        output$select_categories <- shiny::renderUI({
            generateCategoryCheckboxes(ds(),
                values$currently_selected,
                input$array_type
            )
        })

        shiny::observeEvent(input$done, {
            code <- buildArrayCall(
                input$dataset,
                input$array_type,
                input$obj_name,
                input$var_name,
                values$currently_selected,
                input$mr_selection
            )
            shiny::stopApp(returnValue = rstudioapi::insertText(text = code))
        })
    }
    return(shiny::shinyApp(ui, server))
}

generateCategoryCheckboxes <- function (ds, selected_vars, array_type) {
    if (length(selected_vars) == 0) {
        shiny::p("Error: No variables selected.",
            style = "color:red")
    } else if (array_type == "Multiple Response") {
        cats <- lapply(selected_vars, function(x) {
            names(categories(ds[[x]]))
        })
        mr_options <- Reduce(intersect, cats)

        if (length(mr_options) == 0) {
            shiny::p("Error: selected variables have no common categories.",
                style = "color:red")
        } else {
            shiny::checkboxGroupInput("mr_selection",
                "Selection Categories",
                choices = mr_options
            )
        }
    }
}

buildArrayCall <- function(ds_name,
    array_type,
    object_name = "",
    array_var_name,
    vars_selected,
    mr_selection){
    if (array_type == "Multiple Response") {
        f <- 'makeMR('
        sel <- paste0(", ", "selections = ", asCharVector(mr_selection))
    } else {
        f <- 'makeArray('
        sel <- ''
    }

    if (object_name != "") {
        assign <- paste0(object_name, " <- ")
    } else {
        assign <- ""
    }

    call <- paste0(
        assign,
        f,
        ds_name,
        "[ ,",
        asCharVector(vars_selected),
        "], ",
        "name = '",
        array_var_name,
        "'",
        sel,
        ")")
    return(call)
}

asCharVector <- function(v) {
    paste0("c(",
        paste0(paste0("'", v, "'"), collapse = ", "),
        ")")
}

getCrunchDatasets <- function(env) {
    l <- ls(envir = env)
    out <- lapply(l, function(x) get(x, envir = env))
    names(out) <- l
    out <- out[vapply(out, is.dataset, FUN.VALUE = logical(1))]
    if (length(out) == 0) {
        halt("No CrunchDatasets detected.")
    }
    return(out)
}
