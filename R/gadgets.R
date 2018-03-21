# List Datasets ----
#' Open dataset selector
#'
#' @inheritParams listDatasets
#' @return A `loadDataset()` call is pasted into your RStudio session`
listDatasetGadget <- function (kind=c("active", "all", "archived"),
    refresh = FALSE,
    autoclose = FALSE){
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
                shiny::textInput("ds_name", "Object Name (Optional)")
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
makeArrayGadget <- function(ds) {
    #display categorical variables
    vars <- names(variables(ds))[vapply(ds, is.Categorical, FUN.VALUE = logical(1))]
    currently_selected <- ""
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
        miniUI::miniTabstripPanel(id ="tabstrip",
            miniUI::miniTabPanel(id = "var_select",
                title = "Select Subvariables",
                icon = shiny::icon("check-square"),
                miniUI::gadgetTitleBar("Select SubVariables",
                    right = NULL,
                    left = NULL),
                miniUI::miniContentPanel(
                    shiny::fluidRow(
                        shiny::column( width = 4,
                            shiny::textInput("search", "Search")
                        )
                    ),
                    shiny::h4("Select Subvariables"),
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
                    shiny::radioButtons("array_type", "Array Type",
                        choices = c("Categorical Array", "Multiple Response")),
                    shiny::uiOutput("select_categories")
                )
            )
        )
    )

    server <- function (input, output, session) {

        values <- shiny::reactiveValues(currently_selected = "")
        var_subset <- shiny::reactive({vars[grep(input$search, vars)]})

        # We need to keep track of which values have been selected even when
        # the variables which can be selected change because of the search bar. This
        # observer creates a list which keeps track of whatever has been selected or intentionally
        # deseledcted.
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
            generateCategoryCheckboxes(ds, input$selected_vars, input$array_type)
        })
        shiny::observeEvent(input$done, {
            browser()
            code <- buildArrayCall(
                ds,
                input$array_type,
                input$obj_name,
                input$var_name,
                input$selected_vars,
                input$mr_selection
            )
            shiny::stopApp(returnValue = rstudioapi::insertText(text = code))
        })
    }
    crGET(getOption("crunch.api"))
    shiny::runGadget(ui,
        server,
        viewer = shiny::dialogViewer("MR Builder", width = 800),
    )
}

generateCategoryCheckboxes <- function (ds, selected_vars, array_type) {
    cats <- lapply(selected_vars, function(x) {
        names(categories(ds[[x]]))
    })
    cats_consistent <- vapply(cats, function(x) {
        identical(cats[[1]], x)
    }, FUN.VALUE = logical(1))
    if (length(selected_vars) == 0) {
        shiny::p("Error: No variables selected.",
            style = "color:red")
    } else if (!all(cats_consistent)) {
        shiny::p("Error: selected variables have inconsistent categories.",
            style = "color:red")
    } else if (array_type == "Multiple Response") {
        shiny::checkboxGroupInput("mr_selection",
            "Selection Categories",
            choices = cats[[1]]
        )
    }
}

buildArrayCall <- function(ds,
    array_type,
    object_name = "",
    array_var_name,
    vars_selected,
    mr_selection){
    if (array_type == "Multiple Response") {
        f <- 'makeMR('
        sel <- paste0(", ", "selections = ", as_char_vector(mr_selection))
    } else {
        f <- 'makeArray('
        sel <- ''
    }

    if(object_name != "") {
        assign <- paste0(object_name, " <- ")
    } else {
        assign <- ""
    }


    call <-
        paste0(
            assign,
            f,
            as.character(substitute(ds)),
            "[ ,",
            as_char_vector(vars_selected),
            "], ",
            "name = ",
            array_var_name,
            sel,
            ")")
    return(call)
}

as_char_vector <- function(v) {
    paste0("c(",
        paste0(paste0("'", v, "'"), collapse = ", "),
        ")")
}