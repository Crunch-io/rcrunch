#' Open dataset selector
#'
#' @inheritParams listDatasets
#' @return A `loadDataset()` call is pasted into your RStudio session`
#' @keywords internal
listDatasetGadget <- function(kind = c("active", "all", "archived"),
                              refresh = FALSE) {
    projects <- c("Personal Project", names(projects()))
    personal_datasets <- listDatasets(kind = kind, refresh = refresh)

    ui <- miniUI::miniPage(
        miniUI::gadgetTitleBar("Select Dataset"),
        miniUI::miniContentPanel(
            shiny::column(
                width = 3,
                shiny::selectInput(
                    "project",
                    "Select Project",
                    projects
                )
            ),
            shiny::column(
                width = 3,
                shiny::uiOutput("dataset")
            ),
            shiny::column(
                width = 3,
                shiny::textInput("ds_name", "Object Name (Optional)")
            )
        )
    )

    server <- function(input, output, session) {
        output$dataset <- shiny::renderUI({
            if (input$project == "Personal Project") {
                selections <- personal_datasets
            } else {
                selections <- listDatasets(project = input$project, kind = kind, refresh = refresh)
            }
            shiny::selectInput(
                "dataset",
                "Select Dataset",
                selections
            )
        })
        shiny::observeEvent(input$done, {
            code <- buildLoadDatasetCall(
                project = input$project,
                dataset = input$dataset,
                ds_name = input$ds_name
            )
            shiny::stopApp(returnValue = rstudioapi::insertText(text = code))
        })
    }
    shiny::runGadget(ui, server)
}

buildLoadDatasetCall <- function(project, dataset, ds_name = "") {
    dataset <- escapeQuotes(dataset)
    assignment <- ifelse(nchar(ds_name) > 0,
        paste0(ds_name, " <- "),
        ""
    )
    if (project == "Personal Project") {
        code <- paste0(assignment, "loadDataset('", dataset, "')")
    } else {
        code <- paste0(
            assignment, "loadDataset('",
            dataset,
            "', project = '",
            escapeQuotes(project), "')"
        )
    }
    return(code)
}
