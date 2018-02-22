#' Open dataset selector
#'
#' This function launches a shiny gadget which allows you to navigate your Crunch
#' projects and datasets. This is useful if you can't remember a dataset's project
#' and also saves typing long dataset names.
#'
#' @inheritParams listDatasets
#' @return A `listDatasets()` call is pasted into your RStudio session`
#' @export
listDatasetGadget <- function(kind=c("active", "all", "archived"),
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
            shiny::selectInput("dataset",
                "Select Dataset",
                selections)
        })
        shiny::observeEvent(input$done, {

            assignment <- ifelse(nchar(input$ds_name) > 0,
                paste0(input$ds_name, " <- "),
                "")
            if (input$project == "Personal Project") {
                code <- paste0(assignment, "loadDataset('", input$dataset,"')")
            } else {
                code <- paste0(assignment, "loadDataset('",
                    input$dataset,
                    "', project = '",
                    input$project, "')" )
            }
            shiny::stopApp(returnValue = rstudioapi::insertText(text = code))
        })
    }
    shiny::runGadget(ui, server)
}