#' Open dataset selector
#'
#' This function launches a shiny gadget which allows you to navigate your Crunch
#' projects and datasets. This is useful if you can't remember a dataset's project
#' and also saves typing long dataset names.
#'
#' @param selected_project Specifies the default project for the gadget
#' @inheritParams listDatasets
#' @return A `listDatasets()` call is pasted into your RStudio session`
#' @export
listDatasetGadget <- function(kind=c("active", "all", "archived"),
    selected_project = "Personal Project",
    refresh = FALSE){
    projects <- names(projects())
    datasets <- lapply(projects, function(x) {
        listDatasets(project = x, kind = kind, refresh = refresh)
    })
    names(datasets) <- projects
    #the default project is not returned by projects()
    datasets[["Personal Project"]] <- listDatasets(kind = kind, refresh = refresh)

    ui <- miniUI::miniPage(
        miniUI::gadgetTitleBar("Drag to select points"),
        miniUI::miniContentPanel(
            shiny::column(width = 6,
                shiny::selectInput("project",
                    "Select Project",
                    names(datasets),
                    selected = selected_project)),
            shiny::column(width = 6,
                shiny::uiOutput("dataset"))
        )
    )

    server <- function(input, output, session) {
        output$dataset <- shiny::renderUI({
            shiny::selectInput("dataset", "Select Dataset", datasets[[input$project]])
        })
        shiny::observeEvent(input$done, {
            if( input$project == "Personal Project") {
                code <- paste0("loadDataset('", input$dataset,"')")
            } else {
                code <- paste0("loadDataset('",
                    input$dataset,
                    "', project = '",
                    input$project, "')" )
            }
            shiny::stopApp(returnValue = rstudioapi::insertText(text = code))
        })
    }
    shiny::runGadget(ui, server)
}

