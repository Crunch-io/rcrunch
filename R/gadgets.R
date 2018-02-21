#' Open dataset selector
#'
#' @return
#' @export
#' @importFrom miniUI gadgetTitleBar miniContentPanel miniPage
#' @importFrom rstudioapi insertText
#' @importFrom shiny runGadget stopApp observeEvent column uiOutput selectInput renderUI
listDatasetGadget <- function(kind, selected_project, refresh ){
    projects <- names(projects())
    datasets <- lapply(projects, function(x) {
        listDatasets(project = x, kind = kind, refresh = refresh)
    })
    names(datasets) <- projects

    ui <- miniPage(
        gadgetTitleBar("Drag to select points"),
        miniContentPanel(
            column(width = 6,
                selectInput("project",
                    "Select Project",
                    projects,
                    selected = selected_project)),
            column(width = 6,
                uiOutput("dataset"))
        )
    )

    server <- function(input, output, session) {
        output$dataset <- renderUI({
            selectInput("dataset", "Select Dataset", datasets[[input$project]])
        })

        observeEvent(input$done, {
            code <- paste0("loadDataset('", input$dataset, "', project = '", input$project, "')" )
            stopApp(returnValue = insertText(text = code))
        })
    }
    runGadget(ui, server)
}

