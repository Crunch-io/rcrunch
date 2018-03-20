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
#
#
# makeArrayGadget <- function(ds, is_mr = FALSE){
#     vars <- names(variables(ds))
#     ui <- miniUI::miniPage(
#         tags$head(
#             tags$style(HTML("
#      .multicol {
#        -webkit-column-count: 3; /* Chrome, Safari, Opera */
#        -moz-column-count: 3; /* Firefox */
#        column-count: 3;
#      }
#    "
#             )
#             )
#         ),
#         miniUI::gadgetTitleBar("Select Dataset"),
#         miniUI::miniContentPanel(
#             shiny::fluidRow(
#                 shiny::column( width = 3,
#                     shiny::textInput("regex", "Regex")
#                 ),
#                 shiny::column( width = 3,
#                     shiny::textInput("name", "Variable Name"))
#                 shiny::uiOutput("mr_selection"),
#                 shiny::column( width = 3,
#                     shiny::textInput("obj_name", "Object Name (Optional)")
#                 )
#             ),
#             shiny::h4("Select Subvariables"),
#             shiny::fluidRow(
#                 shiny::column(width = 4,
#                     shiny::checkboxInput("select_all", "Select All")
#                 )
#             ),
#             shiny::fluidRow(
#                 tags$div(class = "multicol",
#                     shiny::column(width = 12,
#                         shiny::uiOutput("variables")
#                     )
#                 )
#             )
#         )
#     )
#
#     server <- function (input, output, session) {
#
#         output$mr_selection <- shiny::renderUI({
#             if (is_mr) {
#                 shiny::column(width = 3,
#                     shiny::textInput("selection", "Selection")
#                 )
#             }
#         })
#
#         output$variables <- shiny::renderUI({
#             var_subset <- vars[grep(input$regex, vars)]
#             selections <- if(input$select_all) {
#                 var_subset
#             } else {
#                 ""}
#             shiny::checkboxGroupInput("selected_vars",
#                 NULL,
#                 choices = var_subset,
#                 selected =  selections
#             )
#         })
#     }
#     shiny::runGadget(ui,
#         server,
#         viewer = shiny::dialogViewer("MR Builder", width = 800))
# }
#
# #makeArrayGadget(ds, is_mr = FALSE)
#
#
# buildArrayCall <- function(ds,
#     is_mr,
#     object,
#     vars_selected,
#     mr_selection){
#     if (is_mr) {
#         f <- 'makeMR('
#         sel <- mr_selection
#     } else {
#         f <- 'makeArray('
#         sel <- ''
#     }
#
#     if(object != "") {
#         assign <- paste0(" <- ", object)
#     } else {
#         assign <- ""
#     }
# browser()
#     call <- paste0(
#         assign,
#         f,
#         name(ds),
#         "[, c(",
#         paste0("'", escapeQuotes(vars_selected), "'", collapse = ", "),
#         ")])")
#     return(call)
# }
#
