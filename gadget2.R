makeArrayGadget <- function(ds, is_mr = FALSE){
    vars <- names(variables(ds))
    ui <- miniUI::miniPage(
        tags$head(
            tags$style(HTML("
     .multicol {
       -webkit-column-count: 3; /* Chrome, Safari, Opera */
       -moz-column-count: 3; /* Firefox */
       column-count: 3;
     }
   "
            )
            )
        ),
        miniUI::gadgetTitleBar("Select Dataset"),
        miniUI::miniContentPanel(
            shiny::fluidRow(
                shiny::column( width = 4,
                    shiny::textInput("regex", "Regex")
                ),
                shiny::uiOutput("mr_selection"),
                shiny::column( width = 4,
                    shiny::textInput("obj_name", "Object Name (Optional)")
                )
            ),
            shiny::h4("Select Subvariables"),
            shiny::fluidRow(
                shiny::column(width = 4,
                    shiny::checkboxInput("select_all", "Select All")
                )
            ),
            shiny::fluidRow(
                tags$div(class = "multicol",
                    shiny::column(width = 12,
                        shiny::uiOutput("variables")
                    )
                )
            )
        )
    )

    server <- function (input, output, session) {

        output$mr_selection <- shiny::renderUI({
            if (is_mr) {
                shiny::column(width = 4,
                    shiny::textInput("selection", "Selection")
                )
            }
        })

        output$variables <- shiny::renderUI({
            var_subset <- vars[grep(input$regex, vars)]
            selections <- if(input$select_all) {
                var_subset
            } else {
                ""}
            shiny::checkboxGroupInput("selected_vars",
                NULL,
                choices = var_subset,
                selected =  selections
            )
        })
    }
    shiny::runGadget(ui,
        server,
        viewer = shiny::dialogViewer("MR Builder", width = 800))
}

#makeArrayGadget(ds, is_mr = FALSE)


buildArrayCall <- function(ds,
    is_mr,
    regex,
    object,
    variables,
    vars_selected,
    mr_selection){
    if (is_mr) {
        f <- 'makeMR('
        sel <- mr_selection
    } else {
        f <- 'makeArray('
        sel <- ''
    }
    if (regex != "" && all(vars_selected == variables)){
        var_regex <- paste0("grep(", regex, ')')
    } else {
        var_regex <- vars_selected
    }

    if(object != "") {
        assign <- paste0(" <- ", object)
    } else {
        assign <- ""
    }

    call <- paste0(
        assign,
        f,
        name(ds),
        "[,",
        var_regex,
        "])")
}

