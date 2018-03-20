
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


makeArrayGadget <- function(ds, is_mr = FALSE){
    #display categorical variables
    vars <- names(variables(ds))[vapply(ds, is.Categorical, FUN.VALUE = logical(1))]
    currently_selected <- ""
    ui <- miniUI::miniPage(
        # shiny::tags$head(
        #     shiny::tags$style(shiny::HTML("
        #          .multicol {
        #            -webkit-column-count: 3; /* Chrome, Safari, Opera */
        #            -moz-column-count: 3; /* Firefox */
        #            column-count: 3;
        #          }
        #        "
        #     )
        #     )
        # ),
        miniUI::gadgetTitleBar("Select Dataset"),
        miniUI::miniTabstripPanel(
            miniUI::miniTabPanel( "Select Variables",
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
            miniUI::miniTabPanel( "Build Variable",
                miniUI::miniContentPanel(
                    shiny::textInput("obj_name", "Object Name (Optional)"),
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
                selected =  values$currently_selected
            )
        })

        output$select_categories <- shiny::renderUI({
            cats <- lapply(input$selected_vars, function(x){
                names(categories(ds[[x]]))
            })
            cats_consistent <- vapply(cats, function(x){
                identical(cats[[1]], x)
            }, FUN.VALUE = logical(1))
            if(length(input$selected_vars) == 0) {
                print(input$selected_vars)
                shiny::p("Error: No variables selected.",
                    style = "color:red")
            } else if (!all(cats_consistent)) {
                shiny::p("Error: selected variables have inconsistent categories.",
                    style = "color:red")
            } else if (input$array_type == "Multiple Response") {
                shiny::checkboxGroupInput("mr_selection",
                    "Selection Categories",
                    choices = cats[[1]]
                )
            }
        })
    }
    shiny::runGadget(ui,
        server,
        viewer = shiny::dialogViewer("MR Builder", width = 800),
            )
}

makeArrayGadget(ds, is_mr = FALSE)
