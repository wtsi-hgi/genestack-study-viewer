ui <- fluidPage(
    titlePanel("Genestack Study Viewer"),
    
    sidebarLayout(
        sidebarPanel(
            # Inputs
            selectInput(
                inputId = "select",
                label = "Projects",
                choices = list(
                    "Loading..." = 0
                ),
                selected = 0
            ),
            
            searchInput(
                inputId = "search",
                label = "Search Metadata",
                placeholder = "add * for wildcard",
                btnSearch = icon("search"),
                btnReset = icon("remove")
            ),

            textOutput("no_results"),
            dataTableOutput("search_results"),
            br(),
            dataTableOutput("additional_summary")
        ),

        mainPanel(
            # Data Tables
            dataTableOutput("study_meta"),
            br(),
            dataTableOutput("additional_meta")
        )

    ),

    tags$head(tags$style("tbody {cursor: pointer;}"))
)
