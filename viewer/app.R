library("rjson")
library(tidyverse)
library(shiny)
library(shinyWidgets)
library(DT)
library(httr)
library(curl)

source("helpers.R")

httr::set_config(httr::config(http_version = 1))

# sets the default number of rows to 50 so that all of the data will be shown 
options(
    DT.options = list(
        pageLength = 50,
        searching = FALSE,
        lengthChange = FALSE,
        paging = FALSE
    )
)

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


server <- function(input, output, session) {
    # Get the latest API Data
    select_choices <- list(
        "Please select..." = 0
    )
    
    api_data <- genestack_api_call("studyUser", "studies")
    study_data <- api_data # Copy the data so it doesn't need to recall API

    full_data <- list() # This is where we put all the extra info, for use by the search
    study_indexes <- list()

    if (length(study_data) == 1) {
        print(study_data)
        quit(status = 1)
    }

    for (i in 1:length(study_data[["data"]])) {
        select_choices[[format_title(study_data[["data"]][[i]])]] <- i
        study_indexes[[study_data[["data"]][[i]][["genestack:accession"]]]] <- i
        full_data[[study_data[["data"]][[i]][["genestack:accession"]]]] <- get_study_additional_data(study_data[["data"]][[i]][["genestack:accession"]])
    }

    updateSelectInput(
        session,
        "select",
        choices = select_choices
    )


    add_data <- list()
    summary_table <- data.frame()
    search_results_data <- list()

    observeEvent(input$search, {
        if (input$search != ""){
            output$no_results = renderText({"Searching..."})
            results = accessions_to_titles(search_studies(input$search, full_data), study_data)
            if (nrow(results) != 0) {
                search_results_data <<- results
                output$search_results = renderDataTable({
                    return(
                        datatable(
                            results["titles"],
                            caption = "Search Results (related studies)",

                            rownames = FALSE,
                            colnames = NULL,
                            selection = "single",

                            # TODO: See other escape TODO
                            escape = FALSE
                        )
                    )
                })
                output$no_results = NULL
            } else {
                output$search_results = NULL
                output$no_results = renderText({"No Search Results"})
            }
        }
    })
    
    observeEvent(input$search_reset, {
        output$search_results = NULL
    })

    # runs whenever a row is clicked
    observeEvent(input$search_results_rows_selected, {
        clicked = input$search_results_rows_selected
        acc = search_results_data[clicked,][["accessions"]]
        index = study_indexes[[acc]]

        # updates the selection box to show the new choice
        updateSelectInput(
            session = session,
            "select",
            selected = index
        )
    })
    
    
    #updates whenever the select box is changed and passes the index of the data that needs to be formatted 
    observeEvent(input$select, {
        
        if (input$select == 0) {
            # "Please Select" Option
            return(NULL)
        }

        # Transpose and put the key/value data into the UI table
        transposed = find_study(input$select, study_data)
        colnames(transposed) <- c("key","value")
        output$study_meta = renderDataTable({
            return(
                datatable(
                    transposed,
                    caption = "Study Data",
                    rownames = FALSE,

                    # This allows line breaks (<br>), but could allow XSS
                    # TODO: Better way of doing line breaks
                    escape = FALSE
                )
            )
            
        })

        # Deal with all the additonal study data
        # 1. Get the Study ID
        study_id <- filter(transposed, key == "genestack:accession")[["value"]]

        # 2. Get the Additional Data
        add_data <<- full_data[[study_id]]

        # 3. Summarise 
        data_types <- c()
        data_descrs <- c()
        data_ids <- c()

        for (type in names(add_data)) {
            if (is.list(add_data[type])) {
                for (data_part in add_data[[type]]) {
                    data_types <- append(data_types, str_to_title(type))
                    data_descrs <- append(data_descrs, data_part[["metadata"]][["Description"]])
                    data_ids <- append(data_ids, data_part[["itemId"]])
                }
            }
        }

        # 4. Make the Nice Summary Table
        summary_table <<- data.frame(data_types, data_descrs, data_ids)

        if (ncol(summary_table) != 0) {
            output$additional_summary = renderDataTable({
                return(
                    datatable(
                        summary_table[1:2],
                        caption = "Available Additional Data",

                        selection = "single",
                        rownames = FALSE,
                        colnames = c("", "Description")
                    )
                )
            })
        } else {
            output$additional_summary = NULL
        }

        # Hide the Old Additional Data
        output$additional_meta = NULL

    })

    # Display Additional Data When Requested
    observeEvent(input$additional_summary_rows_selected, {
        # 1. Get the ID of the data we want
        clicked = input$additional_summary_rows_selected
        req_id <- summary_table[clicked,][["data_ids"]]
        req_type <- tolower(summary_table[clicked,][["data_types"]])

        # 2. Get all of the neccesary data
        data_form <- add_data[[req_type]]
        requested_data <- format_json(Filter(function(elem) elem[["itemId"]] == req_id, data_form)[[1]][["metadata"]])
        colnames(requested_data) <- c("key", "value")

        # 3. Put the metadata in the Shiny table
        output$additional_meta = renderDataTable({
            return(
                datatable(
                    requested_data,
                    caption = "Additional Data",

                    rownames = FALSE,
                    escape = FALSE # Same issue here as other places
                )
            )
        })

    })
}

# Run the application 
shinyApp(ui = ui, server = server)
