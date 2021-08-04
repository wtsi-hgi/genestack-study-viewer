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
options(DT.options = list(pageLength = 50))

# search's a json file for selected word
search_json <- function(searched_word){
    # the main data frame that we will output
    data_frame<- data.frame()
    # a storage data frame we can remake so we can append it to the end of the main data frame.
    temp_data_frame = data.frame()
    # loops through all the studies
    for (i in 1:length(json_file["data"][[1]])){
        # stores our data that we want to add to the data frame
        data_temp = list()
        # loops through each line in the study
        for (j in 1:length(json_file["data"][[1]][[i]])){
            # if that line is null then ignore it
            if (!is.null(json_file["data"][[1]][[i]][[j]])){
                # variables to organize the array to make it easier to read now we are at the point we want to use it
                data = json_file["data"][[1]][[i]][[j]]
                title = json_file["data"][[1]][[i]][["Study Title"]]
                # if the line is not an array (has multiple lines of data inside it like location)
                if (!is.list(data)){
                    if (!is.null(data)){
                        # if the word we are searching for is in the data
                        if (grepl( toupper(searched_word), toupper(data), fixed = TRUE)){
                            # if the data is not a duplicate
                            if (!(data %in% data_temp)){
                                # add that line of data to the data list
                                data_temp = c(data,data_temp)
                            }
                        }
                    }
                }
                # if the data does have another array inside it
                else{
                    # loop through that array
                    for (k in 1:length(data)){
                        # if the word we are searching for is in the data
                        if (!is.null(data[[k]])){
                            if (grepl( toupper(searched_word), toupper(data[[k]]), fixed = TRUE)){
                                # if the data is not a duplicate
                                if (!(data[[k]] %in% data_temp)){
                                    # add that line of data to the data list
                                    data_temp = c(data[[k]],data_temp)
                                }
                            }
                        }
                    }
                }
            }
        }
        # if there is any data in the list
        if (length(data_temp) != 0){
            # make a data frame
            temp_data_frame = data.frame(
                # save the data to it
                Study_title = c(title),
                Study_data = c(str_c(data_temp, collapse = "<br>")),
                stringsAsFactors = FALSE
            )
            # append it to the main data frame
            data_frame <- rbind(temp_data_frame,data_frame)
        }
    }
    return(data_frame)
}

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
                placeholder = NULL,
                btnSearch = icon("search"),
                btnReset = icon("remove"),
                width = "450px"
            ),

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

    )
)


server <- function(input, output, session) {
    # stores a copy of the table so it can be accessed later
    global_store <- reactiveVal(NULL)

    # Get the latest API Data
    select_choices <- list(
        "Please select..." = 0
    )
    title_to_index <- list()
    
    api_data <- genestack_api_call("studyUser", "studies")
    study_data <- api_data # Copy the data so it doesn't need to recall API

    if (length(study_data) == 1) {
        print(study_data)
        quit(status = 1)
    }

    for (i in 1:length(study_data[["data"]])) {
        select_choices[[format_title(study_data[["data"]][[i]])]] <- i
        title_to_index[[study_data[["data"]][[i]][["Study Title"]]]] <- i
    }

    updateSelectInput(
        session,
        "select",
        choices = select_choices
    )


    add_data <- list()
    summary_table <- data.frame()

    observeEvent(input$search, {
        # if the input isn't empty then continue as grep breaks if it tries to search for an empty string
        if (input$search != ""){
            # search through the data and formats it
            transposed = search_json(input$search)
            
            # if the table isn't empty
            if (nrow(transposed) != 0) {
                # updates the store
                global_store(transposed[1])
                # tells the table what to render and how
                output$search_results = renderDataTable({
                    return(
                        datatable(
                            transposed["Study_title"],
                            caption = "Search Results",
                            options = list(
                                "searching" = FALSE,
                                "lengthChange" = FALSE,
                                "paging" = FALSE
                            ),
                            rownames = FALSE,
                            colnames = NULL,
                            selection = "single",

                            # TODO: See other escape TODO
                            escape = FALSE
                        )
                    )
                })
            }
        }
    })
    
    # runs whenever a row is clicked
    observeEvent(input$search_results_rows_selected, {
        clicked = input$search_results_rows_selected
        index = (title_to_index[[global_store()[[1]][[clicked]]]])

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
                    options = list(
                        "searching" = FALSE,
                        "lengthChange" = FALSE,
                        "paging" = FALSE
                    ),
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
        add_data <<- get_study_additional_data(study_id)

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
                        options = list(
                            "searching" = FALSE,
                            "lengthChange" = FALSE,
                            "paging" = FALSE
                        ),
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
                    options = list(
                        "searching" = FALSE,
                        "lengthChange" = FALSE,
                        "paging" = FALSE
                    ),
                    rownames = FALSE,
                    escape = FALSE # Same issue here as other places
                )
            )
        })

    })
}

# Run the application 
shinyApp(ui = ui, server = server)
