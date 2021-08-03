library("rjson")
library(tidyverse)
library(shiny)
library(shinyWidgets)
library(DT)
library(httr)
library(curl)

source("helpers.R")

httr::set_config(httr::config(http_version = 1))

# empty list of studies that can be selected from, this is updated from the API call
choices = list("Please select..." = 0)
# a hash which contains the titles linked to their index
title_to_index <- list()

# makes the API call, needs key to be added to work
r <- GET(
    'https://genestack.sanger.ac.uk/frontend/rs/genestack/studyUser/default-released/studies',
    add_headers(
        accept = 'application/json',
        `Genestack-API-Token` = Sys.getenv("API_KEY")
    )
)

# takes contents of call and assigns it to variable, call returns a json by default which is what we want to use.
json_file <- content(r)


if (length(json_file) == 1){
    print(json_file)
    quit(status=1)
}

# separates the json file into its separate studies by assigning it a number based on its index
for (i in 1:length(json_file["data"][[1]])){
    choices[[format_title(json_file["data"][[1]][[i]])]] <- i
    # fills in the index hash
    title_to_index[[json_file["data"][[1]][[i]][["Study Title"]]]] <- i
}

# sets the default number of rows to 50 so that all of the data will be shown 
options(DT.options = list(pageLength = 50))

#formats the json file so it can be placed into the table and look correct
format_json <- function(study_number) {
    # gets the correct study based on the provided index of the desired study.
    json_file <- (json_file["data"][[1]][[as.integer(study_number)]])
    json_data_frame = map(json_file, ~ str_c(., collapse = "<br>")) %>% as_tibble
    transposed = as_tibble(cbind(nms = names(json_data_frame), t(json_data_frame)))
    return(transposed)
}

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
                choices,
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

            dataTableOutput("search_results")
        ),

        mainPanel(
            # Data Tables
            dataTableOutput("study_meta")
        )

    )
)


server <- function(input, output, session) {
    # stores a copy of the table so it can be accessed later
    global_store <- reactiveVal(NULL)
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
                            options = list(
                                "searching" = FALSE,
                                "lengthChange" = FALSE,
                                "paging" = FALSE
                            ),
                            rownames = FALSE,
                            colnames = c("Study Title"),
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
        transposed = format_json(input$select)
        colnames(transposed) <- c("key","value")
        output$study_meta = renderDataTable({
            return(
                datatable(
                    transposed,
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
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
