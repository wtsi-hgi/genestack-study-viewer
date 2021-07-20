library("rjson")
library(tidyverse)
library(shiny)
library(shinyWidgets)
library(DT)
library(httr)

# empty list of studies that can be selected from, this is updated from the API call
choices = list()
# a hash which contains the titles linked to their index
title_to_index <- list()

# makes the API call, needs key to be added to work
r <-GET('https://genestack.sanger.ac.uk/frontend/rs/genestack/studyUser/default-released/studies', add_headers(accept =  'application/json',
                                                                                                               `Genestack-API-Token` = 'REDACTED'))
# takes contents of call and assigns it to variable, call returns a json by default which is what we want to use.
json_file <- content(r)

# separates the json file into its separate studies by assigning it a number based on its index
for (i in 1:length(json_file["data"][[1]])){
    choices[[paste(json_file["data"][[1]][[i]][["Study Title"]]," (",json_file["data"][[1]][[i]][["genestack:accession"]],")", sep = "")]] <- i
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
                    # if the word we are searching for is in the data
                    if (grepl( toupper(searched_word), toupper(data), fixed = TRUE)){
                        # if the data is not a duplicate 
                        if (!(data %in% data_temp)){
                            # add that line of data to the data list
                            data_temp = c(data,data_temp)
                        }
                    }
                }
                # if the data does have another array inside it
                else{
                    # loop through that array 
                    for (k in 1:length(data)){
                        # if the word we are searching for is in the data
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
    # makes everything inside next to each other as a row
    flowLayout(
        # input box
        selectInput("select", label = ("Projects"), 
                    choices,  
                    selected = 1),
        # search box
        searchInput(
            inputId = "search", label = "Seach for metadata",
            placeholder = NULL,
            btnSearch = icon("search"),
            btnReset = icon("remove"),
            width = "450px"
        )
    ),
    # the two tables
    DT::dataTableOutput("mymetatable"), 
    DT::dataTableOutput("mytable")
)


server <- function(input, output) {
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
                output$mymetatable = DT::renderDataTable(
                    selection = list(mode = "single", target = "cell"),
                    {
                    sketch<-htmltools::withTags(table(
                        tableHeader(transposed,escape=F
                        )))
                    
                    thing = DT::datatable(
                        transposed
                        ,rownames = FALSE
                        ,container = sketch,escape=F
                    )
                    return(thing)
                })
            }
        }
    })
    # runs whenever a row is clicked
    observeEvent(input$mymetatable_row_last_clicked, {
        # stores the title of the last row clicked
        clicked = input$mymetatable_row_last_clicked
        # converts the title to the correct index using the hash
        index = (title_to_index[[global_store()[[1]][[clicked]]]])
        # updates the selection box to show the new choice
        updateSelectInput(
            session = getDefaultReactiveDomain(),
            "select",
            label = ("Projects"),
            choices = choices,
            selected = index
        )
    })
    
    
    #updates whenever the select box is changed and passes the index of the data that needs to be formatted 
    observeEvent(input$select, {
        # formats the data
        transposed = format_json(input$select)
        colnames(transposed) <- c("key","value")
        # tells the table what to render and how
        output$mytable = DT::renderDataTable({
            sketch<-htmltools::withTags(table(
                tableHeader(transposed,escape=F
                )))
            thing = DT::datatable(
                transposed
                ,rownames = FALSE
                ,container = sketch,escape=F
            )
            return(thing)
        })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
