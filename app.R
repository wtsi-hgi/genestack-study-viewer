library("rjson")
library(tidyverse)
library(shiny)
library(DT)
library(httr)

# empty list of studies that can be selected from, this is updated from the API call
choices = list()

# makes the API call, needs key to be added to work
r <-GET('https://genestack.sanger.ac.uk/frontend/rs/genestack/studyUser/default-released/studies', add_headers(accept =  'application/json',
                                                                                                               `Genestack-API-Token` = 'REDACTED'))
# takes contents of call and assigns it to variable, call returns a json by defualt which is what we want to use.
json_file <- content(r)

# seperates the json file into its seperate studies by assigning it a number based on its index
for (i in 1:length(json_file["data"][[1]])){
    choices[[paste(json_file["data"][[1]][[i]][["Study Title"]]," (",json_file["data"][[1]][[i]][["genestack:accession"]],")", sep = "")]] <- i
}

# sets the default number of rows to 50 so that all of the data will be shown 
options(DT.options = list(pageLength = 50))

#formats the json file so it can be placed into the table and look correct
format_json <- function(study_number) {
    # gets the correct study based on the proivided index of the desired study.
    json_file <- (json_file["data"][[1]][[as.integer(study_number)]])
    json_data_frame = map(json_file, ~ str_c(., collapse = "<br>")) %>% as_tibble
    transposed = as_tibble(cbind(nms = names(json_data_frame), t(json_data_frame)))
    return(transposed)
}


ui <- fluidPage(
    selectInput("select", label = h3("Projects"), 
                choices,  
                selected = 1),
    
    DT::dataTableOutput("mytable") 
    
)


server <- function(input, output) {
    #updates whenever the select box is changed and passes the index of the data that needs to be formatted 
    observeEvent(input$select, {
        # formats the data
        transposed = format_json(input$select)
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
