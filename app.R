library("rjson")
library(tidyverse)
library(shiny)

choices = list()
json_file <- fromJSON(file = "test4.json")
for (i in 1:length(json_file["data"][[1]])){
    choices[[paste(json_file["data"][[1]][[i]][["Study Title"]]," (",json_file["data"][[1]][[i]][["genestack:accession"]],")", sep = "")]] <- i
}

# Give the input file name to the function.
options(DT.options = list(pageLength = 50))
format_json <- function(study_number) {
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
    
    observeEvent(input$select, {
        transposed = format_json(input$select)
    
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
