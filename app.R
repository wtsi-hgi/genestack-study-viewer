library("rjson")
library(tidyverse)
library(shiny)

# Give the input file name to the function.
options(DT.options = list(pageLength = 50))
format_json <- function(file_name) {
    json_file <- fromJSON(file = file_name)
    json_data_frame = map(json_file, ~ str_c(., collapse = "<br>")) %>% as_tibble
    transposed = as_tibble(cbind(nms = names(json_data_frame), t(json_data_frame)))
    return(transposed)
}


ui <- fluidPage(
    selectInput("select", label = h3("Projects"), 
                choices = list("Project 1" = "test1.json", "Project 2" = "test2.json", "Project 3" = "test3.json"), 
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
