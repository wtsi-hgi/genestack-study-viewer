# syntax=docker/dockerfile:1
FROM rocker/shiny-verse:latest

 RUN R -e "install.packages(pkgs=c('rjson','shiny','shinyWidgets','DT','httr',dependencies = TRUE))"   

 COPY . /app/shiny_save

 EXPOSE 3838

 CMD ["R","-e", "shiny::runApp('/app/shiny_save', host='0.0.0.0', port=3838)"]


