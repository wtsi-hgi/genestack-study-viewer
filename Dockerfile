FROM rocker/shiny-verse:latest

RUN R -e "install.packages(pkgs=c('rjson','shiny','shinyWidgets','DT','httr',dependencies = TRUE))"   
 
RUN mkdir -p /app/src

COPY . /app/shiny_save

WORKDIR /app/shiny_save

EXPOSE 3838

CMD ["./start.sh"]


