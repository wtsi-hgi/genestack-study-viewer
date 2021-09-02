library("rjson")

source("viewer/helpers.R")

while (TRUE) {
    fileConn <- file("data/studies.json")
    api_data <- genestack_api_call("studyUser", "studies")
    write(toJSON(api_data), fileConn)
    close(fileConn)

    for (i in 1:length(api_data[["data"]])) {
        fileConn <- file(paste("data/", api_data[["data"]][[i]][["genestack:accession"]], ".json", sep=""))
        write(toJSON(get_study_additional_data(api_data[["data"]][[i]][["genestack:accession"]])), fileConn)
        close(fileConn)
    }

    Sys.sleep(3600)

}