library("httr")
library("curl")
httr::set_config(httr::config(http_version = 1))

format_title <- function(study) {
    return(paste(study["Study Title"], "-", study["genestack:accession"]))
}

genestack_api_call <- function(user, endpoint) {
    req <- GET(
        paste(
            "https://genestack.sanger.ac.uk/frontend/rs/genestack/",
            user,
            "/default-released/",
            endpoint,
            sep = ""
        ),
        add_headers(
            accept = "application/json",
            `Genestack-API-Token` = Sys.getenv("API_KEY")
        )
    )
    return(content(req))
}

get_study_additional_data <- function(study_id) {
    endpoints = c("expression", "variant")
    for (endpoint in endpoints) {
        print(endpoint)
        print(
            genestack_api_call(
                "integrationUser",
                paste(
                    "integration/link/",
                    endpoint,
                    "/group/by/study/",
                    study_id,
                    sep=""
                )
            )
        )
    }
}