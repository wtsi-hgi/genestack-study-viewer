library("httr")
library("curl")
library("tidyverse")
httr::set_config(httr::config(http_version = 1))

format_title <- function(study) {
    return(paste(study["Study Title"], "-", study["genestack:accession"]))
}

accessions_to_titles <- function(accessions, study_data) {
    titles <- c()
    for (accession in accessions) {
        for (study in study_data[["data"]]) {
            if (study[["genestack:accession"]] == accession) {
                titles <- append(titles, study[["Study Title"]])
                break
            }
        }
    }
    return(data.frame(titles))
}

# Change list of key:value pairs (value can be any datatype) to dataframe
format_json <- function(json_data) {
    json_data_frame = map(json_data, ~ str_c(., collapse = "<br>")) %>% as_tibble
    transposed = as_tibble(cbind(nms = names(json_data_frame), t(json_data_frame)))
    
    # Remove Blank Values
    transposed <- transposed %>% filter(V2 != "")
    return(transposed)
}

# Find a Study in the json_file using a Study Number
find_study <- function(study_number, json_file) {
    temp_json <- (json_file["data"][[1]][[as.integer(study_number)]])
    return(format_json(temp_json))
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

    if (length(content(req)) != 0) {
        return(content(req))
    }
    return(NULL)
}

get_study_additional_data <- function(study_id) {
    endpoints = c("expression", "variant")
    rtn_data = list()
    for (endpoint in endpoints) {
        rtn_data[[endpoint]] <- genestack_api_call(
            "integrationUser",
            paste(
                "integration/link/",
                endpoint,
                "/group/by/study/",
                study_id,
                sep=""
            )
        )
    }
    return(rtn_data)
}

search_studies <- function(query, study_additional_data) {

    studies <- c()

    # Firstly, straight up search Studies
    results <- genestack_api_call(
        "studyUser",
        paste(
            "studies?searchSpecificTerms=true&query=",
            query,
            sep = ""
        )
    )[["data"]]

    for (study in results) {
        studies <- append(studies, study[["genestack:accession"]])
    }

    # Next, search additional datasets

    # Places to search - user:endpoint pairs 
    endpoints <- list(
        c("variantUser", "variant"),
        c("expressionUser", "expression")
    )

    for (endpoint in endpoints) {
        results <- genestack_api_call(
            endpoint[1],
            paste(
                endpoint[2],
                "?searchSpecificTerms=true&query=",
                query,
                sep = ""
            )
        )[["data"]]

        # We now need to find what study/studies this is associated to
        for (item in results) { # item is each result from the search
            itemId <- item[["itemId"]]
            for (study in names(study_additional_data)) { # study should be genestack:accession
                for (type in names(study_additional_data[[study]])) { # type should be variant | expression
                    if (type == endpoint[2]) {
                        for (data in study_additional_data[[study]][[type]]) { # data will be each extra bit associated to a study
                            if (data[["itemId"]] == itemId) {
                                studies = append(studies, study)
                            }
                        }
                    }
                }
            }
        }
    }

    return(studies)
}