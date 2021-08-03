format_title <- function(study) {
    return(paste(study["Study Title"], "-", study["genestack:accession"]))
}