
#' Creates general data ontology
#'
#' Creates general data ontology used by all data tables from the concept table. Option to save/load as .rds file.
#' @param declare TRUE/FALSE will output status and data information during the process
#' @param store_ontology TRUE/FALSE If TRUE: will attempt to load .rds file from the current outDirectory; will create and save it if it does not exist. If FALSE: will build table.
#'
#' @return Returns a ontology table dictionary of concepts contained in the 'concept' table.
#' @import data.table DBI
#' @export
#'
#' @examples
#' \dontrun{
#' dataOntology <- makeDataOntology(declare=FALSE,store_ontology=TRUE)
#' }
makeDataOntology <- function(declare=FALSE, store_ontology=FALSE) {
  if (declare==TRUE) {message("Retrieving concept data...")}
  create <- TRUE
  found <- FALSE

  if (store_ontology==TRUE) {
    if (file.exists(paste0(getOption("outDirectory"),"dataOntology.rds")) ) {
      message("Data Ontology found; loading... ")
      dataOntology = readRDS(paste0(getOption("outDirectory"),"dataOntology.rds"))
      message("Data Ontology loaded from memory successfully. ")
      create <- FALSE
      found <- TRUE
    }else{
      message("Data Ontology file not found in declared out_directory; creating... ")
      found <- FALSE
    }

  }

  if (create == TRUE) {
    conceptQuery <- "SELECT concept_id, concept_name, domain_id, vocabulary_id, concept_class_id, concept_code FROM concept WHERE invalid_reason = '';"
    dataOntology <- sqlQuery(conceptQuery)
    dataOntology <- data.table(dataOntology)
  }

  if (declare==TRUE) {
    message("Concept data loaded; data found for: ")
    message(paste0(length(unique(dataOntology$domain_id)), " unique domains."))
    message(paste0(length(unique(dataOntology$vocabulary_id)), " unique vocabularies."))
    message(paste0(length(unique(dataOntology$concept_class_id)), " unique concept classes."))
  }

  if (store_ontology == TRUE & found == FALSE) { # save data ontology
    message(paste0("Storing Data Ontology: ", getOption("outDirectory"),"dataOntology.rds"))
    saveRDS(dataOntology, paste0(getOption("outDirectory"),"dataOntology.rds"))

  }

  return(dataOntology)

}

