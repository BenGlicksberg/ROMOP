#' Retrieves patient clinical encounter data
#'
#' Compiles encounter data for a given patient list. Concepts are mapped through the created data ontology. Encounter data are retrieved from visit_occurence table and include: visit_type, encounter_type, etc.
#' @param patient_list Comma-separated string of patient ids
#' @param declare TRUE/FALSE will output status and data information during the process
#'
#' @return table of mapped encounter concepts for specific patients contained in a provided csv formmated string of ids.
#' @import data.table DBI odbc
#' @export
#'
#' @examples
#' ptEncs <- getEncounters("1,2,3,4",declare=TRUE)
getEncounters <- function(patient_list, declare=FALSE) {

  if (exists("dataOntology")) { # ensure dataOntology exists

    queryStatement <- paste0('SELECT person_id, visit_occurrence_id, visit_concept_id, visit_start_datetime, visit_end_datetime, visit_source_concept_id, visit_source_value, admitting_source_concept_id, discharge_to_concept_id FROM visit_occurrence WHERE person_id IN (', patient_list,') ')

    if (declare==TRUE) {message("Loading encounters data...")}

    # get visit data
    ptEncs <- sqlQuery(queryStatement)

    if (nrow(ptEncs)==0) {
      message("No encounter data found for current patient list")
    } else {
      if (declare==TRUE) {message("Encounters data loaded; formatting...")}

      ptEncs <- data.table(ptEncs) # convert to data.table

      # merge in relevant information concept ids
      ptEncs <- merge(ptEncs,dataOntology[,c("concept_id","concept_name")], by.x="visit_concept_id", by.y="concept_id", all.x=TRUE)
      names(ptEncs)[names(ptEncs) == 'concept_name'] <- 'visit_concept' # rename column
      ptEncs <- ptEncs[,-"visit_concept_id"]
      ptEncs <- merge(ptEncs,dataOntology[,c("concept_id","concept_name")], by.x="visit_source_concept_id", by.y="concept_id", all.x=TRUE)
      names(ptEncs)[names(ptEncs) == 'concept_name'] <- 'visit_source_concept' # rename column
      ptEncs <- ptEncs[,-"visit_source_concept_id"]
      ptEncs <- merge(ptEncs,dataOntology[,c("concept_id","concept_name")], by.x="admitting_source_concept_id", by.y="concept_id", all.x=TRUE)
      names(ptEncs)[names(ptEncs) == 'concept_name'] <- 'admitting_concept' # rename column
      ptEncs <- ptEncs[,-"admitting_source_concept_id"]
      ptEncs <- merge(ptEncs,dataOntology[,c("concept_id","concept_name")], by.x="discharge_to_concept_id", by.y="concept_id", all.x=TRUE)
      names(ptEncs)[names(ptEncs) == 'concept_name'] <- 'discharge_concept' # rename column
      ptEncs <- ptEncs[,-"discharge_to_concept_id"]

      return(ptEncs)

    }

  } else { #endif dataOntology exists
    message("Error: dataOntology does not exist. Please first run makeDataOntology.")
  }


}
