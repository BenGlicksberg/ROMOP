#' Extract synonyms and descendants for concepts of interest.
#'
#' For given vocabulary and concept, returns the mapped standard concept(s) as well as decendent concept(s). Requires dataOntology to have been created (makeDataOntology funciton).
#'
#' @param vocabulary Comma-separated string of relevant vocabularies for inclusion criteria
#' @param codes Semi-colon separated string of code concepts for inclusion criteria, corresponding to the order for vocabulary. Multiple codes can be used per vocabulary and should be comma-separated.
#'
#' @return Returns a table of concepts contained under (i.e., below in the heirarchy) the query concept.
#' @export
#'
#' @examples
#' conceptsInfo <- exploreConcepts(vocabulary = “ATC, ICD10CM”, codes = “A01A; K50, K51”)
exploreConcepts <- function(vocabulary, codes) {

  if (exists("dataOntology")) { # ensure dataOntology exists

    criteriaMapped <- unpackAndMap(vocabulary,codes)

    if (nrow(criteriaMapped)>0) {
      codesFormatted <- paste0(criteriaMapped$concept_id,collapse=",")
      synonymDataFiltered <- identifySynonyms(codesFormatted)
      synonymCodes <- paste(c(codesFormatted, unique(synonymDataFiltered$concept_id_2)),collapse=",")
      mappingDataInfo <- identifyMappings(synonymCodes)

      return(mappingDataInfo)

    } else {
      message("Error: none of the inclusion criteria were able to map to the ontology. Please check terms and try again.")
    }

  } else { #endif dataOntology exists
    message("Error: dataOntology does not exist. Please first run makeDataOntology.")
  }
}
