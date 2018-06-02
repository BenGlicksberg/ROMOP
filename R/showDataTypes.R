#' Shows available data types from the OMOP ontology
#'
#' Details relevant vocabularies per ontological domain. Requires dataOntology to have been created (makeDataOntology funciton).
#' @return Returns a table of vocabularies contained within clinical domains: Condition, Observation, Measurement, Device, Procedure, Drug.
#' @export
#'
#' @examples
#' showDataTypes()
#'
showDataTypes <- function() {

  if (exists("dataOntology")) { # ensure dataOntology exists

    dataTypes = dataOntology[domain_id %in% c("Condition","Observation","Measurement","Device","Procedure","Drug"),c("domain_id", "vocabulary_id")]
    dataTypes = dataTypes[!duplicated(dataTypes)]
    dataTypes = dataTypes[order(domain_id),]

    return(dataTypes)

  } else { #endif dataOntology exists
    message("Error: dataOntology does not exist. Please first run makeDataOntology.")
  }

}
