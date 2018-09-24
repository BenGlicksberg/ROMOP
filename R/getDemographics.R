#' Retrieves patient demographic data
#'
#' Compiles demographic data for all patients or a given patient list if provided. Concepts are mapped through the created data ontology. Demographic data are retrieved from 'person' and 'death' tables and include: birthdate, deathdate, gender, ethnicity, and race.
#'
#' @param patient_list NULL or comma-separated string of patient ids. A provdied patient_list will restrict search to ids. NULL will return demographic data for all available patients.
#' @param declare TRUE/FALSE will output status and data information during the process
#'
#' @return table of mapped demographic concepts for all patients or specific ones for a provided csv formmated string of ids
#' @import data.table DBI
#' @export
#'
#' @examples
#' ptDemo <- getDemographics(patient_list=NULL,declare=TRUE)
getDemographics <-function(patient_list=NULL, declare=FALSE) { # patient list will restrict search

  if (exists("dataOntology")) { # ensure dataOntology exists

    if (length(patient_list) > 1){
      patient_list <- paste(patient_list, collapse = ",")
    }

    queryStatement <- "SELECT person_id, birth_datetime, year_of_birth, gender_concept_id, ethnicity_concept_id, race_concept_id FROM person" # year_of_birth added in case birth_datetime IS NULL
    deathqueryStatement <-"SELECT person_id, death_date FROM death"

    if (!is.null(patient_list)) { # if patient_list not null, append with WHERE statement
      queryStatement <- paste0(queryStatement,paste0(' WHERE person_id IN (', patient_list,') '))
      deathqueryStatement <- paste0(deathqueryStatement,paste0(' WHERE person_id IN (', patient_list,') '))
    }

    # first get main patient data
    ptDemo <- sqlQuery(queryStatement)

    if (nrow(ptDemo)==0) { # check if any pts found
      if (declare==TRUE) {message("No patients found for current input")}
    } else {
      if (declare==TRUE) {message("Data loaded; formatting...")}

      ptDemo <- data.table(ptDemo) # convert to data.table
      current_year <- as.numeric(format(Sys.Date(),"%Y")) # get current year to calculate age
      ptDemo$age <- current_year - ptDemo$year_of_birth # calculate age

      # map concepts to reference table
      ptDemo <- merge(ptDemo, dataOntology[domain_id=="Gender",c("concept_id","concept_name")], by.x ="gender_concept_id", by.y = "concept_id" ,all.x=T) # Gender
      names(ptDemo)[names(ptDemo) == 'concept_name'] <- 'Gender' # rename column
      ptDemo=markNAasUnknown(ptDemo,"Gender",declare)

      ptDemo <- merge(ptDemo, dataOntology[domain_id=="Race",c("concept_id","concept_name")], by.x ="race_concept_id", by.y = "concept_id" ,all.x=T) # Race
      names(ptDemo)[names(ptDemo) == 'concept_name'] <- 'Race' # rename column
      ptDemo=markNAasUnknown(ptDemo,"Race",declare)

      ptDemo <- merge(ptDemo, dataOntology[domain_id=="Ethnicity",c("concept_id","concept_name")], by.x ="ethnicity_concept_id", by.y = "concept_id" ,all.x=T) # Ethnicity
      names(ptDemo)[names(ptDemo) == 'concept_name'] <- 'Ethnicity' # rename column
      ptDemo <- markNAasUnknown(ptDemo,"Ethnicity",declare)

      ### clean up extra columns
      ptDemo <- ptDemo[,-c("ethnicity_concept_id","race_concept_id","gender_concept_id")]

      # add in death date
      ptDeath <- sqlQuery(deathqueryStatement)
      ptDeath <- data.table(ptDeath) # convert to data.table

      # merge with patient data
      ptDemo <- merge(ptDemo, ptDeath,by="person_id",all.x=T)
      # mark Alive/Deceased
      ptDemo$Status <- ifelse(is.na(ptDemo$death_date),"Alive","Deceased")

      return(ptDemo)

    }

  } else { #endif dataOntology exists
    message("Error: dataOntology does not exist. Please first run makeDataOntology.")
  }

}
