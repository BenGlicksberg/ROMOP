#' Retrieves all patient clinical data
#'
#' Wrapper for domain-specific getData functions (e.g., getObservations). Produces a list of tables for all relevant domains.
#'
#' @param patient_list Comma-separated string of patient ids
#' @param declare TRUE/FALSE will output status and data information during the process
#'
#' @return a list of tables for each all data within each domain (e.g., Condition) for all patients provided (can access by ptClinicalData$Condition).
#' @import data.table DBI odbc
#' @export
#'
#' @examples
#' ptClinicalData <- getClinicalData("1,2", declare=TRUE)
getClinicalData<- function(patient_list, declare=FALSE) {

  if (exists("dataOntology")) { # ensure dataOntology exists

    if (length(patient_list) > 1){
      patient_list <- paste(patient_list, collapse = ",")
    }

    ### retrieves data from each data function below
    ptObsData <- getObservations(patient_list,declare=declare)
    ptCondData <- getConditions(patient_list,declare=declare)
    ptProcData <- getProcedures(patient_list,declare=declare)
    ptsMedsData <- getMedications(patient_list,declare=declare)
    ptMeasData <- getMeasurements(patient_list,declare=declare)
    ptDeviceData <- getDevices(patient_list,declare=declare)

    ptClinicalData <- list(ptObsData,ptCondData,ptProcData,ptsMedsData,ptMeasData,ptDeviceData)
    names(ptClinicalData) <- c("Observation", "Condition", "Procedures", "Medications","Measurements","Devices")

    return(ptClinicalData)

  } else { #endif dataOntology exists
    message("Error: dataOntology does not exist. Please first run makeDataOntology.")
  }

}



#################################  modality specific functions

#' Retrieves all patient clinical data from Observations table
#'
#' Produces a table for relevant concepts contained in the 'observation' table mapped through the data ontology for a patient list. Data retrieved include: observation_type, value, etc.
#'
#' @param patient_list Comma-separated string of patient ids
#' @param declare TRUE/FALSE will output status and data information during the process
#'
#' @return a table of relevant clinical data contained with in the 'observation' table
#' @import data.table DBI odbc
#' @export
#'
#' @examples
#' ptObsData <- getObservations("1,2", declare=TRUE)
getObservations <- function(patient_list, declare=FALSE) {

  if (exists("dataOntology")) { # ensure dataOntology exists

    if (length(patient_list) > 1){
      patient_list <- paste(patient_list, collapse = ",")
    }

    ## observation
    # observation_concept_id is SNOMED
    queryStatement <- paste0('SELECT person_id, observation_concept_id, observation_source_concept_id, observation_datetime, observation_type_concept_id, value_as_number, value_as_string, value_as_concept_id, visit_occurrence_id, observation_source_value, unit_source_value FROM observation WHERE person_id IN (', patient_list,') ')

    if (declare==TRUE) {message("Loading Observations data......")}

    ptObsData <- sqlQuery(queryStatement)
    ptObsData <- data.table(ptObsData) # convert to data.table

    ### check for any data
    if (nrow(ptObsData)==0) {
      message("No observation data found for patient list")
    } else {
      if (declare==TRUE) {message("Observation data loaded; formatting...")}

      # obtain table specific ontology
      observationTableOntology <- dataOntology[domain_id=="Observation"]

      # format clinical data
      ptObsData <- merge(ptObsData, observationTableOntology[,c("concept_id","vocabulary_id","concept_code","concept_name")], by.x="observation_concept_id",by.y="concept_id",all.x=TRUE)
      names(ptObsData)[names(ptObsData) == 'concept_code'] <- 'observation_concept_code' # rename column
      names(ptObsData)[names(ptObsData) == 'concept_name'] <- 'observation_concept_name' # rename column
      names(ptObsData)[names(ptObsData) == 'vocabulary_id'] <- 'observation_concept_vocabulary' # rename column
      ptObsData <- ptObsData[,-"observation_concept_id"]

      ptObsData <- merge(ptObsData, observationTableOntology[,c("concept_id","vocabulary_id", "concept_code","concept_name")], by.x="observation_source_concept_id",by.y="concept_id",all.x=TRUE)
      names(ptObsData)[names(ptObsData) == 'concept_code'] <- 'observation_source_code' # rename column
      names(ptObsData)[names(ptObsData) == 'concept_name'] <- 'observation_source_name' # rename column
      names(ptObsData)[names(ptObsData) == 'vocabulary_id'] <- 'observation_source_vocabulary' # rename column
      ptObsData <- ptObsData[,-"observation_source_concept_id"]

      # format metadata
      ptObsData <- merge(ptObsData,dataOntology[,c("concept_id","concept_name")],by.x="observation_type_concept_id",by.y="concept_id", all.x=TRUE)
      names(ptObsData)[names(ptObsData) == 'concept_name'] <- 'observation_type' # rename column
      ptObsData <- ptObsData[,-"observation_type_concept_id"]

      ptObsData=merge(ptObsData,dataOntology[,c("concept_id","concept_name")],by.x="value_as_concept_id",by.y="concept_id", all.x=TRUE)
      names(ptObsData)[names(ptObsData) == 'concept_name'] <- 'value_concept' # rename column
      ptObsData <- ptObsData[,-"value_as_concept_id"]

      if (declare==TRUE) {message("Observation data formatted successfully ")}

    }

    return(ptObsData)

  } else { #endif dataOntology exists
    message("Error: dataOntology does not exist. Please first run makeDataOntology.")
  }

}



#' Retrieves all patient clinical data from Condition table
#'
#' Produces a table for relevant concepts contained in the 'condition_occurrence' table mapped through the data ontology for a patient list. Data retrieved include: condition_type, condition_status, etc.
#'
#' @param patient_list Comma-separated string of patient ids
#' @param declare TRUE/FALSE will output status and data information during the process
#'
#' @return a table of relevant clinical data contained with in the 'condition_occurrence' table
#' @import data.table DBI odbc
#' @export
#'
#' @examples
#' ptCondData <- getConditions("1,2", declare=TRUE)
getConditions <- function(patient_list, declare=FALSE) {

  if (exists("dataOntology")) { # ensure dataOntology exists

    if (length(patient_list) > 1){
      patient_list <- paste(patient_list, collapse = ",")
    }

    queryStatement <- paste0('SELECT person_id, condition_concept_id, condition_start_datetime, visit_occurrence_id, condition_type_concept_id, condition_source_value, condition_source_concept_id, condition_status_concept_id FROM condition_occurrence WHERE person_id IN (', patient_list,') ')

    if (declare==TRUE) {message("Loading Condition data...")}


    ptCondData <- sqlQuery(queryStatement)
    ptCondData <- data.table(ptCondData) # convert to data.table


    ### check for any data
    if (nrow(ptCondData)==0) {
      message("No condition data found for patient list")
    } else {

      if (declare==TRUE) {message("Condition data loaded; formatting...")}


      # obtain table specific ontology
      conditionTableOntology <- dataOntology[grep("Condition",domain_id)]

      # format clinical data
      ptCondData <- merge(ptCondData, conditionTableOntology[,c("concept_id","vocabulary_id","concept_code","concept_name")], by.x="condition_concept_id",by.y="concept_id",all.x=TRUE)
      names(ptCondData)[names(ptCondData) == 'concept_code'] <- 'condition_concept_code' # rename column
      names(ptCondData)[names(ptCondData) == 'concept_name'] <- 'condition_concept_name' # rename column
      names(ptCondData)[names(ptCondData) == 'vocabulary_id'] <- 'condition_concept_vocabulary' # rename column
      ptCondData <- ptCondData[,-"condition_concept_id"]

      ptCondData <- merge(ptCondData, conditionTableOntology[,c("concept_id","vocabulary_id", "concept_code","concept_name")], by.x="condition_source_concept_id",by.y="concept_id",all.x=TRUE)
      names(ptCondData)[names(ptCondData) == 'concept_code'] <- 'condition_source_code' # rename column
      names(ptCondData)[names(ptCondData) == 'concept_name'] <- 'condition_source_name' # rename column
      names(ptCondData)[names(ptCondData) == 'vocabulary_id'] <- 'condition_source_vocabulary' # rename column
      ptCondData <- ptCondData[,-"condition_source_concept_id"]

      # format metadatadata
      ptCondData <- merge(ptCondData,dataOntology[,c("concept_id","concept_name")],by.x="condition_type_concept_id",by.y="concept_id", all.x=TRUE)
      names(ptCondData)[names(ptCondData) == 'concept_name'] <- 'condition_type' # rename column
      ptCondData <- ptCondData[,-"condition_type_concept_id"]
      ptCondData <- merge(ptCondData,dataOntology[,c("concept_id","concept_name")],by.x="condition_status_concept_id",by.y="concept_id", all.x=TRUE)
      names(ptCondData)[names(ptCondData) == 'concept_name'] <- 'condition_status_type' # rename column
      ptCondData <- ptCondData[,-"condition_status_concept_id"]

      if (declare==TRUE) {message("Condition data formatted successfully. ")}

    }

    return(ptCondData)

  } else { #endif dataOntology exists
    message("Error: dataOntology does not exist. Please first run makeDataOntology.")
  }

}



#' Retrieves all patient clinical data from Procedures table
#'
#' Produces a table for relevant concepts contained in the 'procedure_occurrence' table mapped through the data ontology for a patient list. Data retrieved include: procedure_type, etc.
#'
#' @param patient_list Comma-separated string of patient ids
#' @param declare TRUE/FALSE will output status and data information during the process
#'
#' @return a table of relevant clinical data contained with in the 'procedure_occurrence' table
#' @import data.table DBI odbc
#' @export
#'
#' @examples
#' ptProcData <- getProcedures("1,2", declare=TRUE)
getProcedures <- function(patient_list, declare=FALSE){

  if (exists("dataOntology")) { # ensure dataOntology exists

    if (length(patient_list) > 1){
      patient_list <- paste(patient_list, collapse = ",")
    }

    queryStatement <- paste0('SELECT person_id, procedure_concept_id, procedure_datetime, quantity, visit_occurrence_id, procedure_type_concept_id, procedure_source_value, procedure_source_concept_id  FROM procedure_occurrence WHERE person_id IN (', patient_list,') ')

    if (declare==TRUE) {message("Loading Procedures data...")}

    ptProcData <- sqlQuery(queryStatement)
    ptProcData <- data.table(ptProcData) # convert to data.table

    ### check for any data
    if (nrow(ptProcData)==0) {
      message("No procedure data found for patient list")
    } else {
      if (declare==TRUE) {message("Procedure data loaded; formatting...")}

      # obtain table specific ontology
      procedureTableOntology <- dataOntology[domain_id=="Procedure"]

      # format clinical data
      ptProcData <- merge(ptProcData, procedureTableOntology[,c("concept_id","vocabulary_id","concept_code","concept_name")], by.x="procedure_concept_id",by.y="concept_id",all.x=TRUE)
      names(ptProcData)[names(ptProcData) == 'concept_code'] <- 'procedure_concept_code' # rename column
      names(ptProcData)[names(ptProcData) == 'concept_name'] <- 'procedure_concept_name' # rename column
      names(ptProcData)[names(ptProcData) == 'vocabulary_id'] <- 'procedure_concept_vocabulary' # rename column
      ptProcData <- ptProcData[,-"procedure_concept_id"]

      ptProcData <- merge(ptProcData, procedureTableOntology[,c("concept_id","vocabulary_id", "concept_code","concept_name")], by.x="procedure_source_concept_id",by.y="concept_id",all.x=TRUE)
      names(ptProcData)[names(ptProcData) == 'concept_code'] <- 'procedure_source_code' # rename column
      names(ptProcData)[names(ptProcData) == 'concept_name'] <- 'procedure_source_name' # rename column
      names(ptProcData)[names(ptProcData) == 'vocabulary_id'] <- 'procedure_source_vocabulary' # rename column
      ptProcData <- ptProcData[,-"procedure_source_concept_id"]

      # format metadata
      ptProcData <- merge(ptProcData,dataOntology[,c("concept_id","concept_name")],by.x="procedure_type_concept_id",by.y="concept_id", all.x=TRUE)
      names(ptProcData)[names(ptProcData) == 'concept_name'] <- 'procedure_type' # rename column
      ptProcData <- ptProcData[,-"procedure_type_concept_id"]

      if (declare==TRUE) {message("Procedure data formatted successfully.")}

    }

    return(ptProcData)

  } else { #endif dataOntology exists
    message("Error: dataOntology does not exist. Please first run makeDataOntology.")
  }
}



#' Retrieves all patient clinical data from Medications table
#'
#' Produces a table for relevant concepts contained in the 'drug_exposure' table mapped through the data ontology for a patient list. Data retrieved include: drug_type, route, etc.
#'
#' @param patient_list Comma-separated string of patient ids
#' @param declare TRUE/FALSE will output status and data information during the process
#'
#' @return a table of relevant clinical data contained with in the 'drug_exposure' table
#' @import data.table DBI odbc
#' @export
#'
#' @examples
#' ptsMedsData <- getMedications("1,2", declare=TRUE)
getMedications <- function(patient_list, declare=FALSE) {

  if (exists("dataOntology")) { # ensure dataOntology exists

    if (length(patient_list) > 1){
      patient_list <- paste(patient_list, collapse = ",")
    }

    queryStatement <- paste0('SELECT person_id, drug_concept_id, drug_exposure_start_datetime, drug_exposure_end_datetime, drug_type_concept_id, stop_reason, refills, quantity, days_supply, sig, route_concept_id, dose_unit_source_value, visit_occurrence_id, drug_source_value, drug_source_concept_id, route_source_value FROM drug_exposure WHERE person_id IN (', patient_list,') ')

    if (declare==TRUE) {message("Loading Medications data...")}


    ptsMedsData <- sqlQuery(queryStatement)
    ptsMedsData <- data.table(ptsMedsData) # convert to data.table


    ### check for any data
    if (nrow(ptsMedsData)==0) {
      message("No medication data found for patient list")
    } else {
      if (declare==TRUE) {message("Medication data loaded; formatting...")}

      # obtain table specific ontology
      medicationTableOntology <- dataOntology[domain_id=="Drug"]

      # format clinical data
      ptsMedsData <- merge(ptsMedsData, medicationTableOntology[,c("concept_id","vocabulary_id","concept_code","concept_name")], by.x="drug_concept_id",by.y="concept_id",all.x=TRUE)
      names(ptsMedsData)[names(ptsMedsData) == 'concept_code'] <- 'medication_concept_code' # rename column
      names(ptsMedsData)[names(ptsMedsData) == 'concept_name'] <- 'medication_concept_name' # rename column
      names(ptsMedsData)[names(ptsMedsData) == 'vocabulary_id'] <- 'medication_concept_vocabulary' # rename column
      ptsMedsData <- ptsMedsData[,-"drug_concept_id"]

      ptsMedsData <- merge(ptsMedsData, medicationTableOntology[,c("concept_id","vocabulary_id", "concept_code","concept_name")], by.x="drug_source_concept_id",by.y="concept_id",all.x=TRUE)
      names(ptsMedsData)[names(ptsMedsData) == 'concept_code'] <- 'medication_source_code' # rename column
      names(ptsMedsData)[names(ptsMedsData) == 'concept_name'] <- 'medication_source_name' # rename column
      names(ptsMedsData)[names(ptsMedsData) == 'vocabulary_id'] <- 'medication_source_vocabulary' # rename column
      ptsMedsData <- ptsMedsData[,-"drug_source_concept_id"]

      # format metadata
      ptsMedsData <- merge(ptsMedsData,dataOntology[,c("concept_id","concept_name")],by.x="drug_type_concept_id",by.y="concept_id", all.x=TRUE)
      names(ptsMedsData)[names(ptsMedsData) == 'concept_name'] <- 'drug_type' # rename column
      ptsMedsData <- ptsMedsData[,-"drug_type_concept_id"]
      ptsMedsData <- merge(ptsMedsData,dataOntology[,c("concept_id","concept_name")],by.x="route_concept_id",by.y="concept_id", all.x=TRUE)
      names(ptsMedsData)[names(ptsMedsData) == 'concept_name'] <- 'route_concept' # rename column
      ptsMedsData <- ptsMedsData[,-"route_concept_id"]

      if (declare==TRUE) {message("Medication data formatted successfully.")}

    }

    return(ptsMedsData)

  } else { #endif dataOntology exists
    message("Error: dataOntology does not exist. Please first run makeDataOntology.")
  }
}


#' Retrieves all patient clinical data from Measurement table
#'
#' Produces a table for relevant concepts contained in the 'measurement' table mapped through the data ontology for a patient list. Data retrieved include: measurement_type, value, unit, etc.
#'
#' @param patient_list Comma-separated string of patient ids
#' @param declare TRUE/FALSE will output status and data information during the process
#'
#' @return a table of relevant clinical data contained with in the 'measurement' table
#' @import data.table DBI odbc
#' @export
#'
#' @examples
#' ptMeasData <- getMeasurements("1,2", declare=TRUE)
getMeasurements <- function(patient_list, declare=FALSE) {

  if (exists("dataOntology")) { # ensure dataOntology exists

    if (length(patient_list) > 1){
      patient_list <- paste(patient_list, collapse = ",")
    }

    queryStatement <- paste0('SELECT person_id, measurement_concept_id, measurement_datetime, measurement_type_concept_id, value_as_number, value_as_concept_id, unit_concept_id, visit_occurrence_id, measurement_source_value, measurement_source_concept_id FROM measurement WHERE person_id IN (', patient_list,') ');

    if (declare==TRUE) {message("Loading Measurements data...")}

    ptMeasData <- sqlQuery(queryStatement)
    ptMeasData <- data.table(ptMeasData) # convert to data.table


    ### check for any data
    if (nrow(ptMeasData)==0) {
      message("No measurement data found for patient list")
    } else {
      if (declare==TRUE) {message("Measurement data loaded; formatting...")}

      # obtain table specific ontology
      measurementTableOntology <- dataOntology[domain_id=="Measurement"]


      # format clinical data
      ptMeasData <- merge(ptMeasData, measurementTableOntology[,c("concept_id","vocabulary_id","concept_code","concept_name")], by.x="measurement_concept_id",by.y="concept_id",all.x=TRUE)
      names(ptMeasData)[names(ptMeasData) == 'concept_code'] <- 'measurement_concept_code' # rename column
      names(ptMeasData)[names(ptMeasData) == 'concept_name'] <- 'measurement_concept_name' # rename column
      names(ptMeasData)[names(ptMeasData) == 'vocabulary_id'] <- 'measurement_concept_vocabulary' # rename column
      ptMeasData <- ptMeasData[,-"measurement_concept_id"]

      ptMeasData <- merge(ptMeasData, measurementTableOntology[,c("concept_id","vocabulary_id", "concept_code","concept_name")], by.x="measurement_source_concept_id",by.y="concept_id",all.x=TRUE)
      names(ptMeasData)[names(ptMeasData) == 'concept_code'] <- 'measurement_source_code' # rename column
      names(ptMeasData)[names(ptMeasData) == 'concept_name'] <- 'measurement_source_name' # rename column
      names(ptMeasData)[names(ptMeasData) == 'vocabulary_id'] <- 'measurement_source_vocabulary' # rename column
      ptMeasData <- ptMeasData[,-"measurement_source_concept_id"]

      # format metadata
      ptMeasData <- merge(ptMeasData,dataOntology[,c("concept_id","concept_name")],by.x="measurement_type_concept_id",by.y="concept_id", all.x=TRUE)
      names(ptMeasData)[names(ptMeasData) == 'concept_name'] <- 'measurement_type' # rename column
      ptMeasData <- ptMeasData[,-"measurement_type_concept_id"]
      ptMeasData <- merge(ptMeasData,dataOntology[,c("concept_id","concept_name")],by.x="value_as_concept_id",by.y="concept_id", all.x=TRUE)
      names(ptMeasData)[names(ptMeasData) == 'concept_name'] <- 'value_concept' # rename column
      ptMeasData <- ptMeasData[,-"value_as_concept_id"]
      ptMeasData <- merge(ptMeasData,dataOntology[,c("concept_id","concept_name")],by.x="unit_concept_id",by.y="concept_id", all.x=TRUE)
      names(ptMeasData)[names(ptMeasData) == 'concept_name'] <- 'unit_concept' # rename column
      ptMeasData <- ptMeasData[,-"unit_concept_id"]

      if (declare==TRUE) {message("Measurement data formatted successfully.")}

    }

    return(ptMeasData)

  } else { #endif dataOntology exists
    message("Error: dataOntology does not exist. Please first run makeDataOntology.")
  }

}


#' Retrieves all patient clinical data from Device table
#'
#' Produces a table for relevant concepts contained in the 'device_exposure' table mapped through the data ontology for a patient list. Data retrieved include: device_type, etc.
#'
#' @param patient_list Comma-separated string of patient ids
#' @param declare TRUE/FALSE will output status and data information during the process
#'
#' @return a table of relevant clinical data contained with in the 'device_exposure' table
#' @import data.table DBI odbc
#' @export
#'
#' @examples
#' ptDeviceData <- getDevices("1,2", declare=TRUE)
getDevices <- function(patient_list, declare=FALSE) {

  if (exists("dataOntology")) { # ensure dataOntology exists

    if (length(patient_list) > 1){
      patient_list <- paste(patient_list, collapse = ",")
    }

    queryStatement <- paste0('SELECT person_id, device_concept_id, device_exposure_start_datetime, device_exposure_end_datetime, device_type_concept_id, device_source_value, visit_occurrence_id, device_source_concept_id FROM device_exposure WHERE person_id IN (', patient_list,') ')

    if (declare==TRUE) {message("Loading Devices data...")}
    ptDeviceData <- sqlQuery(queryStatement)
    ptDeviceData <- data.table(ptDeviceData) # convert to data.table


    ### check for any data
    if (nrow(ptDeviceData)==0) {
      message("No device data found for patient list")
    } else {
      if (declare==TRUE) {message("Device data loaded; formatting...")}
      # obtain table specific ontology
      deviceTableOntology = dataOntology[grep("Device",domain_id)]

      # format clinical data
      ptDeviceData <- merge(ptDeviceData, deviceTableOntology[,c("concept_id","vocabulary_id","concept_code","concept_name")], by.x="device_concept_id",by.y="concept_id",all.x=TRUE)
      names(ptDeviceData)[names(ptDeviceData) == 'concept_code'] <- 'device_concept_code' # rename column
      names(ptDeviceData)[names(ptDeviceData) == 'concept_name'] <- 'device_concept_name' # rename column
      names(ptDeviceData)[names(ptDeviceData) == 'vocabulary_id'] <- 'device_concept_vocabulary' # rename column
      ptDeviceData <- ptDeviceData[,-"device_concept_id"]

      ptDeviceData <- merge(ptDeviceData, deviceTableOntology[,c("concept_id","vocabulary_id", "concept_code","concept_name")], by.x="device_source_concept_id",by.y="concept_id",all.x=TRUE)
      names(ptDeviceData)[names(ptDeviceData) == 'concept_code'] <- 'device_source_code' # rename column
      names(ptDeviceData)[names(ptDeviceData) == 'concept_name'] <- 'device_source_name' # rename column
      names(ptDeviceData)[names(ptDeviceData) == 'vocabulary_id'] <- 'device_source_vocabulary' # rename column
      ptDeviceData <- ptDeviceData[,-"device_source_concept_id"]

      # format metadata
      ptDeviceData <- merge(ptDeviceData,dataOntology[,c("concept_id","concept_name")],by.x="device_type_concept_id",by.y="concept_id", all.x=TRUE)
      names(ptDeviceData)[names(ptDeviceData) == 'concept_name'] <- 'device_type' # rename column
      ptDeviceData <- ptDeviceData[,-"device_type_concept_id"]

      if (declare==TRUE) {message("Device data formatted successfully.")}

    }

    return(ptDeviceData)

  } else { #endif dataOntology exists
    message("Error: dataOntology does not exist. Please first run makeDataOntology.")
  }
}
