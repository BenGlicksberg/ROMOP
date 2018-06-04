

#############################
######### GENERAL ##########
#############################

#define standard_concepts
standard_concepts <- function(){
  data.table("domain_type"= c("Measurement","Condition","Drug","Observation","Device","Procedure"),"concepts"= c("LOINC,SNOMED,CPT4","SNOMED","RxNorm,CPT4,NDC","SNOMED,CPT4,LOINC,HCPCS","SNOMED,HCPCS","SNOMED,CPT4,HCPCS"))
}

### general query function ###
sqlQuery <- function(query) {

  # creating connection object
  drv <- DBI::dbDriver(Sys.getenv("driver"))
  con <- DBI::dbConnect(drv, user=Sys.getenv("username"), password=Sys.getenv("password"), dbname=Sys.getenv("dbname"), host=Sys.getenv("host"), port = as.integer(Sys.getenv("port")))

  # close db connection after function call exits
  on.exit(DBI::dbDisconnect(con))

  # send query
  res <-DBI::dbSendQuery(con, query)

  # get elements from results
  result <- DBI::fetch(res, -1)

  return(result)
}

#############################
######## PROCESSING #########
#############################

### mark any empty Demographics fields as Unknown
markNAasUnknown <- function(tbl, ColToUse, declare=FALSE) {

  if (ColToUse %in% colnames(tbl)) {
    if (any(is.na(tbl[is.na(get(ColToUse))]))) {
      missing_rows=tbl[is.na(get(ColToUse))]
      tbl[is.na(get(ColToUse)),eval(ColToUse):="Unknown"]
    } else { # no NA values in column
      if (declare==TRUE) {
        message(paste0("no NA values found for ", ColToUse))
      }
    }

  } else {
    message(paste0("column ", ColToUse, " not found"))
  }

  return(tbl)

}


## check search input parameters
checkParameters <- function(strategy_in, function_in, strategy_out, function_out) {
  pass_requirements = FALSE

  if (strategy_in %in% c("direct","mapped") & function_in %in% c("and","or")) {
    if (!is.null(strategy_out) & !is.null(function_out)) {
      if (strategy_out %in% c("direct","mapped") & function_out %in% c("and","or")) {
        pass_requirements <- TRUE
      }
    } else {
      pass_requirements <- TRUE
    }
  }

  return(pass_requirements)

}




## unpack vocabularies and codes for search function
#' @import data.table
unpackAndMap <- function(vocabularies_input, codes_input) {
  vocabularies_split <- trimws(strsplit(vocabularies_input,",")[[1]])
  codes_split <- trimws(strsplit(codes_input,",")[[1]])

  # match to one another
  dataCriteria <- data.table::data.table(vocabularies = vocabularies_split, codes = codes_split)

  dataCriteria <- dataCriteria[, list( # unpack codes
    codes = trimws(unlist(strsplit(codes, ";")))),
    by = vocabularies]

  # map inclusion criteria to dataOntology
  dataCriteriaMapped <- merge(dataCriteria, dataOntology, by.x= "codes", by.y = "concept_code")
  dataCriteriaMapped <- dataCriteriaMapped[vocabularies==vocabulary_id]

  return(dataCriteriaMapped)

}

# for 'Mapped' straegy; map input concept codes to common ontology
identifySynonyms <- function(codesFormatted) {
  synonymQuery <- paste0('SELECT concept_id_1, concept_id_2, relationship_id, invalid_reason FROM concept_relationship WHERE concept_id_1 IN (',codesFormatted,');')
  synonymData <- sqlQuery(synonymQuery)
  synonymData <- data.table::data.table(synonymData)
  synonymData <- synonymData[invalid_reason == ""]
  synonymData <- synonymData[,-"invalid_reason"]

  # check for "Maps to" or "%- RxNorm%" or "%- SNOMED%" | standard concepts
  synonymDataFiltered <- synonymData[(relationship_id == "Maps to") | (grepl("- RxNorm",relationship_id)) | (grepl("- SNOMED",relationship_id)) ]

  return(synonymDataFiltered)

}

# for 'Mapped' straegy; map input concept codes (from common ontology) to common ontology descendants
#' @import data.table
identifyMappings <- function(synonymCodes) {

  mappingQuery <- paste0('SELECT ancestor_concept_id, descendant_concept_id FROM concept_ancestor A WHERE A.ancestor_concept_id IN (', synonymCodes,' );')
  mappingData <- sqlQuery(mappingQuery)
  mappingData <- data.table::data.table(mappingData)

  mappingDataInfo <- merge(mappingData,dataOntology, by.x = "descendant_concept_id", by.y = "concept_id")

  return(mappingDataInfo)

}


# identify tables to seach for concepts of interest (direct strategy)
identifyTablesDirect <- function(criteriaTable) {

  searchTable = list()

  for(d in unique(standard_concepts()$domain_type)){ # scan through all domain types
    mappingData = criteriaTable[domain_id == d]
    mappingCodes = mappingData[domain_id == d]$concept_id
    searchTable[[d]] <- mappingCodes # compile codes per domain type into one table
  }

  return(searchTable)
}


# identify tables to seach for concepts of interest (mapped strategy)
identifyTablesMapped <- function(mappingDataInfo) {

  searchTable = list()

  for(d in unique(standard_concepts()$domain_type)) { # scan through all domain types

    mappingDataInfoFiltered <- mappingDataInfo[domain_id==d]
    mappingDataInfoFiltered <-  mappingDataInfoFiltered[(grep(gsub(",","|",standard_concepts()[domain_type==d,concepts]),vocabulary_id))] # map to common concepts specifically used to the domain
    mappingCodes <- mappingDataInfoFiltered$concept_id
    searchTable[[d]] <- mappingCodes
  }

  return(searchTable)

}

### identifyPatients based on function
# function = OR (union)
identifyPatientsOR <- function(pts_condition, pts_observation, pts_measurement, pts_device, pts_drug, pts_procedure) {

  patient_list=c()

  if (!is.null(pts_condition)) {
    patient_list = union(patient_list, unique(pts_condition$person_id))
  }

  if (!is.null(pts_observation)) {
    patient_list = union(patient_list, unique(pts_observation$person_id))
  }

  if (!is.null(pts_measurement)) {
    patient_list = union(patient_list, unique(pts_measurement$person_id))
  }

  if (!is.null(pts_device)) {
    patient_list = union(patient_list, unique(pts_device$person_id))
  }

  if (!is.null(pts_drug)) {
    patient_list = union(patient_list, unique(pts_drug$person_id))
  }

  if (!is.null(pts_procedure)) {
    patient_list = union(patient_list, unique(pts_procedure$person_id))
  }

  return(patient_list)

}

# function = AND (intersect)
# To identify overlapping patients, we have to backmap the descendant terms to the original concepts
#' @import data.table
identifyPatientsAND <- function(criteriaMapped, synonymDataFiltered, mappingDataInfo, pts_condition, pts_observation, pts_measurement, pts_device, pts_drug, pts_procedure) {

  names(mappingDataInfo)[names(mappingDataInfo) == 'vocabulary_id'] <- 'mapped_vocabulary_id'
  names(mappingDataInfo)[names(mappingDataInfo) == 'concept_name'] <- 'mapped_concept_name'

  synonymMapped <- merge(mappingDataInfo[,c("descendant_concept_id","ancestor_concept_id","mapped_vocabulary_id","mapped_concept_name")], synonymDataFiltered[,c("concept_id_1","concept_id_2")], by.x = "ancestor_concept_id", by.y = "concept_id_2", allow.cartesian=TRUE)
  synonymMapped <- synonymMapped[!duplicated(synonymMapped)]

  combinedMapped <- merge(synonymMapped, criteriaMapped, by.x = "concept_id_1", by.y = "concept_id", allow.cartesian=TRUE)
  combinedMapped <- combinedMapped[!duplicated(combinedMapped)]

  combinedDirect <- merge(mappingDataInfo, criteriaMapped, by.x = "ancestor_concept_id", by.y = "concept_id", allow.cartesian=TRUE)
  combinedDirect <- combinedDirect[!duplicated(combinedDirect)]


  ### derive patient list by concept_codes
  # create code dictionary per original concept input
  # initializepatient_list

  unique_codes <- unique(criteriaMapped$codes)

  code_map = list()
  patient_list = list()

  for(c in unique_codes) {
    seed_codes = paste(criteriaMapped[codes == c]$concept_id,collapse=",")
    code_map[[c]] <- c(seed_codes) # initialize list with original concept code (i.e. in case of ATC category)
    code_map[[c]] <- c(code_map[[c]], combinedDirect[ancestor_concept_id %in% seed_codes]$descendant_concept_id) # add in direct mapped descendants
    code_map[[c]] <- c(code_map[[c]], combinedMapped[concept_id_1 %in% seed_codes]$descendant_concept_id)  # add in synonym codes and descendants

    patient_list[[c]] <- c()
  }

  if (!is.null(pts_condition)) { #Condition

    condition_codes <- unique(criteriaMapped[domain_id=="Condition"]$codes)

    for(c in condition_codes) {
      patient_list[[c]]  <- union(patient_list[[c]], pts_condition[condition_concept_id %in% code_map[[c]]]$person_id)
    }
  }

  if (!is.null(pts_observation)) { #Observation
    observation_codes <- unique(criteriaMapped[domain_id=="Observation"]$codes)

    for(c in observation_codes) {
      patient_list[[c]]  <- union(patient_list[[c]], pts_observation[observation_concept_id %in% code_map[[c]]]$person_id)
    }
  }

  if (!is.null(pts_measurement)) { #Measurement
    measurement_codes <- unique(criteriaMapped[domain_id=="Measurement"]$codes)

    for(c in measurement_codes) {
      patient_list[[c]]  <- union(patient_list[[c]], pts_measurement[measurement_concept_id %in% code_map[[c]]]$person_id)
    }
  }

  if (!is.null(pts_device)) {#Device
    device_codes <- unique(criteriaMapped[domain_id=="Device"]$codes)

    for(c in device_codes) {
      patient_list[[c]]  <- union(patient_list[[c]], pts_device[device_concept_id %in% code_map[[c]]]$person_id)
    }
  }

  if (!is.null(pts_drug)) { #Drug
    drug_codes = unique(criteriaMapped[domain_id=="Drug"]$codes)

    for(c in drug_codes) {
      patient_list[[c]]  <- union(patient_list[[c]], pts_drug[drug_concept_id %in% code_map[[c]]]$person_id)
    }
  }

  if (!is.null(pts_procedure)) {#Procedure
    procedure_codes <- unique(criteriaMapped[domain_id=="Procedure"]$codes)

    for(c in procedure_codes) {
      patient_list[[c]]  <- union(patient_list[[c]], pts_procedure[procedure_concept_id %in% code_map[[c]]]$person_id)
    }
  }

  # get intersected list
  patient_list_intersected = Reduce(intersect,patient_list)

  return(patient_list_intersected)

}


# add counts to search query concepts by unique patients
#' @import dplyr data.table
summarizeFoundConcepts <- function(pts_condition, pts_observation, pts_measurement, pts_device, pts_drug, pts_procedure){

  conceptCount <- data.table(matrix(nrow=0,ncol=2))
  colnames(conceptCount) <- c("concept_id","pt_count")

  summarizeConcepts <- function(tblname, colname) {
    tbl_concepts <- tblname %>%
      group_by_(colname) %>%
      summarise(COUNT = n())
    tbl_concepts <- data.table(tbl_concepts)
    colnames(tbl_concepts) <-  c("concept_id","pt_count")
    return(tbl_concepts)
  }


  if (!is.null(pts_condition)) {
    condition_concepts_count <- summarizeConcepts(pts_condition,"condition_concept_id")
    conceptCount <- rbind(conceptCount, condition_concepts_count)
  }

  if (!is.null(pts_observation)) {
    observation_concepts_count <- summarizeConcepts(pts_observation,"observation_concept_id")
    conceptCount <- rbind(conceptCount, observation_concepts_count)
  }

  if (!is.null(pts_measurement)) {
    measurement_concepts_count <- summarizeConcepts(pts_measurement,"measurement_concept_id")
    conceptCount <- rbind(conceptCount, measurement_concepts_count)
  }

  if (!is.null(pts_device)) {
    device_concepts_count <- summarizeConcepts(pts_device,"device_concept_id")
    conceptCount <- rbind(conceptCount, device_concepts_count)
  }

  if (!is.null(pts_drug)) {
    drug_concepts_count <- summarizeConcepts(pts_drug,"drug_concept_id")
    conceptCount <- rbind(conceptCount, drug_concepts_count)
  }

  if (!is.null(pts_procedure)) {
    procedure_concepts_count <- summarizeConcepts(pts_procedure,"procedure_concept_id")
    conceptCount <- rbind(conceptCount, procedure_concepts_count)
  }

  return(conceptCount)

}


