#############################
# Ben Glicksberg
# Butte Lab / ICHS / UCSF
# 2018
#############################

library(dplyr)
library(DBI)
library(odbc)
library(data.table)


#############################
###### INITIALIZATION #######
#############################

### securely retrieve credentials stored in environment variables
# ~/.Renviron

#define standard_concepts
standard_concepts= data.table("domain_type"= c("Measurement","Condition","Drug","Observation","Device","Procedure"),"concepts"= c("LOINC,SNOMED,CPT4","SNOMED","RxNorm,CPT4,NDC","SNOMED,CPT4,LOINC,HCPCS","SNOMED,HCPCS","SNOMED,CPT4,HCPCS"))

### initialize outDirectory
outDirectory = "out/"





#############################
######### GENERAL ##########
#############################

### general query function ###
sqlQuery <- function(query) {

  # creating connection object
  drv <- dbDriver(Sys.getenv("driver"))
  con <- dbConnect(drv, user=Sys.getenv("username"), password=Sys.getenv("password"), dbname=Sys.getenv("dbname"), host=Sys.getenv("host"), port = as.integer(Sys.getenv("port")))

  # close db connection after function call exits
  on.exit(dbDisconnect(con))

  # send query
  res <- dbSendQuery(con, query)

  # get elements from results
  result <- fetch(res, -1)

  return(result)
}


changeOutDirectory <- function(outdir, create = FALSE) {

  if (dir.exists(outdir)) {
    message(paste0(outdir, " set as OutDirectory. "))
    outDirectory = outdir
  } else {
    if (create == TRUE) {
      message(paste0(outdir, " does not exist. Created and set to OutDirectoy. "))
    } else {
      message(paste0(outdir, " does not exist. Please set 'create = TRUE' if you wish to create it or choose an already existing directory. OutDirectory not set. "))
    }
  }

}


#############################
######## PROCESSING #########
#############################

summarizeDemographics <- function(ptDemo) {

  message(paste0("# of patients: ", ptDemo %>% tally()))
  message(paste0("Mean age: ",round(mean(ptDemo$age),3)))
  message(paste0("Median age: ",round(median(ptDemo$age),3)))
  message(paste0("STD age: ",round(sd(ptDemo$age),3)))

  # compile Status info
  message("Status breakdown:")
  print(data.table(ptDemo %>% group_by(Status) %>% summarise (n = n()) %>% mutate(proportion = n / sum(n))))

  # compile gender info
  message("Gender breakdown:")
  print(data.table(ptDemo %>% group_by(Gender) %>% summarise (n = n()) %>% mutate(proportion = n / sum(n))))

  # compile race info
  message("Race breakdown:")
  print(data.table(ptDemo %>% group_by(Race) %>% summarise (n = n()) %>% mutate(proportion = n / sum(n))))

  # compile ethnicity info
  message("Ethnicity breakdown:")
  print(data.table(ptDemo %>% group_by(Ethnicity) %>% summarise (n = n()) %>% mutate(proportion = n / sum(n))))

}

# return table of available domains and vocabularies
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
unpackAndMap <- function(vocabularies_input, codes_input) {
  vocabularies_split <- trimws(strsplit(vocabularies_input,",")[[1]])
  codes_split <- trimws(strsplit(codes_input,",")[[1]])

  # match to one another
  dataCriteria <- data.table(vocabularies = vocabularies_split, codes = codes_split)

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
  synonymData <- data.table(synonymData)
  synonymData <- synonymData[invalid_reason == ""]
  synonymData <- synonymData[,-"invalid_reason"]

  # check for "Maps to" or "%- RxNorm%" or "%- SNOMED%" | standard concepts *** decision
  synonymDataFiltered <- synonymData[(relationship_id == "Maps to") | (grepl("- RxNorm",relationship_id)) | (grepl("- SNOMED",relationship_id)) ]

  return(synonymDataFiltered)

}

# for 'Mapped' straegy; map input concept codes (from common ontology) to common ontology descendants
identifyMappings <- function(synonymCodes) {

  mappingQuery <- paste0('SELECT ancestor_concept_id, descendant_concept_id FROM concept_ancestor A WHERE A.ancestor_concept_id IN (', synonymCodes,' );')
  mappingData <- sqlQuery(mappingQuery)
  mappingData <- data.table(mappingData)

  mappingDataInfo <- merge(mappingData,dataOntology, by.x = "descendant_concept_id", by.y = "concept_id")

  return(mappingDataInfo)

}


# identify tables to seach for concepts of interest (direct strategy)
identifyTablesDirect <- function(criteriaTable) {

   searchTable = list()

       for(d in unique(standard_concepts$domain_type)){ # scan through all domain types
         mappingData = criteriaTable[domain_id == d]
         mappingCodes = mappingData[domain_id == d]$concept_id
         searchTable[[d]] <- mappingCodes # compile codes per domain type into one table
       }

  return(searchTable)
}


# identify tables to seach for concepts of interest (mapped strategy)
identifyTablesMapped <- function(mappingDataInfo) {

  searchTable = list()

  for(d in unique(standard_concepts$domain_type)) { # scan through all domain types

    mappingDataInfoFiltered <- mappingDataInfo[domain_id==d]
    mappingDataInfoFiltered <-  mappingDataInfoFiltered[(grep(gsub(",","|",standard_concepts[domain_type==d,concepts]),vocabulary_id))] # map to common concepts specifically used to the domain
    mappingCodes <- mappingDataInfoFiltered$descendant_concept_id
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



#############################
########## ONTOLOGY #########
#############################

makeDataOntology <- function(declare=FALSE, store_ontology=FALSE) {
  if (declare==TRUE) {message("Retrieving concept data...")}
  create <- TRUE
  found <- FALSE

  if (store_ontology==TRUE) {
    if (file.exists(paste0(outDirectory,"dataOntology.rds")) ) {
      message("Data Ontology found; loading... ")
      dataOntology = readRDS(paste0(outDirectory,"dataOntology.rds"))
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
    message(paste0("Storing Data Ontology: ", outDirectory,"dataOntology.rds"))
    saveRDS(dataOntology, paste0(outDirectory,"dataOntology.rds"))

  }

  return(dataOntology)

}

# function to simply extract descendants for concepts of interest
exploreConcepts <- function(vocabulary, codes) {

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
}



#############################
####### FIND PATIENTS #######
#############################

#### specific table search functions

searchCondition <- function(useSource,codes) {
  conditionQuery <- paste0('SELECT person_id, condition_concept_id FROM condition_occurrence WHERE condition',useSource,'_concept_id IN (',codes,') ')
  dataCondition <- sqlQuery(conditionQuery)
  dataCondition <- data.table(dataCondition)
  dataCondition <- dataCondition[!duplicated(dataCondition)]
  return(dataCondition)
}

searchObservation <- function(useSource,codes) {
  observationQuery <- paste0('SELECT person_id, observation_concept_id FROM observation WHERE observation',useSource,'_concept_id IN (',codes,') ')
  dataObservation <- sqlQuery(observationQuery)
  dataObservation <- data.table(dataObservation)
  dataObservation <- dataObservation[!duplicated(dataObservation)]
  return(dataObservation)
}

searchMeasurement <- function(useSource,codes) {
  measurementQuery <- paste0('SELECT person_id, measurement_concept_id FROM measurement WHERE measurement',useSource,'_concept_id IN (',codes,') ')
  dataMeasurement <- sqlQuery(measurementQuery)
  dataMeasurement <- data.table(dataMeasurement)
  dataMeasurement <- dataMeasurement[!duplicated(dataMeasurement)]
  return(dataMeasurement)
}

searchDrug <- function(useSource,codes) {
  drugQuery <- paste0('SELECT person_id, drug_concept_id FROM drug_exposure WHERE drug',useSource,'_concept_id IN (',codes,') ')
  dataDrug <- sqlQuery(drugQuery)
  dataDrug <- data.table(dataDrug)
  dataDrug <- dataDrug[!duplicated(dataDrug)]
  return(dataDrug)
}

searchDevice <- function(useSource,codes) {
  deviceQuery <- paste0('SELECT person_id, device_concept_id FROM device_exposure WHERE device',useSource,'_concept_id IN (',codes,') ')
  dataDevice <- sqlQuery(deviceQuery)
  dataDevice <- data.table(dataDevice)
  dataDevice <- dataDevice[!duplicated(dataDevice)]
  return(dataDevice)
}

searchProcedure<- function(useSource,codes) {
  procedureQuery <- paste0('SELECT person_id, procedure_concept_id FROM procedure_occurrence WHERE procedure',useSource,'_concept_id IN (',codes,') ')
  dataProcedure <- sqlQuery(procedureQuery)
  dataProcedure <- data.table(dataProcedure)
  dataProcedure <- dataProcedure[!duplicated(dataProcedure)]
  return(dataProcedure)
}


### main findPatients function
findPatients <- function(strategy_in="mapped", vocabulary_in, codes_in, function_in = "or", strategy_out = NULL, vocabulary_out = NULL, codes_out = NULL, function_out = NULL, declare=FALSE, save=FALSE, out_name=NULL) {

  if (exists("dataOntology")) { # ensure dataOntology exists

  ## strategy:
  #### mapped- map to common ontology, find descendants, and search | RECOMMENDED
  #### direct- search directly for included codes only
  strategy_in <- tolower(strategy_in) # force lowercase
  if (!is.null(strategy_out)) {strategy_out = tolower(strategy_out)}

  ## function:
  #### and- criteria require INTERSECTION (i.e. criteria 1 AND criteria 2 AND ...)
  #### or- criteria require UNION (i.e. criteria 1 OR criteria 2 OR ...)
  function_in = tolower(function_in) # force lowercase
  if (!is.null(function_out)) {function_out = tolower(function_out)}

  # check parameters
  pass_requirements <- checkParameters(strategy_in, function_in, strategy_out, function_out)

  # require correct parameters
  if (pass_requirements == TRUE) {

  if (save==TRUE) {

    if (is.null(out_name)) {
      outdir <- paste0(outDirectory,gsub(" ", "_",Sys.time()))
      dir.create(outdir)
    } else {
      # check to see if directory already exists
      outdir <- paste0(outDirectory,out_name)
      if (!dir.exists(outdir)) {
      dir.create(paste0(outDirectory,out_name))
      } else {
        outdir <- paste0(outDirectory,gsub(" ", "_",Sys.time()))
        dir.create(outdir)
        message(paste0(outDirectory, out_name, " directory already exists. Saving results to: ", outdir))
      }
    }

    fout <- paste0(outdir,"/query.txt")

    sink(fout)
    cat(paste0("inclusion strategy: ", strategy_in ,"\n"))
    cat(paste0("inclusion vocabularies: ", vocabulary_in ,"\n"))
    cat(paste0("inclusion codes: ", codes_in ,"\n"))
    cat(paste0("inclusion function: ", function_in ,"\n"))
    cat(paste0("exclusion strategy: ", strategy_out ,"\n"))
    cat(paste0("exclusion vocabularies: ", vocabulary_out ,"\n"))
    cat(paste0("exclusion codes: ", codes_out ,"\n"))
    cat(paste0("exclusion function: ", function_out ,"\n"))
    sink()

  }


  ## vocabulary_in: vocabularies that will be used for INCLUSION criteria
  #### format = "VOCABULARY1, VOCABULARY2, VOCABULARY3"
  ###### vocabulary_in = "ATC, ICD10CM, SNOMED"

  ## codes_in: codes corresponding to order of vocabularies_in that will be used for INCLUSION criteria
  #### format = "c1v1;c2v1, c1v2;c2v2;c3v2, c1v3"
  ###### codes_in = "L01XC19;J01X, Y07.9;O33.7, 25343008"

  ## vocabulary_out: vocabularies that will be used for EXCLUSION criteria
  #### same format as above

  ## codes_out: codes corresponding to order of vocabularies_out that will be used for EXCLUSION criteria
  #### same format as above

  # 1- INCLUSION

  #### unpack vocabulary_in and codes_in
  inclusionCriteriaMapped <- unpackAndMap(vocabulary_in,codes_in)

  ### check to see if any codes matched
  if (nrow(inclusionCriteriaMapped)>0) {

  includeCodesFormatted <- paste0(inclusionCriteriaMapped$concept_id,collapse=",")

  if (declare == TRUE) {
    message("The following INCLUSION criteria are being used: \n")
    print(inclusionCriteriaMapped)
  }

  if (save == TRUE) {
    fout = paste0(outdir,"/inclusion_criteria_mapped.txt")
    write.table(inclusionCriteriaMapped, file = fout, sep='\t', row.names=F, quote=F)
  }


  # MAPPING
  if (strategy_in == "direct") {

    useSource <- "_source" # search _source_concept_id
    includeSearchTable <- identifyTablesDirect(inclusionCriteriaMapped)

  } else if (strategy_in == "mapped") {
    ## RECOMMENDED, but can lead to:
    #### cross-mapping (i.e. from ICD code (e.g. diabetes) to procedure/measurement)
    #### extra mapping (i.e. ICD 10 code --> ICD9 & 10 results)

    useSource <- "" # search _concept_id

    # get common ontology synonyms
    includeSynonymDataFiltered <- identifySynonyms(includeCodesFormatted)
    includeSynonymCodes <- paste(c(includeCodesFormatted, unique(includeSynonymDataFiltered$concept_id_2)),collapse=",") ## adds original codes into ancestor query (b/c of scenarios with ATC))

    # get descendents
    includeMappingDataInfo <- identifyMappings(includeSynonymCodes)

    if (declare == TRUE) {
      message("The following INCLUSION mapped concepts are being queried: \n")
      print(includeMappingDataInfo)
    }
    # save mapped concepts after patient count per concept added

    # get tables to search for mapped concepts
    includeSearchTable <- identifyTablesMapped(includeMappingDataInfo)


  } #endif strategy_in == mapped

  # 2- SEARCH INCLUSION

  # if any condition table codes
  if (length(includeSearchTable$Condition)>0) {
    if (declare==TRUE) {message("querying Conditions...")}
    condition_codes <- paste(includeSearchTable$Condition,collapse=",")
    pts_condition_include <- searchCondition(useSource,condition_codes)
  } else {
    pts_condition_include <- NULL
  }

  # if any observation table codes
  if (length(includeSearchTable$Observation)>0) {
    if (declare==TRUE){message("querying Observations") }
    observation_codes <- paste(includeSearchTable$Observation,collapse=",")
    pts_observation_include <- searchObservation(useSource,observation_codes)
  } else {
    pts_observation_include <- NULL
  }

  # if any measurement table codes
  if (length(includeSearchTable$Measurement)>0) {
    if (declare==TRUE) {message("querying Measurements")}
    measurement_codes <- paste(includeSearchTable$Measurement,collapse=",")
    pts_measurement_include <- searchMeasurement(useSource,measurement_codes)
  } else {
    pts_measurement_include <- NULL
  }

  # if any drug table codes
  if (length(includeSearchTable$Drug)>0) {
    if (declare==TRUE) {message("querying Drugs")}
    drug_codes <- paste(includeSearchTable$Drug,collapse=",")
    pts_drug_include <- searchDrug(useSource,drug_codes)
  } else {
    pts_drug_include <- NULL
  }

  # if any device table codes
  if (length(includeSearchTable$Device)>0) {
    if (declare==TRUE) {message("querying Devices")}
    device_codes <- paste(includeSearchTable$Drug,collapse=",")
    pts_device_include <- searchDevice(useSource,device_codes)
  } else {
    pts_device_include <- NULL
  }

  # if any procedure table codes
  if (length(includeSearchTable$Procedure)>0) {
    if (declare==TRUE) {message("querying Procedures")}
    procedure_codes <- paste(includeSearchTable$Procedure,collapse=",")
    pts_procedure_include <- searchProcedure(useSource,procedure_codes)
  }else{
    pts_procedure_include <- NULL
  }

  # save mapped concepts with patient counts
  if (save == TRUE) {
    fout = paste0(outdir,"/inclusion_criteria_mapped_concepts.txt")
    includeDataInfowPatients <- summarizeFoundConcepts(pts_condition_include, pts_observation_include, pts_measurement_include, pts_device_include, pts_drug_include, pts_procedure_include)
    # merge pt counts with all concepts
    includeMappingCombined <- merge(includeMappingDataInfo, includeDataInfowPatients, by.x = "descendant_concept_id", by.y = "concept_id", all.x = TRUE)
    write.table(includeMappingCombined, file = fout, sep='\t', row.names=F, quote=F)
  }


  # 3- EXCLUSION
  # vocabulary_out = "ICD9CM"
  # codes_out = "250.00"

  if (!is.null(vocabulary_out) & !is.null(codes_out) & !is.null(strategy_out) & !is.null(function_out)) { # if any exclusion criteria

     #### unpack vocabulary_in and codes_in
     exclusionCriteriaMapped <- unpackAndMap(vocabulary_out,codes_out)

     if (nrow(exclusionCriteriaMapped)>0) {

     excludeCodesFormatted <- paste0(exclusionCriteriaMapped$concept_id,collapse=",")

     if (declare == TRUE) {
       message("The following EXCLUSION criteria are being used: \n")
       print(exclusionCriteriaMapped)
     }

     if (save == TRUE) {
       fout <- paste0(outdir,"/exclusion_criteria_mapped.txt")
       write.table(exclusionCriteriaMapped, file = fout, sep='\t', row.names=F, quote=F)
     }



     if (strategy_out == "direct") {

      useSource <- "_source" # search _source_concept_id
      excludeSearchTable <- identifyTablesDirect(exclusionCriteriaMapped)

     } else if (strategy_out == "mapped") {

       # get common ontology synonyms
       excludeSynonymDataFiltered <- identifySynonyms(excludeCodesFormatted)
       excludeSynonymCodes <- paste(c(excludeCodesFormatted, unique(excludeSynonymDataFiltered$concept_id_2)),collapse=",") ## adds original codes into ancestor query (b/c of scenarios with ATC))

       # get descendents
       excludeMappingDataInfo <- identifyMappings(excludeSynonymCodes)

       if (declare == TRUE) {
         message("The following EXCLUSION mapped concepts are being queried: \n")
         print(excludeMappingDataInfo)
       }

       # save mapped concepts once patient counts are added

       # get tables to search for mapped concepts
       excludeSearchTable <- identifyTablesMapped(excludeMappingDataInfo)

     }

    # 4- SEARCH EXCLUSION

     # if any condition table codes
     if (length(excludeSearchTable$Condition)>0) {
       if (declare==TRUE) {message("querying Conditions...")}
       condition_codes <- paste(excludeSearchTable$Condition,collapse=",")
       pts_condition_exclude <- searchCondition(useSource,condition_codes)
     } else {
       pts_condition_exclude <- NULL
     }

     # if any observation table codes
     if (length(excludeSearchTable$Observation)>0) {
       if (declare==TRUE) {message("querying Observations")}
       observation_codes <- paste(excludeSearchTable$Observation,collapse=",")
       pts_observation_exclude <- searchObservation(useSource,condition_codes)
     } else {
       pts_observation_exclude <- NULL
     }

     # if any measurement table codes
     if (length(excludeSearchTable$Measurement)>0) {
       if (declare==TRUE) {message("querying Measurements")}
       measurement_codes <- paste(excludeSearchTable$Measurement,collapse=",")
       pts_measurement_exclude <- searchMeasurement(useSource,measurement_codes)
     } else {
       pts_measurement_exclude <- NULL
     }


     # if any drug table codes
     if (length(excludeSearchTable$Drug)>0) {
       if (declare==TRUE) {message("querying Drugs")}
       drug_codes <- paste(excludeSearchTable$Drug,collapse=",")
       pts_drug_exclude <- searchDrug(useSource,drug_codes)
     } else {
       pts_drug_exclude <- NULL
     }

     # if any device table codes
     if (length(excludeSearchTable$Device)>0) {
       if (declare==TRUE) {message("querying Devices")}
       device_codes <- paste(excludeSearchTable$Device,collapse=",")
       pts_device_exclude <- searchDevice(useSource,device_codes)
     } else {
       pts_device_exclude <- NULL
     }

     # if any procedure table codes
     if (length(excludeSearchTable$Procedure)>0) {
       if (declare==TRUE) {message("querying Procedures")}
       procedure_codes <- paste(excludeSearchTable$Procedure,collapse=",")
       pts_procedure_exclude <- searchProcedure(useSource,procedure_codes)
     } else {
       pts_procedure_exclude <- NULL
     }

     # save mapped concepts with patient counts
     if (save == TRUE) {
       fout <- paste0(outdir,"/exclusion_criteria_mapped_concepts.txt")
       excludeDataInfowPatients <- summarizeFoundConcepts(pts_condition_exclude, pts_observation_exclude, pts_measurement_exclude, pts_device_exclude, pts_drug_exclude, pts_procedure_exclude)
       # merge pt counts with all concepts
       excludeMappingCombined <- merge(excludeMappingDataInfo, excludeDataInfowPatients, by.x = "descendant_concept_id", by.y = "concept_id", all.x = TRUE)
       write.table(excludeMappingCombined, file = fout, sep='\t', row.names=F, quote=F)
     }

    } else { #endif exclusion criteria match
       message("Warning: exclusion criteria were not able to map to ontology. Therefore, query running for inclusion criteria only.")
     }
  } # endif exclusion

  # 5 - PROCESS INCLUSION(/EXCLUSION) depending on functions

  if (function_in=="or") {
    include_patient_list <- identifyPatientsOR(pts_condition_include, pts_observation_include, pts_measurement_include, pts_device_include, pts_drug_include, pts_procedure_include)
  } else if (function_in=="and") {
    include_patient_list <- identifyPatientsAND(inclusionCriteriaMapped, includeSynonymDataFiltered, includeMappingDataInfo, pts_condition_include, pts_observation_include, pts_measurement_include, pts_device_include, pts_drug_include, pts_procedure_include)
  }

  patient_list <- include_patient_list

  if (!is.null(vocabulary_out) & !is.null(codes_out) & !is.null(strategy_out) & !is.null(function_out)) { # if any exclusion criteria

   if (nrow(exclusionCriteriaMapped)>0) { #verify that exclusion criteria were found

    if (function_out=="or") {
    exclude_patient_list <- identifyPatientsOR(pts_condition_exclude, pts_observation_exclude, pts_measurement_exclude, pts_device_exclude, pts_drug_exclude, pts_procedure_exclude)
    } else if (function_out=="and") {
    exclude_patient_list <- identifyPatientsAND(exclusionCriteriaMapped, excludeSynonymDataFiltered, excludeMappingDataInfo, pts_condition_exclude, pts_observation_exclude, pts_measurement_exclude, pts_device_exclude, pts_drug_exclude, pts_procedure_exclude)
    }

    inclusion_exclusion_overlapping_patients <- intersect(patient_list,exclude_patient_list)
    print(paste0(length(inclusion_exclusion_overlapping_patients), " overlapping patients excluded from the original inclusion input based on the exclusion criteria."))

    # remove overlapping patients
    patient_list <- setdiff(patient_list,inclusion_exclusion_overlapping_patients)

   } # endif exclusion criteria found

  } # endif exclusion null


  print(paste0(length(patient_list), " patients found that meet the inclusion criteria."))


  if (save == TRUE) {
    fout = paste0(outdir,"/outcome.txt")

    sink(fout)

    if (exists("inclusion_exclusion_overlapping_patients")) {
      cat(paste0(length(include_patient_list), " patients found from the inclusion criteria ONLY.\n"))
      cat(paste0(length(exclude_patient_list), " patients found from the exclusion criteria ONLY.\n"))
      cat(paste0(length(inclusion_exclusion_overlapping_patients), " overlapping patients excluded from the original inclusion input based on the exclusion criteria.\n"))
      cat(paste0(length(patient_list), " patients found that meet the inclusion and exclusion criteria.\n"))
    } else {
    cat(paste0(length(patient_list), " patients found that meet the inclusion criteria.\n"))
    }
    sink()

    fout <- paste0(outdir,"/patient_list.txt")
    write.table(data.frame(patient_list),file=fout, sep='\t', row.names = F, quote=F)
    message(paste0("Outcome from query saved in: ",outdir))

  }


  return(patient_list)

  } else { # endif no inclusion found
    message("Error: none of the inclusion criteria were able to map to the ontology. Please check terms and try again.")
  }

  } else {  #endif pass_requirements
    message("Error: invalid strategies and/or functions selected. Please use either 'direct' or 'mapped' for strategies. Please use either 'and' or 'or' for functions.")
  }

  } else { #endif dataOntology exists
    message("Error: dataOntology does not exist. Please first run makeDataOntology.")
  }

}



#############################
######### GET DATA ##########
#############################

### get demographics data ###

getDemographics <-function(patient_list=NULL, declare=FALSE) { # patient list will restrict search

  if (exists("dataOntology")) { # ensure dataOntology exists

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


### functions for retrieving clinical data ###
### these functions require patient_list otherwise it would return too much data

getClinicalData<- function(patient_list, declare=FALSE) {

  if (exists("dataOntology")) { # ensure dataOntology exists

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

getObservations <- function(patient_list, declare=FALSE) {

  if (exists("dataOntology")) { # ensure dataOntology exists

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


getConditions <- function(patient_list, declare=FALSE) {

  if (exists("dataOntology")) { # ensure dataOntology exists

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

getProcedures <- function(patient_list, declare=FALSE){

  if (exists("dataOntology")) { # ensure dataOntology exists

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

getMedications <- function(patient_list, declare=FALSE) {

  if (exists("dataOntology")) { # ensure dataOntology exists

  queryStatement <- paste0('SELECT person_id, drug_concept_id, order_datetime, drug_exposure_start_datetime, drug_exposure_end_datetime, drug_type_concept_id, stop_reason, refills, quantity, days_supply, sig, route_concept_id, dose_unit_source_value, visit_occurrence_id, drug_source_value, drug_source_concept_id, route_source_value FROM drug_exposure WHERE person_id IN (', patient_list,') ')

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

getMeasurements <- function(patient_list, declare=FALSE) {

  if (exists("dataOntology")) { # ensure dataOntology exists

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

getDevices <- function(patient_list, declare=FALSE) {

  if (exists("dataOntology")) { # ensure dataOntology exists

  queryStatement <- paste0('SELECT person_id, device_concept_id, device_exposure_start_datetime, device_exposure_end_datetime, device_type_concept_id, device_source_value, visit_occurrence_id, device_source_concept_id FROM device_exposure WHERE person_id IN (', patient_list,') ');

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


