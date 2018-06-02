

#' Find patients based on clinical critera
#'
#' Identify patients based on clinical data inclusion (and exclusion, if desired) criteria. Flexible to allow for multiple data types, vocabularies, and concepts.
#' @param strategy_in "mapped" or "direct" (dictates the strategy for how inclusion criteria are treated. "direct" searches for codes as provided, "mapped" maps criteria to standard concepts and finds descendants.
#' @param vocabulary_in vocabularies for inclusion criteria (comma-separated string of vocabularies)
#' @param codes_in specific concept codes for inclusion criteria (semi-colon separated string of code concepts, corresponding to the order for vocabulary_in. Multiple codes can be used per vocabulary and should be comma-separated.)
#' @param function_in  "and" or "or" (dictates how multiple inclusion should be treated. "and" necessitates that all inclusion criteria are met (i.e., intersection), while "or" allows for any critera to be met (i.e., union) )
#' @param strategy_out "mapped", "direct", or NULL (default) (dictates the strategy for how exclusion are treated. NULL indicates no exclusion criteria.)
#' @param vocabulary_out vocabularies for exclusion criteria or NULL (default) (comma-separated string of relevant vocabularies for exclusion criteria. NULL indicates no exclusion criteria)
#' @param codes_out specific concept codes for exclusion criteria or NULL (default) (semi-colon separated string of code concepts for inclusion criteria, corresponding to the order for vocabulary_out. Multiple codes can be used per vocabulary and should be comma-separated. NULL indicates no exclusion criteria.)
#' @param function_out "and", "or", or NULL (default) (dictates how multiple exclusion should be treated. and necessitates that all exclusion criteria are met (i.e., intersection), while or allows for any critera to be met (i.e., union). NULL indicates no exclusion criteria. )
#' @param declare TRUE/FALSE will output status and data information during the process
#' @param save TRUE/FALSE whether intermediate components of the search should be saved (e.g., mapped concepts found with unique patient counts per concept).
#' @param out_name name assigned to search query or NULL (if save = TRUE, saves query using provided name. If the provided name already exists as a directory (or is NULL), the directory defaults to datetime name)
#'
#' @return List of patients that meet inclusion criteria (and not exclusion criteria if entered).
#' @import data.table DBI odbc
#' @export
#'
#' @examples
#' patient_list = findPatients(strategy_in="mapped", vocabulary_in = "ICD10CM", codes_in = "F41", strategy_out="mapped", vocabulary_out = "MeSH", codes_out = "D002998", function_out = "and")
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



################## specific table search functions

#' @import data.table DBI odbc
searchCondition <- function(useSource,codes) {
  conditionQuery <- paste0('SELECT person_id, condition_concept_id FROM condition_occurrence WHERE condition',useSource,'_concept_id IN (',codes,') ')
  dataCondition <- sqlQuery(conditionQuery)
  dataCondition <- data.table(dataCondition)
  dataCondition <- dataCondition[!duplicated(dataCondition)]
  return(dataCondition)
}

#' @import data.table DBI odbc
searchObservation <- function(useSource,codes) {
  observationQuery <- paste0('SELECT person_id, observation_concept_id FROM observation WHERE observation',useSource,'_concept_id IN (',codes,') ')
  dataObservation <- sqlQuery(observationQuery)
  dataObservation <- data.table(dataObservation)
  dataObservation <- dataObservation[!duplicated(dataObservation)]
  return(dataObservation)
}

#' @import data.table DBI odbc
searchMeasurement <- function(useSource,codes) {
  measurementQuery <- paste0('SELECT person_id, measurement_concept_id FROM measurement WHERE measurement',useSource,'_concept_id IN (',codes,') ')
  dataMeasurement <- sqlQuery(measurementQuery)
  dataMeasurement <- data.table(dataMeasurement)
  dataMeasurement <- dataMeasurement[!duplicated(dataMeasurement)]
  return(dataMeasurement)
}

#' @import data.table DBI odbc
searchDrug <- function(useSource,codes) {
  drugQuery <- paste0('SELECT person_id, drug_concept_id FROM drug_exposure WHERE drug',useSource,'_concept_id IN (',codes,') ')
  dataDrug <- sqlQuery(drugQuery)
  dataDrug <- data.table(dataDrug)
  dataDrug <- dataDrug[!duplicated(dataDrug)]
  return(dataDrug)
}

#' @import data.table DBI odbc
searchDevice <- function(useSource,codes) {
  deviceQuery <- paste0('SELECT person_id, device_concept_id FROM device_exposure WHERE device',useSource,'_concept_id IN (',codes,') ')
  dataDevice <- sqlQuery(deviceQuery)
  dataDevice <- data.table(dataDevice)
  dataDevice <- dataDevice[!duplicated(dataDevice)]
  return(dataDevice)
}

#' @import data.table DBI odbc
searchProcedure<- function(useSource,codes) {
  procedureQuery <- paste0('SELECT person_id, procedure_concept_id FROM procedure_occurrence WHERE procedure',useSource,'_concept_id IN (',codes,') ')
  dataProcedure <- sqlQuery(procedureQuery)
  dataProcedure <- data.table(dataProcedure)
  dataProcedure <- dataProcedure[!duplicated(dataProcedure)]
  return(dataProcedure)
}
