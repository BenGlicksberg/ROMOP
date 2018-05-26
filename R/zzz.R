library(data.table)
library(RMySQL)
library(DBI)
library(odbc)

### securely retrieve credentials stored in environment variables
# ~/.Renviron


#############################
###### INITIALIZATION #######
#############################

# check credentials exist
checkCredentialsExist <- function() {
  if(Sys.getenv("driver") != "" & Sys.getenv("username") != "" & Sys.getenv("password") != "" & Sys.getenv("dbname") != "" & Sys.getenv("host") != "" & Sys.getenv("port") != ""){
    pass <- TRUE

    # load required drivers
    if (Sys.getenv("driver")=="MySQL") {
      library("RMySQL")
    } else if (Sys.getenv("driver")=="PostgreSQL") {
      library("RPostgreSQL")
    } else {
      pass <- FALSE
      message("Invalid driver type, please select only 'MySQL' or 'PostgreSQL'")
    }

  }else{
    pass <- FALSE
    message("Failed to retrieve crednetials from .Renviron. Please ensure that the file exists and is formatted correctly (please refer to ReadMe)")
  }

  return(pass)
}


# check that successful connection can be made to OMOP server
checkOMOPconnection <- function() {

  status<- tryCatch(
    {
      drv <- dbDriver(Sys.getenv("driver"))
      con <- dbConnect(drv, user=Sys.getenv("username"), password=Sys.getenv("password"), dbname=Sys.getenv("dbname"), host=Sys.getenv("host"), port = as.integer(Sys.getenv("port")))
    },
    warning = function(w) {
      # ignore
    },
    error = function(e) {
      message("Unable to establish connection to OMOP server.")
      message(e)
    }
  )

  if(!is.null(status)){
    out <- TRUE
    message("Can successfully connect to OMOP server.")
  }else{
    out <- FALSE
  }

  return(out)

}


# check that relevant tables exist in OMOP database
checkOMOPtables <- function(username,password,host,dbname) {

  necessaryTables = c("concept","concept_ancestor","concept_relationship","condition_occurrence","death","device_exposure","drug_exposure","measurement","observation","person","procedure_occurrence","visit_occurrence")

  foundTables = "SHOW TABLES;"
  foundTablesData <- sqlQuery(foundTables)
  foundTablesData <- data.table(foundTablesData)
  colnames(foundTablesData) <- "tables"

  missingTables <- FALSE

  for (tbls in necessaryTables) {
    if (!tbls %in% foundTablesData$tables) { # check if table exists
      missingTables <- TRUE
      message(paste0("missing required table: " , tbls ))
    } else { # check if any data in found table
      dataCheckQuery <- paste0("SELECT * FROM " , tbls , " LIMIT 1;")
      dataCheck <- sqlQuery(dataCheckQuery)
      if (nrow(dataCheck)==0) {
        message(paste0("Warning: no data found in table ", tbls))
      }
    }
  }

  if (missingTables == FALSE) {
    message("All required tables found!")
    return(TRUE)
  } else {
    return(FALSE)
  }

}

#############################
###### INITIALIZATION #######
#############################


# onload functions

.onLoad <- function(...) {

  message("Welcome to ROMOP: please refer to [https://LINK] for detailed instructions on how to use package with examples.\n")
  message(paste0("Current OutDirectory is set to",getwd(), "/out/. Please use changeOutDirectory function to set.\n" ))
  message("Now checking for required crednetials and server connection (note this package will not function without them). Please wait...\n")

  ## Verify crednetials exist
  credentialsExist <- checkCredentialsExist()

  if (credentialsExist == TRUE) { # require credentials

    ## Verify connection
    successfulConnection <- checkOMOPconnection(username,password,host,dbname)

    if (successfulConnection == TRUE) { # require successful connection

      # check if relevant tables exist
      correctTables <- checkOMOPtables(username,password,host,dbname)

      if (correctTables == TRUE) { # require correct tables

        message("Success! Please create 'dataOntology' using the makeDataOntology function.")

      } else { # end if correct tables
        message("Missing required tables; package will not funciton correctly.")
      }

    } else { # end if successful connection
      message("Unable to connect; package will not funciton correctly.")
    }

  } #endif credentials

}
