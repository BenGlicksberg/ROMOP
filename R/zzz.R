library(DBI)
library(odbc)
library(data.table)

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

  on.exit(dbDisconnect(con))

  return(out)

}


# check that relevant tables exist in OMOP database
checkOMOPtables <- function() {

  necessaryTables = c("concept","concept_ancestor","concept_relationship","condition_occurrence","death","device_exposure","drug_exposure","measurement","observation","person","procedure_occurrence","visit_occurrence")

  drv <- dbDriver(Sys.getenv("driver"))
  con <- dbConnect(drv, user=Sys.getenv("username"), password=Sys.getenv("password"), dbname=Sys.getenv("dbname"), host=Sys.getenv("host"), port = as.integer(Sys.getenv("port")))
  foundTablesData <- dbListTables(con)
  on.exit(dbDisconnect(con))

  missingTables <- FALSE

  for (tbls in necessaryTables) {
    if (!tbls %in% foundTablesData) { # check if table exists
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


# .onLoad checks

.onLoad <- function(...) {
  packageStartupMessage(
    paste0("Welcome to ROMOP: please refer to [https://LINK] for detailed instructions on how to use package with examples.\n
Current OutDirectory is set to",getwd(), "/out/. Please use changeOutDirectory function to set.\n
Now checking for required crednetials and server connection (note this package will not function without them). Please wait...\n")
   )

  ## Verify crednetials exist
  credentialsExist <- checkCredentialsExist()

  if (credentialsExist == TRUE) { # require credentials

    ## Verify connection
    successfulConnection <- checkOMOPconnection()

    if (successfulConnection == TRUE) { # require successful connection

      # check if relevant tables exist
      correctTables <- checkOMOPtables()

      if (correctTables == TRUE) { # require correct tables

        message("Success! Please create 'dataOntology' using the makeDataOntology function.\n e.g., dataOntology =  makeDataOntology(declare=TRUE,store_ontology = TRUE)")

      } else { # end if correct tables
        message("Missing required tables; package will not funciton correctly.")
      }

    } else { # end if successful connection
      message("Unable to connect; package will not funciton correctly.")
    }

  } else { #endif credentials
    message("Please refer to the ReadMe to set and format server credentials in the .Renviron file.")
  }
}
