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
  env_vars <- c("driver", "username", "password", "dbname", "host", "port")

  if (length(intersect(env_vars,names(Sys.getenv()))) == length(env_vars)) {
      pass <- TRUE

    # load required drivers
    if (tolower(Sys.getenv("driver"))=="mysql") {
      library(RMySQL)
    } else if (tolower(Sys.getenv("driver")) %in% c("oracle", "postgresql", "redshift", "sql server", "pdw", "bigquery")) {
      library(DatabaseConnector)
      library(SqlRender)
    } else {
      pass <- FALSE
      message("Invalid driver type, please select either: 'mysql', 'oracle',  'postgresql', 'redshift', 'sql server', 'pdw', 'bigquery'")
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
      if (tolower(Sys.getenv("driver"))=="mysql") {
        drv <- dbDriver(Sys.getenv("driver"))
        fullConnectString <- setConnectFunction()
        con <- eval(parse(text = fullConnectString))
      } else {
      # creating connection object using DatabaseConnector
      con <- DatabaseConnector::connect(dbms = tolower(Sys.getenv("driver")),
                        server = Sys.getenv("host"),
                        user = Sys.getenv("username"),
                        password = Sys.getenv("password"),
                        schema = Sys.getenv("dbname"))
      }
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

  if (tolower(Sys.getenv("driver"))=="mysql") {
    on.exit(dbDisconnect(con))
  } else {
    on.exit(DatabaseConnector::disconnect(con))
  }
  return(out)

}


# check that relevant tables exist in OMOP database
checkOMOPtables <- function() {

  necessaryTables = c("concept","concept_ancestor","concept_relationship","condition_occurrence","death","device_exposure","drug_exposure","measurement","observation","person","procedure_occurrence","visit_occurrence")

  if (tolower(Sys.getenv("driver"))=="mysql") {
    drv <- dbDriver(Sys.getenv("driver"))
    fullConnectString <- setConnectFunction()
    con <- eval(parse(text = fullConnectString))
  } else {
    # creating connection object using DatabaseConnector
    con <- DatabaseConnector::connect(dbms = tolower(Sys.getenv("driver")),
                   server = Sys.getenv("host"),
                   user = Sys.getenv("username"),
                   password = Sys.getenv("password"),
                   schema = Sys.getenv("dbname"))
  }

  foundTablesData <- tolower(dbListTables(con))

  if (tolower(Sys.getenv("driver"))=="mysql") {
    on.exit(dbDisconnect(con))
  } else {
    on.exit(DatabaseConnector::disconnect(con))
  }


  missingTables <- FALSE

  for (tbls in necessaryTables) {
    if (!tbls %in% foundTablesData) { # check if table exists
      missingTables <- TRUE
      message(paste0("missing required table: " , tbls ))
    } else { # check if any data in found table
      if (tolower(Sys.getenv("driver"))=="mysql") {
        dataCheckQuery <- paste0("SELECT * FROM " , tbls , " LIMIT 1;")
      } else {
        dataCheckQuery <- paste0("SELECT TOP 1 * FROM " , tbls, ";")
      }
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
    paste0("Welcome to ROMOP: please refer to https://github.com/BenGlicksberg/ROMOP for detailed instructions on how to use package with examples.\n
Current OutDirectory is set to ",getwd(), ". Please use changeOutDirectory function to set.\n e.g., changeOutDirectory('path/to/outdir', create = TRUE) \n
Now checking for required credentials and server connection (note this package will not function without them). Please wait...\n")
   )

  ### initialize outDirectory as current working directory
  options("outDirectory" = paste0(getwd(),"/"))

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
