
#define standard_concepts
#' @import dplyr data.table DBI odbc
standard_concepts <- function(){
  data.table("domain_type"= c("Measurement","Condition","Drug","Observation","Device","Procedure"),"concepts"= c("LOINC,SNOMED,CPT4","SNOMED","RxNorm,CPT4,NDC","SNOMED,CPT4,LOINC,HCPCS","SNOMED,HCPCS","SNOMED,CPT4,HCPCS"))
}

### initialize outDirectory as current working directory
outDirectory = getwd()









