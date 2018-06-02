
#' Summarizes patient demographic data
#'
#' Summarizes patient demographic data from the getDemographics function.
#' @param ptDemo patient demographics table: ptDemo is the patient demographics object from the getDemographics function output.
#'
#' @return none (called for side effect: prints table)
#' @import dplyr data.table
#' @export
#'
#' @examples
#' \dontrun{
#' summarizeDemographics(ptDemo)
#' }
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
