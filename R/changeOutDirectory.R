
#' Change outDirectory
#'
#' Sets the current outDirectory which will store the Data Ontology and all function output. Option to create directory if does not exist.
#'
#' @param outdir directory path
#' @param create TRUE/FALSE (will create the directory if it does not exist)
#'
#' @return none (called for side effect: sets outDirectory)
#' @export
#'
#' @examples
#' changeOutDirectory(outdir=“~/”, create=FALSE)
changeOutDirectory <- function(outdir, create = FALSE) {

  if (dir.exists(outdir)) {
    message(paste0(outdir, " set as OutDirectory. "))
    options("outDirectory" = outdir)
  } else {
    if (create == TRUE) {
      dir.create(outdir)
      options("outDirectory" = outdir)
      message(paste0(outdir, " does not exist. Created and set to OutDirectory. "))
    } else {
      message(paste0(outdir, " does not exist. Please set 'create = TRUE' if you wish to create it or choose an already existing directory. OutDirectory not set. "))
    }
  }

}
