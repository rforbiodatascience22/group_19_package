#' Convert from DNA to RNA
#'
#' @param name_me2 DNA sequence
#'
#' @return Converts from DNA to RNA
#' @export
#'
#' @examples (function2("AATTGGAT"))
function2 <- function(name_me2){
  name_me3 <- gsub("T", "U", name_me2)
  return(name_me3)
}
