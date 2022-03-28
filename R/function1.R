#' Random sequence of bases
#'
#' @param name_m2 is a number defining the length of the sequence
#'
#' @return Creates a random sequence of bases
#' @export
#'
#' @examples function(10)
function1 <- function(name_me2){
  name_me3 <- sample(c("A", "T", "G", "C"), size = name_me2, replace = TRUE)
  name_me4 <- paste0(name_me3, collapse = "")
  return(name_me4)
}
