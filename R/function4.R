#' Codon translator
#'
#' @param codons A sequence of codons
#'
#' @return Translates DNA sequence to amino acids
#' @export
#'
#' @examples function4("AAG","GGA","AGT")
function4 <- function(codons){
  name_me2 <- paste0(codon_table[codons], collapse = "")
  return(name_me2)
}
