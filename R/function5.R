#' AA frequency plotter
#' @importFrom magrittr %>%
#' @param name_me2 Amino acid sequence
#'
#' @return Bar plot showing frequency of each amino acid
#' @export
#'
#' @examples (function5("ILKA"))
function5 <- function(name_me2){
  name_me3 <- name_me2 %>%
    stringr::str_split(pattern = stringr::boundary("character"), simplify = TRUE) %>%
    as.character() %>%
    unique()

  counts <- sapply(name_me3, function(amino_acid) stringr::str_count(string = name_me2, pattern =  amino_acid)) %>%
    as.data.frame()

  colnames(counts) <- c("Counts")
  counts[["Name_me2"]] <- rownames(counts)

  name_me4 <- counts %>%
    ggplot2::ggplot(ggplot2::aes(x = Name_me2, y = Counts, fill = Name_me2)) +
    ggplot2::geom_col() +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none")

  return(name_me4)
}
