
library("magrittr")
library("ggplot2")
library("stringr")
install.packages("devtools")

# Storing data in object
codon_table <- c("UUU" = "F", "UCU" = "S", "UAU" = "Y", "UGU" = "C",
                 "UUC" = "F", "UCC" = "S", "UAC" = "Y", "UGC" = "C",
                 "UUA" = "L", "UCA" = "S", "UAA" = "_", "UGA" = "_",
                 "UUG" = "L", "UCG" = "S", "UAG" = "_", "UGG" = "W",
                 "CUU" = "L", "CCU" = "P", "CAU" = "H", "CGU" = "R",
                 "CUC" = "L", "CCC" = "P", "CAC" = "H", "CGC" = "R",
                 "CUA" = "L", "CCA" = "P", "CAA" = "Q", "CGA" = "R",
                 "CUG" = "L", "CCG" = "P", "CAG" = "Q", "CGG" = "R",
                 "AUU" = "I", "ACU" = "T", "AAU" = "N", "AGU" = "S",
                 "AUC" = "I", "ACC" = "T", "AAC" = "N", "AGC" = "S",
                 "AUA" = "I", "ACA" = "T", "AAA" = "K", "AGA" = "R",
                 "AUG" = "M", "ACG" = "T", "AAG" = "K", "AGG" = "R",
                 "GUU" = "V", "GCU" = "A", "GAU" = "D", "GGU" = "G",
                 "GUC" = "V", "GCC" = "A", "GAC" = "D", "GGC" = "G",
                 "GUA" = "V", "GCA" = "A", "GAA" = "E", "GGA" = "G",
                 "GUG" = "V", "GCG" = "A", "GAG" = "E", "GGG" = "G")

# Make data availble for functions
usethis::use_data(codon_table, overwrite = TRUE, internal = TRUE)

# Make data available for users
usethis::use_data(codon_table, overwrite = TRUE)

usethis::use_r("data")

function1 <- function(name_me2){
  name_me3 <- sample(c("A", "T", "G", "C"), size = name_me2, replace = TRUE)
  name_me4 <- paste0(name_me3, collapse = "")
  return(name_me4)
}

function1(80)

function2 <- function(name_me2){
  name_me3 <- gsub("T", "U", name_me2)
  return(name_me3)
}

function2("ATCGTCATCTATCGTAAAATAGGGGAAGCCTATTTCTGCTGTCAGCACGGGCCCTATGTCATTAACCGCCGGCCAGTATT")


function3 <- function(name_me2, start = 1){
  name_me3 <- nchar(name_me2)
  codons <- substring(name_me2,
                      first = seq(from = start, to = name_me3-3+1, by = 3),
                      last = seq(from = 3+start-1, to = name_me3, by = 3))
  return(codons)
}

output3 <- function3("AUCGUCAUCUAUCGUAAAAUAGGGGAAGCCUAUUUCUGCUGUCAGCACGGGCCCUAUGUCAUUAACCGCCGGCCAGUAUU")

# Translates to AA
function4 <- function(codons){
  name_me2 <- paste0(codon_table[codons], collapse = "")
  return(name_me2)
}

function4(output3)


usethis::use_package("magrittr")
usethis::use_package("ggplot2")
usethis::use_package("stringr")

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

function5("IVIYRKIGEAYFCCQHGPYVINRRPV")

usethis::use_r("function1")
usethis::use_r("function2")
usethis::use_r("function3")
usethis::use_r("function4")
usethis::use_r("function5")




