#Replace fst_rm_stop_punct with version where you can manually provide a list. '
# New stopword list is a list of strings
proposed_fst_rm_stop_punct <- function(data, stopword_list = "nltk", manual = FALSE, manual_list = list()) {
  if (manual == FALSE) {
    swords <- stopwords::stopwords("fi", stopword_list)
  } else {
    if (stringr::str_detect(manual_list, ",")) {
      manual_list <- manual_list %>%
        lapply(tolower) %>%
        stringr::str_extract_all(pattern = "\\w+") %>%
        unlist()
    } else {
      manual_list <- manual_list %>%
        lapply(tolower)
    }
    swords <- manual_list
  }
  output <- data %>%
    dplyr::mutate(lemma = stringr::str_replace(.data$lemma, "#", "")) %>%
    dplyr::filter(!.data$lemma %in% swords) %>%
    dplyr::filter(.data$upos != "PUNCT")
  output
}


