# use_svydesign_weights = FALSE,
# id = "",
# svydesign = NULL,
# use_column_weights = FALSE

fst_length_summary <- function(data,
                               desc = "All respondents",
                               incl_sentences = TRUE,
                               use_svydesign_weights = FALSE,
                               id = "",
                               svydesign = NULL,
                               use_column_weights = FALSE) {
  if (use_svydesign_weights == TRUE) {
    data <- fst_use_svydesign(data = data, svydesign = svydesign, id = id)
  }
  no_resp_count <- length(which(data$sentence %in% c("NA", "na")))
  if (use_svydesign_weights == TRUE) {
    #DO THINGS
  } else if (use_column_weights == TRUE) {
    data2 <- dplyr::select(data, doc_id, sentence, weight) %>%
      dplyr::mutate(length = stringr::str_count(sentence, "\\w+")/ weight) %>%
      dplyr::filter(!is.na(sentence)) %>%
      dplyr::filter(sentence != "na") %>%
      dplyr::filter(sentence != "NA")
    data <- data[!duplicated(data), ] %>%
      dplyr::group_by(doc_id) %>%
      dplyr::summarise(
        number_sentences = dplyr::n(),
        number_of_words = sum(length)
      )
  } else {
    data <- dplyr::select(data, doc_id, sentence) %>%
      dplyr::mutate(length = stringr::str_count(sentence, "\\w+")) %>%
      dplyr::filter(!is.na(sentence)) %>%
      dplyr::filter(sentence != "na") %>%
      dplyr::filter(sentence != "NA")
    data <- data[!duplicated(data), ] %>%
      dplyr::group_by(doc_id) %>%
      dplyr::summarise(
        number_sentences = dplyr::n(),
        number_of_words = sum(length)
      )
  }
  word_df <- data %>%
    dplyr::summarize(
      "Description" = paste0(desc, "- Words"),
      "Respondents" = dplyr::n_distinct(doc_id),
      "Mean" = mean(data$number_of_words),
      "Minimum" = min(data$number_of_words),
      "Q1" = quantile(data$number_of_words, 0.25),
      "Median" = median(data$number_of_words),
      "Q3" = quantile(data$number_of_words, 0.75),
      "Maximum" = max(data$number_of_words)
    )
  if (incl_sentences == TRUE) {
    sentence_df <- data %>%
      dplyr::summarize(
        "Description" = paste0(desc, "- Sentences"),
        "Respondents" = dplyr::n_distinct(doc_id),
        "Mean" = mean(data$number_sentences),
        "Minimum" = min(data$number_sentences),
        "Q1" = quantile(data$number_sentences, 0.25),
        "Median" = median(data$number_sentences),
        "Q3" = quantile(data$number_sentences, 0.75),
        "Maximum" = max(data$number_sentences)
      )
    word_df <- rbind(word_df, sentence_df)
  }
  word_df
}
