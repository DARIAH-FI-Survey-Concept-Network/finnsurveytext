#' Compare parts-of-speech
#'
#' Compare words in responses based on part-of-speech tagging for between 2 and
#' 4 sets of prepared data.
#'
#' @param data1 A dataframe of text in CoNLL-U format for the first group.
#' @param data2 A dataframe of text in CoNLL-U format for the second group.
#' @param data3 An optional dataframe of text in CoNLL-U format for the third
#'  group, default is `NULL`.
#' @param data4 An optional dataframe of text in CoNLL-U format for the fourth
#'  group, default is `NULL`.
#' @param name1 An optional "name" for the first group, default is `"Group 1"`.
#' @param name2 An optional "name" for the second group, default is `"Group 2"`.
#' @param name3 An optional "name" for the third group, default is `"Group 3"`.
#' @param name4 An optional "name" for the fourth group, default is `"Group 4"`.
#'
#' @return Table of POS tag counts for the groups.
#' @export
#'
#' @examples
#' f <- conllu_dev_q11_1_f_nltk
#' m <- conllu_dev_q11_1_m_nltk
#' na <- conllu_dev_q11_1_na_nltk
#' all <- conllu_dev_q11_1_nltk
#' fst_pos_compare(f, m, na, all, "Female", "Male", "Not Spec.", "All")
#' fst_pos_compare(f, m, name1 = "Female", name2 = "Male")
fst_pos_compare <- function(data1, data2, data3 = NULL, data4 = NULL, name1 = "Group 1", name2 = "Group 2", name3 = "Group 3", name4 = "Group 4") {
  pos_lookup <- data.frame(
    "UPOS" = c(
      "ADJ", "ADP", "ADV", "AUX", "CCONJ", "DET",
      "INTJ", "NOUN", "NUM", "PART", "PRON",
      "PROPN", "PUNCT", "SCONJ", "SYM", "VERB",
      "X"
    ),
    "Part_of_Speech_Name" = c(
      " adjective", " adposition",
      " adverb", " auxiliary",
      " coordinating conjunction",
      " determiner", " interjection",
      " noun", " numeral", " particle",
      " pronoun", " proper noun",
      " punctuation",
      " subordinating conjunction",
      " symbol", " verb", " other"
    )
  )
  if (!is.null(data3)) {
    if (!is.null(data4)) {
      denom4 <- nrow(data4)
      name4_2 <- paste(name4, "Count")
      name4_3 <- paste(name4, "Prop")
      pos_table4 <- data4 %>%
        dplyr::count(upos, sort = TRUE) %>%
        dplyr::mutate(!!name4_3 := round(n / denom4, 3)) %>%
        dplyr::rename(!!name4_2 := n) %>%
        dplyr::rename(UPOS = upos)
      denom3 <- nrow(data3)
      name3_2 <- paste(name3, "Count")
      name3_3 <- paste(name3, "Prop")
      pos_table3 <- data3 %>%
        dplyr::count(upos, sort = TRUE) %>%
        dplyr::mutate(!!name3_3 := round(n / denom3, 3)) %>%
        dplyr::rename(!!name3_2 := n) %>%
        dplyr::rename(UPOS = upos)
      denom2 <- nrow(data2)
      name2_2 <- paste(name2, "Count")
      name2_3 <- paste(name2, "Prop")
      pos_table2 <- data2 %>%
        dplyr::count(upos, sort = TRUE) %>%
        dplyr::mutate(!!name2_3 := round(n / denom2, 3)) %>%
        dplyr::rename(!!name2_2 := n) %>%
        dplyr::rename(UPOS = upos)
      denom1 <- nrow(data1)
      name1_2 <- paste(name1, "Count")
      name1_3 <- paste(name1, "Prop")
      pos_table1 <- data1 %>%
        dplyr::count(upos, sort = TRUE) %>%
        dplyr::mutate(!!name1_3 := round(n / denom1, 3)) %>%
        dplyr::rename(!!name1_2 := n) %>%
        dplyr::rename(UPOS = upos)
      df <- merge(x = pos_lookup, y = pos_table1, by = "UPOS") %>%
        merge(pos_table2, by = "UPOS") %>%
        merge(pos_table3, by = "UPOS") %>%
        merge(pos_table4, by = "UPOS")
    } else {
      denom3 <- nrow(data3)
      name3_2 <- paste(name3, "Count")
      name3_3 <- paste(name3, "Prop")
      pos_table3 <- data3 %>%
        dplyr::count(upos, sort = TRUE) %>%
        dplyr::mutate(!!name3_3 := round(n / denom3, 3)) %>%
        dplyr::rename(!!name3 := n) %>%
        dplyr::rename(UPOS = upos)
      denom2 <- nrow(data2)
      name2_2 <- paste(name2, "Count")
      name2_3 <- paste(name2, "Prop")
      pos_table2 <- data2 %>%
        dplyr::count(upos, sort = TRUE) %>%
        dplyr::mutate(!!name2_3 := round(n / denom2, 3)) %>%
        dplyr::rename(!!name2_2 := n) %>%
        dplyr::rename(UPOS = upos)
      denom1 <- nrow(data1)
      name1_2 <- paste(name1, "Count")
      name1_3 <- paste(name1, "Prop")
      pos_table1 <- data1 %>%
        dplyr::count(upos, sort = TRUE) %>%
        dplyr::mutate(!!name1_3 := round(n / denom1, 3)) %>%
        dplyr::rename(!!name1_2 := n) %>%
        dplyr::rename(UPOS = upos)
      df <- merge(x = pos_lookup, y = pos_table1, by = "UPOS") %>%
        merge(pos_table2, by = "UPOS") %>%
        merge(pos_table3, by = "UPOS")
    }
  } else {
    denom2 <- nrow(data2)
    name2_2 <- paste(name2, "Count")
    name2_3 <- paste(name2, "Prop")
    pos_table2 <- data2 %>%
      dplyr::count(upos, sort = TRUE) %>%
      dplyr::mutate(!!name2_3 := round(n / denom2, 3)) %>%
      dplyr::rename(!!name2_2 := n) %>%
      dplyr::rename(UPOS = upos)
    denom1 <- nrow(data1)
    name1_2 <- paste(name1, "Count")
    name1_3 <- paste(name1, "Prop")
    pos_table1 <- data1 %>%
      dplyr::count(upos, sort = TRUE) %>%
      dplyr::mutate(!!name1_3 := round(n / denom1, 3)) %>%
      dplyr::rename(!!name1_2 := n) %>%
      dplyr::rename(UPOS = upos)
    df <- merge(x = pos_lookup, y = pos_table1, by = "UPOS") %>%
      merge(pos_table2, by = "UPOS")
  }
  df
}



#' Make comparison summary
#'
#' Compare text responses for between 2 and 4 sets of prepared data.
#'
#' @param data1 A dataframe of text in CoNLL-U format for the first group.
#' @param data2 A dataframe of text in CoNLL-U format for the second group.
#' @param data3 An optional dataframe of text in CoNLL-U format for the third
#'  group, default is `NULL`.
#' @param data4 An optional dataframe of text in CoNLL-U format for the fourth
#'  group, default is `NULL`.
#' @param name1 A string describing data1, default is `"Group 1"`.
#' @param name2 A string describing data2, default is `"Group 2"`.
#' @param name3 A string describing data3, default is `"Group 3"`.
#' @param name4 A string describing data4, default is `"Group 4"`.
#'
#' @return Summary table of responses between groups.
#' @export
#'
#' @examples
#' f <- conllu_dev_q11_1_f_nltk
#' m <- conllu_dev_q11_1_m_nltk
#' na <- conllu_dev_q11_1_na_nltk
#' all <- conllu_dev_q11_1_nltk
#' fst_summarise_compare(m, f, na, all, "Male", "Female", "Not Spec.", "All")
#' fst_summarise_compare(m, f, name1 = "Male", name2 = "Female")
fst_summarise_compare <- function(data1, data2, data3 = NULL, data4 = NULL, name1 = "Group 1", name2 = "Group 2", name3 = "Group 3", name4 = "Group 4") {
  if (!is.null(data3)) {
    if (!is.null(data4)) {
      sum4 <- fst_summarise(data4, name4)
      sum3 <- fst_summarise(data3, name3)
      sum2 <- fst_summarise(data2, name2)
      sum1 <- fst_summarise(data1, name1)
      df <- rbind(sum1, sum2, sum3, sum4)
    } else {
      sum3 <- fst_summarise(data3, name3)
      sum2 <- fst_summarise(data2, name2)
      sum1 <- fst_summarise(data1, name1)
      df <- rbind(sum1, sum2, sum3)
    }
  } else {
    sum2 <- fst_summarise(data2, name2)
    sum1 <- fst_summarise(data1, name1)
    df <- rbind(sum1, sum2)
  }
  df
}



#' Compare response lengths
#'
#' Compare length of text responses for between 2 and 4 sets of prepared data.
#'
#' @param data1 A dataframe of text in CoNLL-U format for the first group.
#' @param data2 A dataframe of text in CoNLL-U format for the second group.
#' @param data3 An optional dataframe of text in CoNLL-U format for the third
#'  group, default is `NULL`.
#' @param data4 An optional dataframe of text in CoNLL-U format for the fourth
#'  group, default is `NULL`.
#' @param name1 A string describing data1, default is `"Group 1"`.
#' @param name2 A string describing data2, default is `"Group 2"`.
#' @param name3 A string describing data3, default is `"Group 3"`.
#' @param name4 A string describing data4, default is `"Group 4"`.
#' @param incl_sentences Whether to include sentence data in table, default is
#'  `TRUE`.
#'
#' @return Dataframe summarising response lengths.
#' @export
#'
#' @examples
#' f <- conllu_dev_q11_1_f_nltk
#' m <- conllu_dev_q11_1_m_nltk
#' na <- conllu_dev_q11_1_na_nltk
#' all <- conllu_dev_q11_1_nltk
#' fst_length_compare(f, m, na, all, "Female", "Male", "Not Spec", "All")
#' fst_length_compare(f, m, name1 = "F", name2 = "M", incl_sentences = FALSE)
fst_length_compare <- function(data1, data2, data3 = NULL, data4 = NULL, name1 = "Group 1", name2 = "Group 2", name3 = "Group 3", name4 = "Group 4", incl_sentences = TRUE) {
  if (!is.null(data3)) {
    if (!is.null(data4)) {
      sum4 <- fst_length_summary(data4, name4, incl_sentences = incl_sentences)
      sum3 <- fst_length_summary(data3, name3, incl_sentences = incl_sentences)
      sum2 <- fst_length_summary(data2, name2, incl_sentences = incl_sentences)
      sum1 <- fst_length_summary(data1, name1, incl_sentences = incl_sentences)
      df <- rbind(sum1, sum2, sum3, sum4)
    } else {
      sum3 <- fst_length_summary(data3, name3, incl_sentences = incl_sentences)
      sum2 <- fst_length_summary(data2, name2, incl_sentences = incl_sentences)
      sum1 <- fst_length_summary(data1, name1, incl_sentences = incl_sentences)
      df <- rbind(sum1, sum2, sum3)
    }
  } else {
    sum2 <- fst_length_summary(data2, name2, incl_sentences = incl_sentences)
    sum1 <- fst_length_summary(data1, name1, incl_sentences = incl_sentences)
    df <- rbind(sum1, sum2)
  }
  df
}




#' Make comparison cloud
#'
#' Creates a comparison wordcloud showing words that occur differently between
#' each group.
#'
#' @param data1 A dataframe of text in CoNLL-U format for the first group.
#' @param data2 A dataframe of text in CoNLL-U format for the second group.
#' @param data3 An optional dataframe of text in CoNLL-U format for the third
#'  group, default is `NULL`.
#' @param data4 An optional dataframe of text in CoNLL-U format for the fourth
#'  group, default is `NULL`.
#' @param name1 A string describing data1, default is `Group 1`.
#' @param name2 A string describing data2, default is `Group 2`.
#' @param name3 A string describing data3, default is `Group 3`.
#' @param name4 A string describing data4, default is `Group 4`.
#' @param pos_filter List of UPOS tags for inclusion, default is `NULL` which
#'  means all word types included.
#' @param max The maximum number of words to display, default is `100`.
#'
#' @return A comparison cloud from wordcloud package.
#' @export
#'
#' @examples
#' d1 <- conllu_dev_q11_1_nltk
#' d2 <- conllu_dev_q11_3_nltk
#' pf1 <- c("NOUN", "VERB", "ADJ", "ADV")
#' fst_comparison_cloud(d1, d2, pos_filter = pf1)
#'
#' f <- conllu_dev_q11_1_f_nltk
#' m <- conllu_dev_q11_1_m_nltk
#' na <- conllu_dev_q11_1_na_nltk
#' n1 <- "Female"
#' n2 <- "Male"
#' n3 <- "NA"
#' fst_comparison_cloud(f, m, na, name1 = n1, name2 = n2, name3 = n3, max = 400)
#' fst_comparison_cloud(f, m, na, name1 = n1, name2 = n2, name3 = n3, max = 100)
fst_comparison_cloud <- function(data1, data2, data3 = NULL, data4 = NULL, name1 = "Group 1", name2 = "Group 2", name3 = "Group 3", name4 = "Group 4", pos_filter = NULL, max = 100) {
  message("Notes on use of fst_comparison_cloud: \n If `max` is large, you may receive \"warnings\" indicating any words which are not plotted due to space constraints.\n\n")
  num1 <- dplyr::n_distinct(data1$doc_id)
  num2 <- dplyr::n_distinct(data2$doc_id)
  if (!is.null(data3)) {
    num3 <- dplyr::n_distinct(data3$doc_id)
    if (!is.null(data4)) {
      num4 <- dplyr::n_distinct(data4$doc_id)
      message(paste0("Note: \n Consider whether your data is balanced between groups being compared and whether each group contains enough data for analysis. \n The number of responses in each group (including \'NAs\') are listed below: \n\t", name1, "=", num1, ", ", name2, "=", num2, ", ", name3, "=", num3, ", ", name4, "=", num4, "\n\n"))
      if (!is.null(pos_filter)) {
        data1 <- dplyr::filter(data1, upos %in% pos_filter)
        data2 <- dplyr::filter(data2, upos %in% pos_filter)
        data3 <- dplyr::filter(data3, upos %in% pos_filter)
        data4 <- dplyr::filter(data4, upos %in% pos_filter)
      }
      data1 <- data1 %>%
        dplyr::filter(.data$dep_rel != "punct") %>%
        dplyr::filter(!is.na(lemma)) %>%
        dplyr::filter(lemma != "na") %>%
        dplyr::count(lemma, sort = TRUE) %>%
        dplyr::rename(!!name1 := n)
      data2 <- data2 %>%
        dplyr::filter(.data$dep_rel != "punct") %>%
        dplyr::filter(!is.na(lemma)) %>%
        dplyr::filter(lemma != "na") %>%
        dplyr::count(lemma, sort = TRUE) %>%
        dplyr::rename(!!name2 := n)
      data3 <- data3 %>%
        dplyr::filter(.data$dep_rel != "punct") %>%
        dplyr::filter(!is.na(lemma)) %>%
        dplyr::filter(lemma != "na") %>%
        dplyr::count(lemma, sort = TRUE) %>%
        dplyr::rename(!!name3 := n)
      data4 <- data4 %>%
        dplyr::filter(.data$dep_rel != "punct") %>%
        dplyr::filter(!is.na(lemma)) %>%
        dplyr::filter(lemma != "na") %>%
        dplyr::count(lemma, sort = TRUE) %>%
        dplyr::rename(!!name4 := n)
      compcloud_data <- dplyr::full_join(data1, data2, by = "lemma") %>%
        dplyr::full_join(data3, by = "lemma") %>%
        dplyr::full_join(data4, by = "lemma")
    } else {
      message(paste0("Note: \n Consider whether your data is balanced between groups being compared and whether each group contains enough data for analysis. \n The number of responses in each group (including \'NAs\') are listed below: \n\t", name1, "=", num1, ", ", name2, "=", num2, ", ", name3, "=", num3, "\n\n"))
      if (!is.null(pos_filter)) {
        data1 <- dplyr::filter(data1, upos %in% pos_filter)
        data2 <- dplyr::filter(data2, upos %in% pos_filter)
        data3 <- dplyr::filter(data3, upos %in% pos_filter)
      }
      data1 <- data1 %>%
        dplyr::filter(.data$dep_rel != "punct") %>%
        dplyr::filter(!is.na(lemma)) %>%
        dplyr::filter(lemma != "na") %>%
        dplyr::count(lemma, sort = TRUE) %>%
        dplyr::rename(!!name1 := n)
      data2 <- data2 %>%
        dplyr::filter(.data$dep_rel != "punct") %>%
        dplyr::filter(!is.na(lemma)) %>%
        dplyr::filter(lemma != "na") %>%
        dplyr::count(lemma, sort = TRUE) %>%
        dplyr::rename(!!name2 := n)
      data3 <- data3 %>%
        dplyr::filter(.data$dep_rel != "punct") %>%
        dplyr::filter(!is.na(lemma)) %>%
        dplyr::filter(lemma != "na") %>%
        dplyr::count(lemma, sort = TRUE) %>%
        dplyr::rename(!!name3 := n)
      compcloud_data <- dplyr::full_join(data1, data2, by = "lemma")
      compcloud_data <- dplyr::full_join(compcloud_data, data3, by = "lemma")
    }
  } else {
    message(paste0("Note: \n Consider whether your data is balanced between groups being compared and whether each group contains enough data for analysis. \n The number of responses in each group (including \'NAs\') are listed below: \n\t", name1, "=", num1, ", ", name2, "=", num2, "\n\n"))
    if (!is.null(pos_filter)) {
      data1 <- dplyr::filter(data1, upos %in% pos_filter)
      data2 <- dplyr::filter(data2, upos %in% pos_filter)
    }
    data1 <- data1 %>%
      dplyr::filter(.data$dep_rel != "punct") %>%
      dplyr::filter(!is.na(lemma)) %>%
      dplyr::filter(lemma != "na") %>%
      dplyr::count(lemma, sort = TRUE) %>%
      dplyr::rename(!!name1 := n)
    data2 <- data2 %>%
      dplyr::filter(.data$dep_rel != "punct") %>%
      dplyr::filter(!is.na(lemma)) %>%
      dplyr::filter(lemma != "na") %>%
      dplyr::count(lemma, sort = TRUE) %>%
      dplyr::rename(!!name2 := n)
    compcloud_data <- dplyr::full_join(data1, data2, by = "lemma")
  }
  rownames(compcloud_data) <- compcloud_data$lemma
  compcloud_data$lemma <- NULL
  compcloud_data[is.na(compcloud_data)] <- 0
  wordcloud::comparison.cloud(compcloud_data,
                              max.words = max,
                              random.order = FALSE,
                              rot.per = 0.35,
                              colors = RColorBrewer::brewer.pal(8, "Dark2")
  )
}
