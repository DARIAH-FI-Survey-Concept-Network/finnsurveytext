#' Compare parts-of-speech
#'
#' Count each POS type for different groups of participants. Data is
#' split based on different values in the `field` column of formatted data.
#' Results will be shown within the plots pane.
#'
#' @param data A dataframe of text in CoNLL-U format with additional `field`
#'  column for splitting data.
#' @param field Column in `data` used for splitting groups
#' @param exclude_nulls Whether to include NULLs in `field` column, default is
#'  `FALSE`
#' @param rename_nulls What to fill NULL values with if `exclude_nulls = FALSE`.
#'
#' @return Table of POS tag counts for the groups.
#' @export
#'
#' @examples
#' fst_pos_compare(fst_child, 'bv1')
#' fst_pos_compare(fst_dev_coop, 'q2')
fst_pos_compare <- function(data,
                            field,
                            exclude_nulls = FALSE,
                            rename_nulls = 'null_data') {
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
  if (exclude_nulls == TRUE) {
    data <- data %>% tidyr::drop_na(field)
  } else {
    data[is.na(data)] <- rename_nulls
  }
  group_data <- data %>% dplyr::group_by_at(field)
  split_data <- dplyr::group_split(group_data)
  names <- dplyr:: group_keys(group_data)
  names(split_data) <- names[[field]]
  names_list <- names[[field]]
  list_of_pos <- list()
  list_of_pos <- append(list_of_pos, list(pos_lookup))
  for (i in 1:length(split_data)) {
    data <- split_data[[i]]
    pos <- fst_pos(data) %>%
      dplyr::rename(!!paste0(as.character(names_list[i]), "-Count") := Count) %>%
      dplyr::rename(!!paste0(as.character(names_list[i]), "-Prop") := Proportion) %>%
      subset(select = -c(UPOS, UPOS_Name) )
    list_of_pos <- append(list_of_pos, list(pos))
  }
  df <- dplyr::bind_cols(list_of_pos)
  df
}



#' Make comparison summary
#'
#' Compare text responses for different groups of participants. Data is
#' split based on different values in the `field` column of formatted data.
#' Results will be shown within the plots pane.
#'
#' @param data A dataframe of text in CoNLL-U format with additional `field`
#'  column for splitting data.
#' @param field Column in `data` used for splitting groups
#' @param exclude_nulls Whether to include NULLs in `field` column, default is
#'  `FALSE`
#' @param rename_nulls What to fill NULL values with if `exclude_nulls = FALSE`.
#'
#' @return Summary table of responses between groups.
#' @export
#'
#' @examples
#' fst_summarise_compare(fst_child, 'bv1')
#' fst_summarise_compare(fst_dev_coop, 'q2')
fst_summarise_compare <- function(data,
                                  field,
                                  exclude_nulls = FALSE,
                                  rename_nulls = 'null_data') {
  if (exclude_nulls == TRUE) {
    data <- data %>% tidyr::drop_na(field)
  } else {
    data[is.na(data)] <- rename_nulls
  }
  group_data <- data %>% dplyr::group_by_at(field)
  split_data <- dplyr::group_split(group_data)
  names <- dplyr:: group_keys(group_data)
  names(split_data) <- names[[field]]
  names_list <- names[[field]]
  list_of_sum <- list()
  for (i in 1:length(split_data)) {
    data <- split_data[[i]]
    sum <- fst_summarise(data, as.character(names_list[i]))
    list_of_sum <- append(list_of_sum, list(sum))
  }
  df <- data.table::rbindlist(list_of_sum)
  df
}



#' Compare response lengths
#'
#' Compare length of text responses for different groups of participants. Data
#' is split based on different values in the `field` column of formatted data.
#' Results will be shown within the plots pane.
#'
#' @param data A dataframe of text in CoNLL-U format with additional `field`
#'  column for splitting data.
#' @param field Column in `data` used for splitting groups
#' @param incl_sentences Whether to include sentence data in table, default is
#'  `TRUE`.
#' @param exclude_nulls Whether to include NULLs in `field` column, default is
#'  `FALSE`
#' @param rename_nulls What to fill NULL values with if `exclude_nulls = FALSE`.
#'
#' @return Dataframe summarising response lengths.
#' @export
#'
#' @examples
#' fst_length_compare(fst_child, 'bv1')
#' fst_length_compare(fst_dev_coop, 'q3', incl_sentences = FALSE)
fst_length_compare <- function(data,
                               field,
                               incl_sentences = TRUE,
                               exclude_nulls = FALSE,
                               rename_nulls = 'null_data') {
  if (exclude_nulls == TRUE) {
    data <- data %>% tidyr::drop_na(field)
  } else {
    data[is.na(data)] <- rename_nulls
  }
  group_data <- data %>% dplyr::group_by_at(field)
  split_data <- dplyr::group_split(group_data)
  names <- dplyr:: group_keys(group_data)
  names(split_data) <- names[[field]]
  names_list <- names[[field]]
  list_of_len <- list()
  for (i in 1:length(split_data)) {
    data <- split_data[[i]]
    len <- fst_length_summary(data, as.character(names_list[i]), incl_sentences = incl_sentences)
    list_of_len <- append(list_of_len, list(len))
  }
  df <- data.table::rbindlist(list_of_len)
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
