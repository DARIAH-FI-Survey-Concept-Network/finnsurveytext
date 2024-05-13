#' Get unique n-grams
#'
#' Takes at least two tables of n-grams and frequencies (either output of
#' `fst_freq_table()` or `fst_ngrams_table()`) and finds n-grams unique to
#'  one table.
#'
#' @param table1 The first table.
#' @param table2 The second table.
#' @param ... Any other tables you want to include.
#'
#' @return Dataframe of words and whether word is unique or not.
#' @export
#'
#' @examples
#' top_child <- fst_freq_table(fst_child)
#' top_dev <- fst_freq_table(fst_dev_coop)
#' fst_get_unique_ngrams(top_child, top_dev)
fst_get_unique_ngrams_OLD <- function(table1, table2, ...) {
  df <- rbind(table1, table2, ...)
  df <- df %>%
    dplyr::mutate(n = 1) %>%
    dplyr::group_by(words) %>%
    dplyr::summarise(n = sum(n)) %>%
    dplyr::mutate(n = ifelse(n == 1, "yes", "no")) %>%
    dplyr::rename(unique_word = n)
  df
}

fst_get_unique_ngrams <- function(list_of_top_words) {
  df <- data.table::rbindlist(list_of_top_words) #this is the row that is changed
  df <- df %>%
    dplyr::mutate(n = 1) %>%
    dplyr::group_by(words) %>%
    dplyr::summarise(n = sum(n)) %>%
    dplyr::mutate(n = ifelse(n == 1, "yes", "no")) %>%
    dplyr::rename(unique_word = n)
  df
}


#' Merge N-grams table with unique words
#'
#' Merges list of unique words from `fst_get_unique_ngrams()` with output of
#' `fst_freq_table()` or `fst_ngrams_table()` so that unique words can be
#' displayed on comparison plots.
#'
#' @param table Output of `fst_freq_table()` or `fst_ngrams_table()`.
#' @param unique_table Output of `fst_get_unique_ngrams()`.
#'
#' @return A table of top n-grams, frequency, and whether the n-gram is
#'  "unique".
#' @export
#'
#' @examples
#' top_child <- fst_freq_table(fst_child)
#' top_dev <- fst_freq_table(fst_dev_coop)
#' unique_words <- fst_get_unique_ngrams(top_child, top_dev)
#' fst_join_unique(top_child, unique_words)
#' fst_join_unique(top_dev, unique_words)
fst_join_unique <- function(table, unique_table) {
  table <- table %>% dplyr::left_join(unique_table, by = "words")
  table
}

#' Plot comparison n-grams
#'
#' Plots frequency n-grams with unique n-grams highlighted.
#'
#' @param table The table of n-grams, output of `get_unique_ngrams()`.
#' @param number The number of n-grams, default is `10`.
#' @param ngrams The type of n-grams, default is `1`.
#' @param unique_colour Colour to display unique words, default is `"indianred"`.
#' @param name An optional "name" for the plot, default is `NULL`.
#' @param override_title An optional title to override the automatic one for
#'  the plot. Default is `NULL`. If `NULL`, title of plot will be `number` "Most
#'  Common 'Term'". 'Term' is "Words", "Bigrams", or "N-Grams" where N > 2.
#'
#' @return Plot of top n-grams with unique terms highlighted.
#' @export
#'
#' @examples
#' top_child <- fst_freq_table(fst_child)
#' top_dev <- fst_freq_table(fst_dev_coop)
#' unique_words <- fst_get_unique_ngrams(top_child, top_dev)
#' top_child_u <- fst_join_unique(top_child, unique_words)
#' top_dev_u <- fst_join_unique(top_dev, unique_words)
#' fst_ngrams_compare_plot(top_child_u, ngrams = 1, name = "Child")
#' fst_ngrams_compare_plot(top_dev_u, ngrams = 1, name = "Dev")
fst_ngrams_compare_plot <- function(table, number = 10, ngrams = 1, unique_colour = "indianred", name = NULL, override_title = NULL) {
  colours <- c("yes" = unique_colour, "no" = "grey50")
  if (ngrams == 1) {
    term <- "Words"
  } else if (ngrams == 2) {
    term <- "Bigrams"
  } else {
    term <- paste0(as.character(ngrams), "-grams")
  }
  if (is.null(override_title)) {
    table %>%
      ggplot2::ggplot(ggplot2::aes(occurrence, words, fill = unique_word)) +
      ggplot2::geom_col() +
      ggplot2::scale_fill_manual(values = colours, guide = "none") +
      ggplot2::labs(y = NULL, title = paste(name, as.character(number), "Most Common", term))
  } else {
    table %>%
      ggplot2::ggplot(ggplot2::aes(occurrence, words, fill = unique_word)) +
      ggplot2::geom_col() +
      ggplot2::scale_fill_manual(values = colours, guide = "none") +
      ggplot2::labs(y = NULL, title = override_title)
  }
}


#' Compare and plot top words
#'
#' Find top and unique top words for different groups of participants. Data is
#' split based on different values in the `field` column of formatted data.
#' Results will be shown within the plots pane.
#'
#' @param data A dataframe of text in CoNLL-U format with additional `field`
#'  column for splitting data.
#' @param field Column in `data` used for splitting groups
#' @param data4 An optional dataframe of text in CoNLL-U format for the fourth
#' @param number The number of n-grams to return, default is `10`.
#' @param norm The method for normalising the data. Valid settings are
#'  `"number_words"` (the number of words in the responses), `"number_resp"`
#'  (the number of responses), or `NULL` (raw count returned, default, also used
#'  when weights are applied).
#' @param pos_filter List of UPOS tags for inclusion, default is `NULL` which
#'  means all word types included.
#' @param strict Whether to strictly cut-off at `number` (ties are
#'  alphabetically ordered), default is `TRUE`.
#' @param use_svydesign_weights Option to weight words in the wordcloud using
#'  weights from  a svydesign object containing the raw data, default is `FALSE`
#' @param id ID column from raw data, required if `use_svydesign_weights = TRUE`
#'  and must match the `docid` in formatted `data`.
#' @param svydesign A svydesign object which contains the raw data and weights.
#' @param use_column_weights Option to weight words in the wordcloud using
#'  weights from  formatted data which includes addition `weight` column,
#'  default is `FALSE`
#' @param exclude_nulls Whether to include NULLs in `field` column, default is
#'  `FALSE`
#' @param rename_nulls What to fill NULL values with if `exclude_nulls = FALSE`.
#' @param unique_colour Colour to display unique words, default is
#'  `"indianred"`.
#'
#' @return Plots of most frequent words in the plots pane with unique words
#'  highlighted.
#' @export
#'
#' @examples
#' fst_freq_compare(fst_child, 'bv1', number = 10, norm = "number_resp")
#' fst_freq_compare(fst_child, 'bv1', number = 10, norm = NULL)
#' fst_child_3 <- within(fst_child, rm(weight))
#' fst_freq_compare(fst_child_3, 'bv1', number = 10, use_svydesign_weights = TRUE, id = 'fsd_id', svydesign = svy_child)
#' fst_freq_compare(fst_child, 'bv1', number = 10, use_column_weights = TRUE, strict = FALSE, unique_colour = 'purple')
fst_freq_compare <- function(data,
                             field,
                             number = 10,
                             norm = NULL,
                             pos_filter = NULL,
                             strict = TRUE,
                             use_svydesign_weights = FALSE,
                             id = "",
                             svydesign = NULL,
                             use_column_weights = FALSE,
                             exclude_nulls = FALSE,
                             rename_nulls = 'null_data',
                             unique_colour = "indianred") {
    if (exclude_nulls == TRUE) {
      data <- data %>% tidyr::drop_na(field)
    } else {
      data[is.na(data)] <- rename_nulls
    }
    group_data <- data %>% dplyr::group_by_at(field)
    split_data <- dplyr::group_split(group_data)
    names <- dplyr:: group_keys(group_data)
    names(split_data) <- names[[field]]
    list_of_top_words <- list()
    for (i in 1:length(split_data)) {
      data <- split_data[[i]]
      top_words <- fst_ngrams_table2(data,
                                     number = number,
                                     ngrams = 1,
                                     norm = norm,
                                     pos_filter = pos_filter,
                                     strict = strict,
                                     use_svydesign_weights = use_svydesign_weights,
                                     id = id,
                                     svydesign = svydesign,
                                     use_column_weights = use_column_weights)
      list_of_top_words <- append(list_of_top_words, list(top_words))
    }
    names(list_of_top_words) <- names[[field]]
    unique <- fst_get_unique_ngrams(list_of_top_words)
    list_of_top_words_tables <- list()
    for (i in 1:length(list_of_top_words)) {
      table <- list_of_top_words[[i]]
      table <- fst_join_unique(table, unique)
      list_of_top_words_tables <- append(list_of_top_words_tables, list(table))
    }
    names(list_of_top_words_tables) <- names[[field]]
    names2 <- names[[field]]
    n <- length(list_of_top_words_tables)
    list_of_plots <- vector('list', n)
    num_rows <- ceiling(n/3)
    for (i in 1:n) {
      table <- list_of_top_words_tables[[i]]
      list_of_plots[[i]] <- fst_ngrams_compare_plot(table,
                                                    number = number,
                                                    ngrams = 1,
                                                    unique_colour = unique_colour,
                                                    override_title = paste(field, '=', names2[i]))
    }
    # list_of_plots
    do.call(gridExtra::grid.arrange, list_of_plots)
  }

#' Compare and plot top n-grams
#'
#' Find top and unique top n-grams for different groups of participants. Data is
#' split based on different values in the `field` column of formatted data.
#' Results will be shown within the plots pane.
#'
#' @param data A dataframe of text in CoNLL-U format with additional `field`
#'  column for splitting data.
#' @param field Column in `data` used for splitting groups
#' @param data4 An optional dataframe of text in CoNLL-U format for the fourth
#' @param number The number of n-grams to return, default is `10`.
#' @param ngrams The type of n-grams to return, default is `1`.
#' @param norm The method for normalising the data. Valid settings are
#'  `"number_words"` (the number of words in the responses), `"number_resp"`
#'  (the number of responses), or `NULL` (raw count returned, default, also used
#'  when weights are applied).
#' @param pos_filter List of UPOS tags for inclusion, default is `NULL` which
#'  means all word types included.
#' @param strict Whether to strictly cut-off at `number` (ties are
#'  alphabetically ordered), default is `TRUE`.
#' @param use_svydesign_weights Option to weight words in the wordcloud using
#'  weights from  a svydesign object containing the raw data, default is `FALSE`
#' @param id ID column from raw data, required if `use_svydesign_weights = TRUE`
#'  and must match the `docid` in formatted `data`.
#' @param svydesign A svydesign object which contains the raw data and weights.
#' @param use_column_weights Option to weight words in the wordcloud using
#'  weights from  formatted data which includes addition `weight` column,
#'  default is `FALSE`
#' @param exclude_nulls Whether to include NULLs in `field` column, default is
#'  `FALSE`
#' @param rename_nulls What to fill NULL values with if `exclude_nulls = FALSE`.
#' @param unique_colour Colour to display unique words, default is
#'  `"indianred"`.
#'
#' @return Plots of top n-grams in the plots pane with unique n-grams
#'  highlighted.
#' @export
#'
#' @examples
#' fst_ngrams_compare(fst_child, 'bv1', ngrams = 4, number = 10, norm = "number_resp")
#' fst_ngrams_compare(fst_child, 'bv1', ngrams = 2, number = 10, norm = NULL)
#' fst_ngrams_compare <- within(fst_child, rm(weight))
#' fst_ngrams_compare(fst_child_3, 'bv1', number = 10, ngrams = 3, use_svydesign_weights = TRUE, id = 'fsd_id', svydesign = svy_child)
#' fst_ngrams_compare(fst_child, 'bv1', number = 10, ngrams = 3, use_column_weights = TRUE, strict = FALSE, unique_colour = 'purple')
fst_ngrams_compare <- function(data,
                              field,
                              number = 10,
                              ngrams = 1,
                              norm = NULL,
                              pos_filter = NULL,
                              strict = TRUE,
                              use_svydesign_weights = FALSE,
                              id = "",
                              svydesign = NULL,
                              use_column_weights = FALSE,
                              exclude_nulls = FALSE,
                              rename_nulls = 'null_data',
                              unique_colour = "indianred") {
  if (exclude_nulls == TRUE) {
    data <- data %>% tidyr::drop_na(field)
  } else {
    data[is.na(data)] <- rename_nulls
  }
  group_data <- data %>% dplyr::group_by_at(field)
  split_data <- dplyr::group_split(group_data)
  names <- dplyr:: group_keys(group_data)
  names(split_data) <- names[[field]]
  list_of_top_words <- list()
  for (i in 1:length(split_data)) {
    data <- split_data[[i]]
    top_words <- fst_ngrams_table2(data,
                                   number = number,
                                   ngrams = ngrams,
                                   norm = norm,
                                   pos_filter = pos_filter,
                                   strict = strict,
                                   use_svydesign_weights = use_svydesign_weights,
                                   id = id,
                                   svydesign = svydesign,
                                   use_column_weights = use_column_weights)
    list_of_top_words <- append(list_of_top_words, list(top_words))
  }
  names(list_of_top_words) <- names[[field]]
  unique <- fst_get_unique_ngrams(list_of_top_words)
  list_of_top_words_tables <- list()
  for (i in 1:length(list_of_top_words)) {
    table <- list_of_top_words[[i]]
    table <- fst_join_unique(table, unique)
    list_of_top_words_tables <- append(list_of_top_words_tables, list(table))
  }
  names(list_of_top_words_tables) <- names[[field]]
  names2 <- names[[field]]
  n <- length(list_of_top_words_tables)
  list_of_plots <- vector('list', n)
  num_rows <- ceiling(n/3)
  for (i in 1:n) {
    table <- list_of_top_words_tables[[i]]
    list_of_plots[[i]] <- fst_ngrams_compare_plot(table,
                                                  number = number,
                                                  ngrams = ngrams,
                                                  unique_colour = unique_colour,
                                                  override_title = paste(field, '=', names2[i]))
  }
  # list_of_plots
  do.call(gridExtra::grid.arrange, list_of_plots)
}

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
