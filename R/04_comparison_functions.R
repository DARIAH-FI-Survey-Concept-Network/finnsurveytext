#' Get unique n-grams from separate top n-grams tables
#'
#' Takes at least two separate tables of n-grams and frequencies (either output
#' of `fst_freq_table()` or `fst_ngrams_table()`) and finds n-grams unique to
#' one table.
#'
#' @param table1 The first n-grams table.
#' @param table2 The second n-grams table.
#' @param ... Any other n-grams tables you want to include.
#'
#' @return Dataframe of words and whether word is unique or not.
#' @export
#'
#' @examples
#' top_child <- fst_freq_table(fst_child)
#' top_dev <- fst_freq_table(fst_dev_coop)
#' fst_get_unique_ngrams_separate(top_child, top_dev)
fst_get_unique_ngrams_separate <- function(table1, table2, ...) {
  df <- rbind(table1, table2, ...)
  df <- df %>%
    dplyr::mutate(n = 1) %>%
    dplyr::group_by(words) %>%
    dplyr::summarise(n = sum(n)) %>%
    dplyr::mutate(n = ifelse(n == 1, "Unique", "Common")) %>%
    dplyr::rename(word_type = n)
  df
}

#' Get unique n-grams from a list of top n-grams tables
#'
#' Takes a list containing at least two tables of n-grams and frequencies
#' (either output of `fst_freq_table()` or `fst_ngrams_table()`) and finds
#' n-grams unique to one table.
#'
#' @param list_of_top_ngrams A list of top ngrams
#'
#' @return Dataframe of words and whether word is unique or not.
#' @export
#'
#' @examples
#' top_child <- fst_freq_table(fst_child)
#' top_dev <- fst_freq_table(fst_dev_coop)
#' list_of_top_words <- list()
#' list_of_top_words <- append(list_of_top_words, list(top_child))
#' list_of_top_words <- append(list_of_top_words, list(top_dev))
#' fst_get_unique_ngrams(list_of_top_words)
fst_get_unique_ngrams <- function(list_of_top_ngrams) {
  df <- data.table::rbindlist(list_of_top_ngrams)
  df <- df %>%
    dplyr::mutate(n = 1) %>%
    dplyr::group_by(words) %>%
    dplyr::summarise(n = sum(n)) %>%
    dplyr::mutate(n = ifelse(n == 1, "Unique", "Common")) %>%
    dplyr::rename(word_type = n)
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
#' unique_words <- fst_get_unique_ngrams_separate(top_child, top_dev)
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
#' @param title_size size to display plot title
#'
#' @return Plot of top n-grams with unique terms highlighted.
#' @export
#'
#' @examples
#' top_child <- fst_freq_table(fst_child)
#' top_dev <- fst_freq_table(fst_dev_coop)
#' unique_words <- fst_get_unique_ngrams_separate(top_child, top_dev)
#' top_child_u <- fst_join_unique(top_child, unique_words)
#' top_dev_u <- fst_join_unique(top_dev, unique_words)
#' fst_ngrams_compare_plot(top_child_u, ngrams = 1, name = "Child")
#' fst_ngrams_compare_plot(top_dev_u, ngrams = 1, name = "Dev", title_size = 10)
fst_ngrams_compare_plot <- function(table,
                                    number = 10,
                                    ngrams = 1,
                                    unique_colour = "indianred",
                                    name = NULL,
                                    override_title = NULL,
                                    title_size = 20) {
  colours <- c("Unique" = unique_colour, "Common" = "grey50")
  if (ngrams == 1) {
    term <- "Words"
  } else if (ngrams == 2) {
    term <- "Bigrams"
  } else {
    term <- paste0(as.character(ngrams), "-grams")
  }
  if (is.null(override_title)) {
    table %>%
      ggplot2::ggplot(ggplot2::aes(occurrence, words, fill = word_type)) +
      ggplot2::geom_col() +
      ggplot2::scale_fill_manual('Word Type', values = colours) +
      ggplot2::labs(y = NULL, title = paste(name, as.character(number), "Most Common", term)) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = title_size))
  } else {
    table %>%
      ggplot2::ggplot(ggplot2::aes(occurrence, words, fill = word_type)) +
      ggplot2::geom_col() +
      ggplot2::scale_fill_manual('Word Type', values = colours) +
      ggplot2::labs(y = NULL, title = override_title) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = title_size))
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
#' @param use_svydesign_field Option to get `field` for splitting the data from
#'  the svydesign object, default is `FALSE`
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
#' @param title_size size to display plot title
#' @param subtitle_size size to display title of individual top words plot
#'
#' @return Plots of most frequent words in the plots pane with unique words
#'  highlighted.
#' @export
#'
#' @examples
#' fst_freq_compare(fst_child, 'gender', number = 10, norm = "number_resp")
#' fst_freq_compare(fst_child, 'gender', number = 10, norm = NULL)
#' s <- survey::svydesign(id=~1, weights= ~paino, data = child)
#' c2 <- fst_child_2
#' c <- fst_child
#' g <- 'gender'
#' fst_freq_compare(c2, g, 10, NULL, NULL, TRUE, TRUE, TRUE, 'fsd_id', s)
#' fst_freq_compare(c, g, use_column_weights = TRUE, strict = FALSE)
fst_freq_compare <- function(data,
                             field,
                             number = 10,
                             norm = NULL,
                             pos_filter = NULL,
                             strict = TRUE,
                             use_svydesign_weights = FALSE,
                             use_svydesign_field = FALSE,
                             id = "",
                             svydesign = NULL,
                             use_column_weights = FALSE,
                             exclude_nulls = FALSE,
                             rename_nulls = 'null_data',
                             unique_colour = "indianred",
                             title_size = 20,
                             subtitle_size = 15) {
    if (use_svydesign_field == TRUE) {
      data <- fst_use_svydesign(data,
                                svydesign = svydesign,
                                id = id,
                                add_cols = field,
                                add_weights = FALSE
      )
    }
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
                                                    override_title = paste(field, '=', names2[i]),
                                                    title_size = subtitle_size)
    }
    title <- paste("Comparison Plot of", number, "Most Common Words")
    plot <- ggpubr::ggarrange(plotlist = list_of_plots, common.legend = TRUE, legend = "right")
    ggpubr::annotate_figure(plot, top = ggpubr::text_grob(title,
                                                          face = "bold", size = title_size
    ))
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
#' @param use_svydesign_field Option to get `field` for splitting the data from
#'  the svydesign object, default is `FALSE`
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
#' @param title_size size to display plot title
#' @param subtitle_size size to display title of individual top ngrams plot
#'
#' @return Plots of top n-grams in the plots pane with unique n-grams
#'  highlighted.
#' @export
#'
#' @examples
#' c <- fst_child
#' g <- 'gender'
#' fst_ngrams_compare(c, g, ngrams = 4, number = 10, norm = "number_resp")
#' fst_ngrams_compare(c, g, ngrams = 2, number = 10, norm = NULL)
#' s <- survey::svydesign(id=~1, weights= ~paino, data = child)
#' c2 <- fst_child_2
#' fst_ngrams_compare(c2, g, 10, 3, NULL, NULL, TRUE, TRUE, TRUE, 'fsd_id', s)
#' fst_ngrams_compare(c, g, 10, 2, use_column_weights = TRUE, strict = TRUE)
fst_ngrams_compare <- function(data,
                              field,
                              number = 10,
                              ngrams = 1,
                              norm = NULL,
                              pos_filter = NULL,
                              strict = TRUE,
                              use_svydesign_weights = FALSE,
                              use_svydesign_field = FALSE,
                              id = "",
                              svydesign = NULL,
                              use_column_weights = FALSE,
                              exclude_nulls = FALSE,
                              rename_nulls = 'null_data',
                              unique_colour = "indianred",
                              title_size = 20,
                              subtitle_size = 15) {
  if (use_svydesign_field == TRUE) {
    data <- fst_use_svydesign(data,
                              svydesign = svydesign,
                              id = id,
                              add_cols = field,
                              add_weights = FALSE
    )
  }
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
                                                  override_title = paste(field, '=', names2[i]),
                                                  title_size = subtitle_size)
  }

  if (ngrams == 1) {
    word = "Words"
  } else if (ngrams == 2) {
    word = "Bigrams"
  } else {
    word = paste0(as.character(ngrams), "-Grams")
  }
  title <- paste("Comparison Plot of", number, "Most Common", word)
  plot <- ggpubr::ggarrange(plotlist = list_of_plots, common.legend = TRUE, legend = "right")
  ggpubr::annotate_figure(plot, top = ggpubr::text_grob(title,
                                                        face = "bold", size = title_size
  ))
}

#' Compare parts-of-speech
#'
#' Count each POS type for different groups of participants. Data is split based
#' on different values in the `field` column of formatted data. Results will be
#' shown within the plots pane.
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
#' fst_pos_compare(fst_child, 'gender')
#' fst_pos_compare(fst_dev_coop, 'region')
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
#' Compare text responses for different groups of participants. Data is split
#' based on different values in the `field` column of formatted data. Results
#' will be shown within the plots pane.
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
#' fst_summarise_compare(fst_child, 'gender')
#' fst_summarise_compare(fst_dev_coop, 'gender')
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
#' fst_length_compare(fst_child, 'gender')
#' fst_length_compare(fst_dev_coop, 'education_level', incl_sentences = FALSE)
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
#' each group. Data is split based on different values in the `field` column of
#' formatted data. Results will be shown within the plots pane.
#'
#' @param data A dataframe of text in CoNLL-U format with additional `field`
#'  column for splitting data.
#' @param field Column in `data` used for splitting groups
#' @param pos_filter List of UPOS tags for inclusion, default is `NULL` which
#'  means all word types included.
#' @param max The maximum number of words to display, default is `100`.
#' @param norm The method for normalising the data. Valid settings are
#'  `"number_words"` (the number of words in the responses), `"number_resp"`
#'  (the number of responses), or `NULL` (raw count returned, default, also used
#'  when weights are applied).
#' @param use_svydesign_weights Option to weight words in the wordcloud using
#'  weights from  a svydesign object containing the raw data, default is `FALSE`
#' @param use_svydesign_field Option to get `field` for splitting the data from
#'  the svydesign object, default is `FALSE`
#' @param id ID column from raw data, required if `use_svydesign_weights = TRUE`
#'  and must match the `docid` in formatted `data`.
#' @param svydesign A svydesign object which contains the raw data and weights.
#' @param use_column_weights Option to weight words in the wordcloud using
#'  weights from  formatted data which includes addition `weight` column,
#'  default is `FALSE`
#' @param exclude_nulls Whether to include NULLs in `field` column, default is
#'  `FALSE`
#' @param rename_nulls What to fill NULL values with if `exclude_nulls = FALSE`.
#'
#' @return A comparison cloud from wordcloud package.
#' @export
#'
#' @examples
#' fst_comparison_cloud(fst_child, 'gender', max = 50)
#' s <- survey::svydesign(id=~1, weights= ~paino, data = child)
#' i <- 'fsd_id'
#' c2 <- fst_child_2
#' fst_comparison_cloud(c2, 'gender', NULL, 100, NULL, TRUE, TRUE, i, s)
#' T <- TRUE
#' fst_comparison_cloud(fst_dev_coop, 'education_level', use_column_weights = T)
#' pf <- c("NOUN", "VERB", "ADJ", "ADV")
#' pf2 <- "NOUN, VERB, ADJ, ADV"
#' fst_comparison_cloud(fst_dev_coop, 'gender', pos_filter = pf)
#' fst_comparison_cloud(fst_dev_coop, 'gender', pos_filter = pf2)
#' fst_comparison_cloud(fst_dev_coop, 'gender', norm = 'number_resp')
fst_comparison_cloud <- function(data,
                                 field,
                                 pos_filter = NULL,
                                 max = 100,
                                 norm = NULL,
                                 use_svydesign_weights = FALSE,
                                 use_svydesign_field = FALSE,
                                 id = "",
                                 svydesign = NULL,
                                 use_column_weights = FALSE,
                                 exclude_nulls = FALSE,
                                 rename_nulls = "null_data") {
  if (use_svydesign_field == TRUE) {
    data <- fst_use_svydesign(data = data,
                              svydesign = svydesign,
                              id = id,
                              add_cols = field,
                              add_weights = FALSE)
  }
  if (exclude_nulls == TRUE) {
    data <- data %>% tidyr::drop_na(field)
  } else {
    data[is.na(data)] <- rename_nulls
  }
  if (use_svydesign_weights == TRUE) {
    data <- fst_use_svydesign(data = data, svydesign = svydesign, id = id)
  }
  if (!is.null(pos_filter)) {
    if (length(pos_filter) == 1) {
      pos_filter <- pos_filter %>%
        stringr::str_extract_all(pattern = "\\w+") %>%
        unlist()
    }
    data <- dplyr::filter(data, upos %in% pos_filter)
  }
  data <- data %>%
    dplyr::filter(.data$dep_rel != "punct") %>%
    dplyr::filter(!is.na(lemma)) %>%
    dplyr::filter(lemma != "na")
  group_data <- data %>% dplyr::group_by_at(field)
  split_data <- dplyr::group_split(group_data)
  names <- dplyr:: group_keys(group_data)
  names(split_data) <- names[[field]]
  names_list <- names[[field]]
  wordcloud_data <- list()
  if (use_svydesign_weights == TRUE) {
    for (i in 1:length(split_data)) {
      data1 <- split_data[[i]]
      words_counts <- dplyr::count(data1, lemma, sort = TRUE, wt = weight) %>%
        dplyr::rename(!!paste0(as.character(names_list[i]), "- Weighted Freq") := n)
      wordcloud_data <- append(wordcloud_data, list(words_counts))
    }
  } else if (use_column_weights == TRUE) {
    for (i in 1:length(split_data)) {
      data1 <- split_data[[i]]
      words_counts <- dplyr::count(data1, lemma, sort = TRUE, wt = weight) %>%
        dplyr::rename(!!paste0(as.character(names_list[i])) := n)
      wordcloud_data <- append(wordcloud_data, list(words_counts))
    }
  } else {
    for (i in 1:length(split_data)) {
      data1 <- split_data[[i]]
      if (is.null(norm)) {
        denom <- 1
      } else if (norm == "number_words") {
        data2 <- data1
        data2 %>%
          dplyr::filter(data2$dep_rel != "punct") %>%
          dplyr::filter(!is.na(lemma)) %>%
          dplyr::filter(lemma != "na")
        denom <- nrow(data2)
      } else if (norm == "number_resp") {
        data2 <- data1
        denom <- dplyr::n_distinct(data2$doc_id)
      } else {
        message("NOTE: A recognised normalisation method has not been provided. \n Function has defaulted to provide raw counts.")
        denom <- 1
      }

      words_counts <- dplyr::count(data1, lemma, sort = TRUE) %>%
        dplyr::mutate(n = (n / denom)) %>%
        dplyr::rename(!!paste0(as.character(names_list[i])) := n)
      wordcloud_data <- append(wordcloud_data, list(words_counts))
    }
  }
  compcloud_data <- wordcloud_data %>% purrr::reduce(dplyr::full_join, by = "lemma")
  compcloud_data <- as.data.frame(compcloud_data)
  rownames(compcloud_data) <- compcloud_data$lemma
  compcloud_data$lemma <- NULL
  compcloud_data[is.na(compcloud_data)] <- 0
  scale_x <- 2
  if (max <= 50) {
    scale_x <- 4
  } else if (max <= 100) {
    scale_x <- 3
  } else {
    scale_x <- 2
  }
  wordcloud::comparison.cloud(compcloud_data,
                              max.words = max,
                              random.order = FALSE,
                              rot.per = 0.35,
                              colors = RColorBrewer::brewer.pal(8, "Dark2"),
                              title.size = 2,
                              scale = c(scale_x, 0.5)
  )
}
