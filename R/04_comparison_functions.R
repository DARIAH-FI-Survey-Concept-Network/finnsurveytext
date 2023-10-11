#' Make Top Words Table
#'
#' Creates a table of the most frequently-occurring words (unigrams) within the
#' data.
#'
#' @param data A dataframe of text in CoNLL-U format.
#' @param number The number of top words to return, default is `10`.
#' @param pos_filter List of UPOS tags for inclusion, default is `NULL` which
#'  means all word types included
#'
#' @return A table of the most frequently occurring words in the data
#' @export
#'
#' @examples
#' fst_get_top_words(conllu_dev_q11_1_nltk, number = 20)
#' top_bullying_words <- fst_get_top_words(conllu_bullying, number = 15, pos_filter = c("NOUN", "VERB", "ADJ", "ADV"))
#' top_f <- fst_get_top_words(conllu_f_nltk, number = 15)
#' top_m <- fst_get_top_words(conllu_m_nltk, number = 15)
#' top_na <- fst_get_top_words(conllu_na_nltk, number = 15)
fst_get_top_words <- function(data, number = 10, pos_filter = NULL) {
  if (!is.null(pos_filter)) {
    data <- dplyr::filter(data, .data$upos %in% pos_filter)
  }
  data %>%
    dplyr::filter(.data$dep_rel != "punct") %>%
    dplyr::filter(!is.na(lemma)) %>%
    dplyr::filter(lemma != 'na') %>%
    dplyr::count(lemma, sort = TRUE) %>%
    dplyr::slice_max(n, n = number, with_ties = FALSE) %>%
    dplyr::mutate(lemma = reorder(lemma, n)) %>%
    dplyr::rename(words = lemma)
}

#' Make Top N-grams Table
#'
#' Creates a table of the most frequently-occurring ngrams within the
#' data.
#'
#' @param data A dataframe of text in CoNLL-U format.
#' @param number The number of n-grams to return, default is `10`.
#' @param ngrams The type of n-grams to return, default is `1`.
#' @param pos_filter List of UPOS tags for inclusion, default is `NULL` which
#'  means all word types included
#'
#' @return A table of the most frequently occurring n-grams in the data.
#' @export
#'
#' @examples
#' fst_get_top_ngrams(conllu_dev_q11_1_nltk)
#' fst_get_top_ngrams(conllu_dev_q11_1_nltk, number = 10, ngrams = 1)
#' top_bullying_ngrams <- fst_get_top_ngrams(conllu_bullying, number = 15, pos_filter = c("NOUN", "VERB", "ADJ", "ADV"))
#' topn_f <- fst_get_top_ngrams(conllu_f_nltk, number = 15, ngrams = 2)
#' topn_m <- fst_get_top_ngrams(conllu_m_nltk, number = 15, ngrams = 2)
#' topn_na <- fst_get_top_ngrams(conllu_na_nltk, number = 15, ngrams = 2)
fst_get_top_ngrams <- function(data, number = 10, ngrams = 1, pos_filter = NULL){
  if (!is.null(pos_filter)) {
    data <- dplyr::filter(data, .data$upos %in% pos_filter)
  }
  data %>%
    dplyr::filter(.data$dep_rel != "punct") %>%
    dplyr::filter(!is.na(lemma)) %>%
    dplyr::filter(lemma != 'na') %>%
    dplyr::mutate(words = udpipe::txt_nextgram(lemma, n = ngrams)) %>%
    dplyr::count(words, sort = TRUE) %>%
    dplyr::slice_max(n, n = number, with_ties = FALSE) %>%
    dplyr::mutate(words = reorder(words, n)) %>%
    dplyr::filter(!is.na(words)) %>%
    dplyr::filter(words != 'na')
}



#' Get Unique N-grams
#'
#' Takes at least two tables of n-grams and frequencies (either output of
#' `fst_get_top_words` or `fst_get_top_ngrams`) and finds n-grams unique to one
#' table.
#'
#' @param table1 The first table
#' @param table2 The second table
#' @param ... Any other tables you want to include
#'
#' @return
#' @export
#'
#' @examples
#' unique_w <- fst_get_unique_ngrams(top_f, top_m, top_na)
#' unique_n <- fst_get_unique_ngrams(topn_f, topn_m, topn_na)
fst_get_unique_ngrams <- function(table1, table2, ...) {
  df <- rbind(table1, table2, ...)
  df <- df %>%
    dplyr::mutate(n = 1) %>%
    dplyr::group_by(words) %>%
    dplyr::summarise(n = sum(n)) %>%
    dplyr::mutate(n = ifelse(n == 1, "yes", "no")) %>%
    dplyr::rename(unique_word = n)
  df
}


#' Merge N-grams Table with Unique Words
#'
#' @param table Output of `fst_get_top_words` or `fst_get_top_ngrams`
#' @param unique_table Output of `fst_get_unique_ngrams`
#'
#' @return A table of top n-grams, frequency, and whether the n-gram is "unique"
#' @export
#'
#' @examples
#' top_fu <- fst_join_unique(top_f, unique_w)
#' top_mu <- fst_join_unique(top_m, unique_w)
#' top_nau <- fst_join_unique(top_na, unique_w)
#' topn_fu <- fst_join_unique(topn_f, unique_n)
#' topn_mu <- fst_join_unique(topn_m, unique_n)
#' topn_nau <- fst_join_unique(topn_na, unique_n)
fst_join_unique <- function(table, unique_table){
  table <- table %>% dplyr::left_join(unique_table, by='words')
  table
}

#' Plot N-grams
#'
#' Plots frequency n-grams with unique n-grams highlighted.
#'
#' @param table The table of n-grams, output of `get_unique_ngrams`
#' @param number The number of n-grams, default is `10`.
#' @param ngrams The type of n-grams, default is `1`.
#' @param unique_colour Colour to display unique words, default is `indianred`.
#' @param name An optional "name" for the plot, default is `NULL`
#'
#' @return Plot of top n-grams with unique terms highlighted.
#' @export
#'
#' @examples
#' fst_ngrams_plot(top_fu, ngrams = 1, name = 'Female')
#' plot_top_fu <- fst_ngrams_plot(top_fu, ngrams = 1, number = 8, override_title = 'Female')
#' plot_top_mu <- fst_ngrams_plot(top_mu, ngrams = 1, override_title = 'Male')
#' plot_top_nau <- fst_ngrams_plot(top_nau, ngrams = 1, override_title = 'No Gender Specified')
#' plot_topn_fu <- fst_ngrams_plot(topn_fu, name = 'Female')
#' plot_topn_mu <- fst_ngrams_plot(topn_mu, name = 'Male')
#' plot_top_mu + ggplot2::ggtitle("Plot of Top Male Words") + ggplot2::xlab("Count") + ggplot2::ylab("Top Words") # How to overwrite
#' plot_topn_nau <- fst_ngrams_plot(topn_nau, name = 'No Gender Specified')
fst_ngrams_plot <- function(table, number = 10, ngrams = 1, unique_colour = 'indianred', name = NULL, override_title = NULL) {
   colours <- c("yes" = unique_colour, "no" = "grey50")
  if (ngrams == 1) {
    term = 'Words'
  } else if (ngrams == 2) {
    term = 'Bigrams'
  } else {
    term = paste0(as.character(ngrams), "-grams")
  }
  if (is.null(override_title)){
    table %>%
      ggplot2::ggplot(ggplot2::aes(n, words, fill = unique_word)) +
      ggplot2::geom_col() +
      ggplot2::scale_fill_manual(values = colours, guide = "none") +
      ggplot2::labs(y = NULL, title = paste(name, as.character(number),"Most Common", term))
  } else {
    table %>%
      ggplot2::ggplot(ggplot2::aes(n, words, fill = unique_word)) +
      ggplot2::geom_col() +
      ggplot2::scale_fill_manual(values = colours, guide = "none") +
      ggplot2::labs(y = NULL, title = override_title)
  }
}




#' Display Comparison Plots
#'
#' Display between 2 and 4 plots within the plots pane. If 2 or 3 plots, they
#' will be in a single row, if there are 4 plots, they will be in 2 rows of 2.
#'
#' @param plot1 First plot to display
#' @param plot2 Second plot to display
#' @param plot3 Optional third plot to display, defaul is `NULL`
#' @param plot4 Optional fourth plot to display, defaul is `NULL`
#'
#' @return Up to 4 plots within the plots pane.
#' @export
#'
#' @examples
#' fst_plot_compare(plot_top_fu, plot_top_mu, plot_top_nau, main_title = 'Comparison Plots')
#' fst_plot_compare(plot_topn_fu, plot_topn_mu, plot_topn_nau, plot_topn_nau)
#' fst_plot_compare(plot_topn_fu, plot_topn_mu, main_title = 'Comparison Plots')
fst_plot_compare <- function(plot1, plot2, plot3 = NULL, plot4 = NULL, main_title = NULL) {
  if (!is.null(plot3)) {
    if (!is.null(plot4)) {
      gridExtra::grid.arrange(plot1, plot2, plot3, plot4, ncol = 2, top = main_title)
    } else {
      gridExtra::grid.arrange(plot1, plot2, plot3, ncol = 3, top = main_title)
    }
  } else {
    gridExtra::grid.arrange(plot1, plot2, ncol = 2, top = main_title)
  }
}


#' Compare and Plot Top Words
#'
#' Find top and unique top words for between 2 and 4 sets of prepared data.
#' Results will be shown within the plots pane. If 2 or 3 plots, they will be in
#' a single row, if there are 4 plots, they will be in 2 rows of 2.
#'
#' @param data1 A dataframe of text in CoNLL-U format for the first plot.
#' @param data2 A dataframe of text in CoNLL-U format for the second plot.
#' @param data3 An option dataframe of text in CoNLL-U format for the third
#' plot, default is `NULL`
#' @param data4 An option dataframe of text in CoNLL-U format for the fourth
#' plot, default is `NULL`
#' @param number The number of top words to return, default is `20`.
#' @param pos_filter List of UPOS tags for inclusion, default is `NULL` which
#'  means all word types included
#' @param name1 An optional "name" for the first plot, default is `NULL`
#' @param name2 An optional "name" for the second plot, default is `NULL`
#' @param name3 An optional "name" for the third plot, default is `NULL`
#' @param name4 An optional "name" for the fourth plot, default is `NULL`
#' @param unique_colour Colour to display unique words, default is `indianred`.
#'
#' @return Between 2 and 4 plots of Top n-grams in the plots pane with unique
#' n-grams highlighted
#' @export
#'
#' @examples
#' fst_unigrams_compare(conllu_dev_q11_1_m_nltk, conllu_dev_q11_1_f_nltk, number = 10)
#' fst_unigrams_compare(conllu_dev_q11_1_m_nltk, conllu_dev_q11_1_f_nltk, conllu_dev_q11_1_na_nltk, number = 5)
#' fst_unigrams_compare(conllu_dev_q11_1_m_nltk, conllu_dev_q11_1_f_nltk, conllu_dev_q11_1_na_nltk,number = 15, unique_colour = 'pink', pos_filter = c("NOUN", "VERB", "ADJ", "ADV"), name1 = 'Male', name2 = 'Female', name3 = 'Not Specified')
fst_unigrams_compare <- function(data1, data2, data3 = NULL, data4 = NULL, number = 10, pos_filter = NULL, name1 = "Group 1", name2 = "Group 2", name3 = "Group 3", name4 = "Group 4", unique_colour = 'indianred') {
  if (!is.null(data3)){
    if (!is.null(data4)){
      top4 <- fst_get_top_words(data4, number = number, pos_filter = pos_filter)
      top3 <- fst_get_top_words(data3, number = number, pos_filter = pos_filter)
      top2 <- fst_get_top_words(data2, number = number, pos_filter = pos_filter)
      top1 <- fst_get_top_words(data1, number = number, pos_filter = pos_filter)
    } else {
      top3 <- fst_get_top_words(data3, number = number, pos_filter = pos_filter)
      top2 <- fst_get_top_words(data2, number = number, pos_filter = pos_filter)
      top1 <- fst_get_top_words(data1, number = number, pos_filter = pos_filter)
    }
  } else {
    top2 <- fst_get_top_words(data2, number = number, pos_filter = pos_filter)
    top1 <- fst_get_top_words(data1, number = number, pos_filter = pos_filter)
  }
  if (!is.null(data3)){
    if (!is.null(data4)){
      unique <- fst_get_unique_ngrams(top1, top2, top3, top4)
      top4_2 <- fst_join_unique(top4, unique)
      top3_2 <- fst_join_unique(top3, unique)
      top2_2 <- fst_join_unique(top2, unique)
      top1_2 <- fst_join_unique(top1, unique)
      plot4 <- fst_ngrams_plot(top4_2, number = number, unique_colour = unique_colour, override_title = name4)
      plot3 <- fst_ngrams_plot(top3_2, number = number, unique_colour = unique_colour, override_title = name3)
      plot2 <- fst_ngrams_plot(top2_2, number = number, unique_colour = unique_colour, override_title = name2)
      plot1 <- fst_ngrams_plot(top1_2, number = number, unique_colour = unique_colour, override_title = name1)
      fst_plot_compare(plot1 = plot1, plot2 = plot2, plot3 = plot3, plot4 = plot4, main_title = paste("Comparison Plot of", as.character(number),"Most Common Words"))
    } else {
      unique <- fst_get_unique_ngrams(top1, top2, top3)
      top3_2 <- fst_join_unique(top3, unique)
      top2_2 <- fst_join_unique(top2, unique)
      top1_2 <- fst_join_unique(top1, unique)
      plot3 <- fst_ngrams_plot(top3_2, number = number, unique_colour = unique_colour, override_title = name3)
      plot2 <- fst_ngrams_plot(top2_2, number = number, unique_colour = unique_colour, override_title = name2)
      plot1 <- fst_ngrams_plot(top1_2, number = number, unique_colour = unique_colour, override_title = name1)
      fst_plot_compare(plot1 = plot1, plot2 = plot2, plot3 = plot3, main_title = paste("Comparison Plot of", as.character(number),"Most Common Words"))
    }
  } else {
    unique <- fst_get_unique_ngrams(top1, top2)
    top2_2 <- fst_join_unique(top2, unique)
    top1_2 <- fst_join_unique(top1, unique)
    plot2 <- fst_ngrams_plot(top2_2, number = number, unique_colour = unique_colour, override_title = name2)
    plot1 <- fst_ngrams_plot(top1_2, number = number, unique_colour = unique_colour, override_title = name1)
    fst_plot_compare(plot1 = plot1, plot2 = plot2, main_title = paste("Comparison Plot of", as.character(number),"Most Common Words"))
  }
}

#' Compare and Plot Top N-Grams
#'
#' Find top and unique top n-grams for between 2 and 4 sets of prepared data.
#' Results will be shown within the plots pane. If 2 or 3 plots, they will be in
#' a single row, if there are 4 plots, they will be in 2 rows of 2.
#'
#' @param data1 A dataframe of text in CoNLL-U format for the first plot.
#' @param data2 A dataframe of text in CoNLL-U format for the second plot.
#' @param data3 An option dataframe of text in CoNLL-U format for the third
#' plot, default is `NULL`
#' @param data4 An option dataframe of text in CoNLL-U format for the fourth
#' plot, default is `NULL`
#' @param number The number of n-grams to return, default is `10`.
#' @param ngrams The type of n-grams to return, default is `1`.
#' @param pos_filter List of UPOS tags for inclusion, default is `NULL` which
#'  means all word types included
#' @param name1 An optional "name" for the first plot, default is `NULL`
#' @param name2 An optional "name" for the second plot, default is `NULL`
#' @param name3 An optional "name" for the third plot, default is `NULL`
#' @param name4 An optional "name" for the fourth plot, default is `NULL`
#' @param unique_colour Colour to display unique words, default is `indianred`.
#'
#' @return Between 2 and 4 plots of Top n-grams in the plots pane with unique
#' n-grams highlighted
#' @export
#'
#' @examples
#' fst_ngrams_compare(conllu_dev_q11_1_m_nltk, conllu_dev_q11_1_f_nltk, number = 10)
#' fst_ngrams_compare(conllu_dev_q11_1_m_nltk, conllu_dev_q11_1_f_nltk, number = 5, ngrams = 3, unique_colour = "black", name1 = 'Male', name2 = 'Female')
#' fst_ngrams_compare(conllu_dev_q11_1_m_nltk, conllu_dev_q11_1_f_nltk, conllu_dev_q11_1_na_nltk, conllu_dev_q11_1_m, number = 20, unique_colour = 'slateblue', pos_filter = c("NOUN", "VERB", "ADJ", "ADV"), name1 = 'Male', name2 = 'Female', name3 = 'Not Spec', name4 = 'Male2')
 fst_ngrams_compare <- function(data1, data2, data3 = NULL, data4 = NULL, number = 10, ngrams = 1, pos_filter = NULL, name1 = "Group 1", name2 = "Group 2", name3 = "Group 3", name4 = "Group 4", unique_colour = 'indianred') {
  if (ngrams == 1) {
    term = 'Words'
  } else if (ngrams == 2) {
    term = 'Bigrams'
  } else {
    term = paste0(as.character(ngrams), "-grams")
  }
  if (!is.null(data3)){
    if (!is.null(data4)){
      top4 <- fst_get_top_ngrams(data4, number = number, ngrams = ngrams, pos_filter = pos_filter)
      top3 <- fst_get_top_ngrams(data3, number = number, ngrams = ngrams, pos_filter = pos_filter)
      top2 <- fst_get_top_ngrams(data2, number = number, ngrams = ngrams, pos_filter = pos_filter)
      top1 <- fst_get_top_ngrams(data1, number = number, ngrams = ngrams, pos_filter = pos_filter)
    } else {
      top3 <- fst_get_top_ngrams(data3, number = number, ngrams = ngrams, pos_filter = pos_filter)
      top2 <- fst_get_top_ngrams(data2, number = number, ngrams = ngrams, pos_filter = pos_filter)
      top1 <- fst_get_top_ngrams(data1, number = number, ngrams = ngrams, pos_filter = pos_filter)
    }
  } else {
    top2 <- fst_get_top_ngrams(data2, number = number, ngrams = ngrams, pos_filter = pos_filter)
    top1 <- fst_get_top_ngrams(data1, number = number, ngrams = ngrams, pos_filter = pos_filter)
  }
  if (!is.null(data3)){
    if (!is.null(data4)){
      unique <- fst_get_unique_ngrams(top1, top2, top3, top4)
      top4_2 <- fst_join_unique(top4, unique)
      top3_2 <- fst_join_unique(top3, unique)
      top2_2 <- fst_join_unique(top2, unique)
      top1_2 <- fst_join_unique(top1, unique)
      plot4 <- fst_ngrams_plot(top4_2, number = number, ngrams = ngrams, unique_colour = unique_colour, override_title = name4)
      plot3 <- fst_ngrams_plot(top3_2, number = number, ngrams = ngrams, unique_colour = unique_colour, override_title = name3)
      plot2 <- fst_ngrams_plot(top2_2, number = number, ngrams = ngrams, unique_colour = unique_colour, override_title = name2)
      plot1 <- fst_ngrams_plot(top1_2, number = number, ngrams = ngrams, unique_colour = unique_colour, override_title = name1)
      fst_plot_compare(plot1 = plot1, plot2 = plot2, plot3 = plot3, plot4 = plot4, main_title = paste("Comparison Plot of", as.character(number),"Most Common", term))
    } else {
      unique <- fst_get_unique_ngrams(top1, top2, top3)
      top3_2 <- fst_join_unique(top3, unique)
      top2_2 <- fst_join_unique(top2, unique)
      top1_2 <- fst_join_unique(top1, unique)
      plot3 <- fst_ngrams_plot(top3_2, number = number, ngrams = ngrams, unique_colour = unique_colour, override_title = name3)
      plot2 <- fst_ngrams_plot(top2_2, number = number, ngrams = ngrams, unique_colour = unique_colour, override_title = name2)
      plot1 <- fst_ngrams_plot(top1_2, number = number, ngrams = ngrams, unique_colour = unique_colour, override_title = name1)
      fst_plot_compare(plot1 = plot1, plot2 = plot2, plot3 = plot3, main_title = paste("Comparison Plot of", as.character(number),"Most Common", term))
    }
  } else {
    unique <- fst_get_unique_ngrams(top1, top2)
    top2_2 <- fst_join_unique(top2, unique)
    top1_2 <- fst_join_unique(top1, unique)
    plot2 <- fst_ngrams_plot(top2_2, number = number, ngrams = ngrams, unique_colour = unique_colour, override_title = name2)
    plot1 <- fst_ngrams_plot(top1_2, number = number, ngrams = ngrams, unique_colour = unique_colour, override_title = name1)
    fst_plot_compare(plot1 = plot1, plot2 = plot2, main_title = paste("Comparison Plot of", as.character(number),"Most Common", term))
  }
}


### NOTE THAT YOU MIGHT NOT WANT THE NLTK REUSLTS EHRE


#' Compare Parts-Of-Speech
#'
#' Compare words in responses based on part-of-speech tagging for between 2 and
#' 4 sets of prepared data.
#'
#' @param data1 A dataframe of text in CoNLL-U format for the first group
#' @param data2 A dataframe of text in CoNLL-U format for the second group.
#' @param data3 An option dataframe of text in CoNLL-U format for the third
#' group, default is `NULL`
#' @param data4 An option dataframe of text in CoNLL-U format for the fourth
#' group, default is `NULL`
#' @param name1 An optional "name" for the first group, default is `NULL`
#' @param name2 An optional "name" for the second group, default is `NULL`
#' @param name3 An optional "name" for the third group, default is `NULL`
#' @param name4 An optional "name" for the fourth group, default is `NULL`
#'
#' @return Table of POS tag counts for the groups
#' @export
#'
#' @examples
#' fst_pos_compare(conllu_dev_q11_1_f, conllu_dev_q11_1_m, conllu_dev_q11_1_na, conllu_dev_q11_1, "Female", "Male", "No Gender Specified", "All")
#' fst_pos_compare(conllu_dev_q11_1_f, conllu_dev_q11_1_m)
#' pos_table <- fst_pos_compare(data1 = conllu_dev_q11_1, name1 = "All", data2 = conllu_dev_q11_1_m, name2 = "Male", data3 = conllu_dev_q11_1_f, name3 = "Female", data4 = conllu_dev_q11_1_na, name4 = "Not Spec")
fst_pos_compare <- function(data1, data2, data3 = NULL, data4 = NULL, name1 = "Group 1", name2 = "Group 2", name3 = "Group 3", name4 = "Group 4") {
  pos_lookup <- data.frame('upos' =c('ADJ', 'ADP', 'ADV', 'AUX', 'CCONJ', 'DET',
                                     'INTJ', 'NOUN', 'NUM', 'PART', 'PRON',
                                     'PROPN', 'PUNCT', 'SCONJ', 'SYM', 'VERB',
                                     'X'),
                           "Part-of-Speech Name" = c(' adjective', ' adposition',
                                           ' adverb', ' auxiliary',
                                           ' coordinating conjunction',
                                           ' determiner', ' interjection',
                                           ' noun', ' numeral', ' particle',
                                           ' pronoun', ' proper noun',
                                           ' punctuation',
                                           ' subordinating conjunction',
                                           ' symbol', ' verb', ' other'))
  if (!is.null(data3)) {
    if (!is.null(data4)) {
      pos_table4 <- data4 %>% dplyr::count(upos, sort = TRUE) %>% dplyr::rename(!!name4 := n)
      pos_table3 <- data3 %>% dplyr::count(upos, sort = TRUE) %>% dplyr::rename(!!name3 := n)
      pos_table2 <- data2 %>% dplyr::count(upos, sort = TRUE) %>% dplyr::rename(!!name2 := n)
      pos_table1 <- data1 %>% dplyr::count(upos, sort = TRUE) %>% dplyr::rename(!!name1 := n)
      df <- merge(x = pos_lookup, y = pos_table1, by = "upos") %>%
        merge(pos_table2, by = "upos") %>%
        merge(pos_table3, by = "upos") %>%
        merge(pos_table4, by = "upos")
    } else {
      pos_table3 <- data3 %>% dplyr::count(upos, sort = TRUE) %>% dplyr::rename(!!name3 := n)
      pos_table2 <- data2 %>% dplyr::count(upos, sort = TRUE) %>% dplyr::rename(!!name2 := n)
      pos_table1 <- data1 %>% dplyr::count(upos, sort = TRUE) %>% dplyr::rename(!!name1 := n)
      df <- merge(x = pos_lookup, y = pos_table1, by = "upos") %>%
        merge(pos_table2, by = "upos") %>%
        merge(pos_table3, by = "upos")
    }
  } else {
    pos_table2 <- data2 %>% dplyr::count(upos, sort = TRUE) %>% dplyr::rename(!!name2 := n)
    pos_table1 <- data1 %>% dplyr::count(upos, sort = TRUE) %>% dplyr::rename(!!name1 := n)
    df <- merge(x = pos_lookup, y = pos_table1, by = "upos") %>%
      merge(pos_table2, by = "upos")
  }
  df
}



#' Make Summary Table
#'
#' Creates a summary table for the input CoNLL-U data which provides the
#' response count and proportion, total number of words, the number of unique
#' words, and the number of unique lemmas.
#'
#' @param data A dataframe of text in CoNLL-U format.
#' @param value A string describing respondents, default is `All respondents`
#'
#' @return Summary table for data
#' @export
#'
#' @examples
#' fst_summarise(conllu_dev_q11_1)
#' female <- fst_summarise(conllu_dev_q11_1_f, "Female respondents")
fst_summarise <- function(data, value = 'All respondents') {
  no_resp_count <- length(which(data$sentence %in% c("NA", "na")))
  df <- data %>%
    dplyr::summarize('Description' = value,
                     'No Response' = no_resp_count,
                     'Respondents' = dplyr::n_distinct(doc_id),
                     'Proportion' = round(dplyr::n_distinct(doc_id) / (no_resp_count + dplyr::n_distinct(doc_id)), 2),
                     'Total Words' = dplyr::n(),
                     'Unique Words' = length(unique(token)),
                     'Unique Lemmas' = length(unique(lemma)))
  df
}

#' Make Comparison Summary
#'
#' Compare text responses for between 2 and 4 sets of prepared data.
#'
#' @param data1 A dataframe of text in CoNLL-U format for the first group
#' @param data2 A dataframe of text in CoNLL-U format for the second group.
#' @param data3 An option dataframe of text in CoNLL-U format for the third
#' group, default is `NULL`
#' @param data4 An option dataframe of text in CoNLL-U format for the fourth
#' group, default is `NULL`
#' @param desc1 A string describing data1, default is `Group 1`
#' @param desc2 A string describing data2, default is `Group 2`
#' @param desc3 A string describing data3, default is `Group 3`
#' @param desc4 A string describing data4, default is `Group 4`
#'
#' @return Summary table of responses between groups
#' @export
#'
#' @examples
#' fst_summarise_compare(conllu_m, conllu_f, conllu_s, conllu_dev_q11_1_nltk, "Male", "Female", "No Gender Specified", "All")
#' summary <- fst_summarise_compare(conllu_m, conllu_f)
#' fst_summarise_compare(conllu_dev_q11_1_m , conllu_dev_q11_1_f,  desc1 = "Male", desc2 = "Female")
fst_summarise_compare <- function(data1, data2, data3 = NULL, data4 = NULL, desc1 = "Group 1", desc2 = "Group 2", desc3 = "Group 3", desc4 = "Group 4") {
  if (!is.null(data3)) {
    if (!is.null(data4)) {
      sum4 <- fst_summarise(data4, desc4)
      sum3 <- fst_summarise(data3, desc3)
      sum2 <- fst_summarise(data2, desc2)
      sum1 <- fst_summarise(data1, desc1)
      df <- rbind(sum1, sum2, sum3, sum4)
    } else {
      sum3 <- fst_summarise(data3, desc3)
      sum2 <- fst_summarise(data2, desc2)
      sum1 <- fst_summarise(data1, desc1)
      df <- rbind(sum1, sum2, sum3)
    }
  } else {
    sum2 <- fst_summarise(data2, desc2)
    sum1 <- fst_summarise(data1, desc1)
    df <- rbind(sum1, sum2)
  }
  df
}


#' Make Length Table
#'
#' Create table summarising length of responses
#'
#' @param dataA dataframe of text in CoNLL-U format.
#' @param desc An optional string describing respondents, default is `NULL`
#' @param incl_sentences Whether to include sentence data in table, default is `TRUE`
#'
#' @return Table summarising lengths of responses
#' @export
#'
#' @examples
#' fst_length_summary(conllu_dev_q11_1, incl_sentences = FALSE)
#' fem <- fst_length_summary(conllu_dev_q11_1, desc = 'Female')
fst_length_summary <- function(data, desc = NULL, incl_sentences = TRUE) {
  no_resp_count <- length(which(data$sentence %in% c("NA", "na")))
  data <- dplyr::select(data, doc_id, sentence) %>%
    dplyr::mutate(length = stringr::str_count(sentence, '\\w+')) %>%
    dplyr::filter(!is.na(sentence)) %>%
    dplyr::filter(sentence != 'na') %>%
    dplyr::filter(sentence != 'NA')
  data <- data[!duplicated(data), ] %>%
    dplyr::group_by(doc_id) %>%
    dplyr::summarise(number_sentences = dplyr::n(), number_of_words = sum(length))
  word_df <- data %>%
    dplyr::summarize('Description' = paste0(desc, '- Words'),
                     'Respondents' = dplyr::n_distinct(doc_id),
                     'Mean' = mean(data$number_of_words),
                     'Minimum' =  min(data$number_of_words),
                     'Q1' = quantile(data$number_of_words, 0.25),
                     'Median' = median(data$number_of_words),
                     'Q3' = quantile(data$number_of_words, 0.75),
                     'Maximum' = max(data$number_of_words)
    )
  if (incl_sentences == TRUE) {
    sentence_df <- data %>%
      dplyr::summarize('Description' = paste0(desc, '- Sentences'),
                       'Respondents' = dplyr::n_distinct(doc_id),
                       'Mean' = mean(data$number_sentences),
                       'Minimum' =  min(data$number_sentences),
                       'Q1' = quantile(data$number_sentences, 0.25),
                       'Median' = median(data$number_sentences),
                       'Q3' = quantile(data$number_sentences, 0.75),
                       'Maximum' = max(data$number_sentences)
      )
    word_df <- rbind(word_df, sentence_df)
  }
  word_df
}



#' Compare Response Lengths
#'
#' Compare length of text responses for between 2 and 4 sets of prepared data.
#'
#' @param data1 A dataframe of text in CoNLL-U format for the first group
#' @param data2 A dataframe of text in CoNLL-U format for the second group.
#' @param data3 An option dataframe of text in CoNLL-U format for the third
#' group, default is `NULL`
#' @param data4 An option dataframe of text in CoNLL-U format for the fourth
#' group, default is `NULL`
#' @param desc1 A string describing data1, default is `Group 1`
#' @param desc2 A string describing data2, default is `Group 2`
#' @param desc3 A string describing data3, default is `Group 3`
#' @param desc4 A string describing data4, default is `Group 4`
#' @param incl_sentences Whether to include sentence data in table, default is `TRUE`
#'
#' @return
#' @export
#'
#' @examples
#' fst_length_compare(conllu_dev_q11_1_f, conllu_dev_q11_1_m, conllu_dev_q11_1_na, conllu_dev_q11_1, "Female", "Male", "Not Spec", "All")
#' male_female <- fst_length_compare(conllu_dev_q11_1_f, conllu_dev_q11_1_m, desc1 = "Female", desc2 = "Male")
fst_length_compare <- function(data1, data2, data3 = NULL, data4 = NULL, desc1 = "Group 1", desc2 = "Group 2", desc3 = "Group 3", desc4 = "Group 4", incl_sentences = TRUE) {
    if (!is.null(data3)) {
      if (!is.null(data4)) {
        sum4 <- fst_length_summary(data4, desc4, incl_sentences = incl_sentences)
        sum3 <- fst_length_summary(data3, desc3, incl_sentences = incl_sentences)
        sum2 <- fst_length_summary(data2, desc2, incl_sentences = incl_sentences)
        sum1 <- fst_length_summary(data1, desc1, incl_sentences = incl_sentences)
        df <- rbind(sum1, sum2, sum3, sum4)
      } else {
        sum3 <- fst_length_summary(data3, desc3, incl_sentences = incl_sentences)
        sum2 <- fst_length_summary(data2, desc2, incl_sentences = incl_sentences)
        sum1 <- fst_length_summary(data1, desc1, incl_sentences = incl_sentences)
        df <- rbind(sum1, sum2, sum3)
      }
    } else {
      sum2 <- fst_length_summary(data2, desc2, incl_sentences = incl_sentences)
      sum1 <- fst_length_summary(data1, desc1, incl_sentences = incl_sentences)
      df <- rbind(sum1, sum2)
    }
    df
  }




#' Make Comparison Cloud
#'
#' Creates a comparison wordcloud showing words that occur differently between
#' each group.
#'
#' @param data1 A dataframe of text in CoNLL-U format for the first group
#' @param data2 A dataframe of text in CoNLL-U format for the second group.
#' @param data3 An option dataframe of text in CoNLL-U format for the third
#' group, default is `NULL`
#' @param data4 An option dataframe of text in CoNLL-U format for the fourth
#' group, default is `NULL`
#' @param desc1 A string describing data1, default is `Group 1`
#' @param desc2 A string describing data2, default is `Group 2`
#' @param desc3 A string describing data3, default is `Group 3`
#' @param desc4 A string describing data4, default is `Group 4`
#' @param pos_filter #'  List of UPOS tags for inclusion, default is `NULL` which
#'  means all word types included
#' @param max The maximum number of words to display, default is `200`
#'
#' @return comparison cloud from wordcloud package
#' @export
#'
#' @examples
#' fst_comparisoncloud(conllu_dev_q11_1_nltk, conllu_dev_q11_2_nltk, conllu_dev_q11_3_nltk, pos_filter = c("NOUN", "VERB", "ADJ", "ADV"))
#' fst_comparisoncloud(conllu_bullying_iso, conllu_dev_q11_1_nltk, conllu_dev_q11_2_nltk, conllu_dev_q11_3_nltk,max = 400)
#' fst_comparisoncloud(conllu_dev_q11_1_f_nltk, conllu_dev_q11_1_m_nltk, conllu_dev_q11_1_na_nltk, desc1 = 'Female', desc2 = 'Male', desc3 = 'NA', max = 400)
#' fst_comparisoncloud(conllu_dev_q11_1_f_nltk, conllu_dev_q11_1_m_nltk, conllu_dev_q11_1_na_nltk, desc1 = 'Female', desc2 = 'Male', desc3 = 'NA', max = 200)
fst_comparisoncloud <- function(data1, data2, data3 = NULL, data4 = NULL, desc1 = "Group 1", desc2 = "Group 2", desc3 = "Group 3", desc4 = "Group 4", pos_filter = NULL, max = 200){
  if (!is.null(data3)) {
    if (!is.null(data4)) {
      if (!is.null(pos_filter)) {
        data1 <- dplyr::filter(data1, upos %in% pos_filter)
        data2 <- dplyr::filter(data2, upos %in% pos_filter)
        data3 <- dplyr::filter(data3, upos %in% pos_filter)
        data4 <- dplyr::filter(data4, upos %in% pos_filter)
      }
      data1 <- data1 %>%
        dplyr::filter(.data$dep_rel != "punct") %>%
        dplyr::filter(!is.na(lemma)) %>%
        dplyr::filter(lemma != 'na') %>%
        dplyr::count(lemma, sort = TRUE) %>%
        dplyr::rename(!!desc1 := n)
      data2 <- data2 %>%
        dplyr::filter(.data$dep_rel != "punct") %>%
        dplyr::filter(!is.na(lemma)) %>%
        dplyr::filter(lemma != 'na') %>%
        dplyr::count(lemma, sort = TRUE) %>%
        dplyr::rename(!!desc2 := n)
      data3 <- data3 %>%
        dplyr::filter(.data$dep_rel != "punct") %>%
        dplyr::filter(!is.na(lemma)) %>%
        dplyr::filter(lemma != 'na') %>%
        dplyr::count(lemma, sort = TRUE) %>%
        dplyr::rename(!!desc3 := n)
      data4 <- data4 %>%
        dplyr::filter(.data$dep_rel != "punct") %>%
        dplyr::filter(!is.na(lemma)) %>%
        dplyr::filter(lemma != 'na') %>%
        dplyr::count(lemma, sort = TRUE) %>%
        dplyr::rename(!!desc4 := n)
      compcloud_data <- dplyr::full_join(data1, data2, by ='lemma') %>%
        dplyr::full_join(data3, by ='lemma') %>%
        dplyr::full_join(data4, by ='lemma')
    } else {
      if (!is.null(pos_filter)) {
        data1 <- dplyr::filter(data1, upos %in% pos_filter)
        data2 <- dplyr::filter(data2, upos %in% pos_filter)
        data3 <- dplyr::filter(data3, upos %in% pos_filter)
      }
      data1 <- data1 %>%
        dplyr::filter(.data$dep_rel != "punct") %>%
        dplyr::filter(!is.na(lemma)) %>%
        dplyr::filter(lemma != 'na') %>%
        dplyr::count(lemma, sort = TRUE) %>%
        dplyr::rename(!!desc1 := n)
      data2 <- data2 %>%
        dplyr::filter(.data$dep_rel != "punct") %>%
        dplyr::filter(!is.na(lemma)) %>%
        dplyr::filter(lemma != 'na') %>%
        dplyr::count(lemma, sort = TRUE) %>%
        dplyr::rename(!!desc2 := n)
      data3 <- data3 %>%
        dplyr::filter(.data$dep_rel != "punct") %>%
        dplyr::filter(!is.na(lemma)) %>%
        dplyr::filter(lemma != 'na') %>%
        dplyr::count(lemma, sort = TRUE) %>%
        dplyr::rename(!!desc3 := n)
      compcloud_data <- dplyr::full_join(data1, data2, by ='lemma')
      compcloud_data <- dplyr::full_join(compcloud_data, data3, by ='lemma')
    }
  } else {
    if (!is.null(pos_filter)) {
      data1 <- dplyr::filter(data1, upos %in% pos_filter)
      data2 <- dplyr::filter(data2, upos %in% pos_filter)
    }
    data1 <- data1 %>%
      dplyr::filter(.data$dep_rel != "punct") %>%
      dplyr::filter(!is.na(lemma)) %>%
      dplyr::filter(lemma != 'na') %>%
      dplyr::count(lemma, sort = TRUE) %>%
      dplyr::rename(!!desc1 := n)
    data2 <- data2 %>%
      dplyr::filter(.data$dep_rel != "punct") %>%
      dplyr::filter(!is.na(lemma)) %>%
      dplyr::filter(lemma != 'na') %>%
      dplyr::count(lemma, sort = TRUE) %>%
      dplyr::rename(!!desc2 := n)
    compcloud_data <- dplyr::full_join(data1, data2, by ='lemma')
  }
  rownames(compcloud_data) <- compcloud_data$lemma
  compcloud_data$lemma <- NULL
  compcloud_data[is.na(compcloud_data)] <- 0
  par(mar = rep(0, 4))
  wordcloud::comparison.cloud(compcloud_data,
                       max.words= max,
                       random.order=FALSE,
                       rot.per=0.35,
                       colors=RColorBrewer::brewer.pal(8, "Dark2")
  )
}
