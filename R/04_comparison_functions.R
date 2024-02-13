#' Get unique n-grams
#'
#' Takes at least two tables of n-grams and frequencies (either output of
#' `fst_get_top_words()` or `fst_get_top_ngrams()`) and finds n-grams unique to
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
#' top_f <- fst_get_top_words(conllu_dev_q11_1_f_nltk)
#' top_m <- fst_get_top_words(conllu_dev_q11_1_m_nltk)
#' top_na <- fst_get_top_words(conllu_dev_q11_1_na_nltk)
#' topn_f <- fst_get_top_ngrams(conllu_dev_q11_1_f_nltk)
#' topn_m <- fst_get_top_ngrams(conllu_dev_q11_1_m_nltk)
#' topn_na <- fst_get_top_ngrams(conllu_dev_q11_1_na_nltk)
#' fst_get_unique_ngrams(top_f, top_m, top_na)
#' fst_get_unique_ngrams(topn_f, topn_m, topn_na)
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


#' Merge N-grams table with unique words
#'
#' Merges list of unique words from `fst_get_unique_ngrams()` with output of
#' `fst_get_top_ngrams()` or `fst_get_top_words()` so that unique words can be
#' displayed on comparison plots.
#'
#' @param table Output of `fst_get_top_words()` or `fst_get_top_ngrams()`.
#' @param unique_table Output of `fst_get_unique_ngrams()`.
#'
#' @return A table of top n-grams, frequency, and whether the n-gram is
#'  "unique".
#' @export
#'
#' @examples
#' top_f <- fst_get_top_words(conllu_dev_q11_1_f_nltk)
#' top_m <- fst_get_top_words(conllu_dev_q11_1_m_nltk)
#' top_na <- fst_get_top_words(conllu_dev_q11_1_na_nltk)
#' topn_f <- fst_get_top_ngrams(conllu_dev_q11_1_f_nltk)
#' topn_m <- fst_get_top_ngrams(conllu_dev_q11_1_m_nltk)
#' topn_na <- fst_get_top_ngrams(conllu_dev_q11_1_na_nltk)
#' unique_words <- fst_get_unique_ngrams(top_f, top_m, top_na)
#' unique_ngrams <- fst_get_unique_ngrams(topn_f, topn_m, topn_na)
#' fst_join_unique(top_f, unique_words)
#' fst_join_unique(topn_m, unique_ngrams)
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
#' top_f <- fst_get_top_words(conllu_dev_q11_1_f_nltk)
#' top_m <- fst_get_top_words(conllu_dev_q11_1_m_nltk)
#' top_na <- fst_get_top_words(conllu_dev_q11_1_na_nltk)
#' topn_f <- fst_get_top_ngrams(conllu_dev_q11_1_f_nltk)
#' topn_m <- fst_get_top_ngrams(conllu_dev_q11_1_m_nltk)
#' topn_na <- fst_get_top_ngrams(conllu_dev_q11_1_na_nltk)
#' unique_words <- fst_get_unique_ngrams(top_f, top_m, top_na)
#' unique_ngrams <- fst_get_unique_ngrams(topn_f, topn_m, topn_na)
#' top_fu <- fst_join_unique(top_f, unique_words)
#' topn_mu <- fst_join_unique(topn_m, unique_ngrams)
#' fst_ngrams_compare_plot(top_fu, ngrams = 1, name = "Female")
#' fst_ngrams_compare_plot(topn_mu, ngrams = 2, name = "Male")
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



#' Display comparison plots
#'
#' Display between 2 and 4 plots within the plots pane. If 2 or 3 plots, they
#' will be in a single row, if there are 4 plots, they will be in 2 rows of 2.
#'
#' @param plot1 First plot to display.
#' @param plot2 Second plot to display.
#' @param plot3 Optional third plot to display, defaul is `NULL`.
#' @param plot4 Optional fourth plot to display, defaul is `NULL`.
#' @param main_title An optional title for the set of plots. The default is
#'  `NULL` and no main title will be included.
#'
#' @return Up to 4 plots within the plots pane.
#' @export
#'
#' @examples
#' top_f <- fst_get_top_words(conllu_dev_q11_1_f_nltk)
#' top_m <- fst_get_top_words(conllu_dev_q11_1_m_nltk)
#' top_na <- fst_get_top_words(conllu_dev_q11_1_na_nltk)
#' topn_f <- fst_get_top_ngrams(conllu_dev_q11_1_f_nltk)
#' topn_m <- fst_get_top_ngrams(conllu_dev_q11_1_m_nltk)
#' topn_na <- fst_get_top_ngrams(conllu_dev_q11_1_na_nltk)
#' unique_words <- fst_get_unique_ngrams(top_f, top_m, top_na)
#' unique_ngrams <- fst_get_unique_ngrams(topn_f, topn_m, topn_na)
#' top_fu <- fst_join_unique(top_f, unique_words)
#' top_mu <- fst_join_unique(top_m, unique_words)
#' top_nau <- fst_join_unique(top_na, unique_words)
#' p1 <- fst_ngrams_compare_plot(top_fu, ngrams = 1, name = "Female")
#' p2 <- fst_ngrams_compare_plot(top_mu, ngrams = 1, name = "Male")
#' p3 <- fst_ngrams_compare_plot(top_nau, ngrams = 1, name = "Not Spec")
#' fst_plot_multiple(p1, p2, p3, main_title = "Comparison Plots")
#' fst_plot_multiple(p1, p1)
fst_plot_multiple <- function(plot1, plot2, plot3 = NULL, plot4 = NULL, main_title = NULL) {
  if (!is.null(plot3)) {
    if (!is.null(plot4)) {
      gridExtra::grid.arrange(plot1, plot2, plot3, plot4, ncol = 2, top = ggpubr::text_grob(main_title, size = 15, face = "bold"))
    } else {
      gridExtra::grid.arrange(plot1, plot2, plot3, ncol = 3, top = ggpubr::text_grob(main_title, size = 15, face = "bold"))
    }
  } else {
    gridExtra::grid.arrange(plot1, plot2, ncol = 2, top = ggpubr::text_grob(main_title, size = 15, face = "bold"))
  }
}


#' Compare and plot top words
#'
#' Find top and unique top words for between 2 and 4 sets of prepared data.
#' Results will be shown within the plots pane. If 2 or 3 plots, they will be in
#' a single row, if there are 4 plots, they will be in 2 rows of 2.
#'
#' @param data1 A dataframe of text in CoNLL-U format for the first plot.
#' @param data2 A dataframe of text in CoNLL-U format for the second plot.
#' @param data3 An optional dataframe of text in CoNLL-U format for the third
#'  plot, default is `NULL`.
#' @param data4 An optional dataframe of text in CoNLL-U format for the fourth
#'  plot, default is `NULL`.
#' @param number The number of top words to return, default is `10`.
#' @param norm The method for normalising the data. Valid settings are
#'  `"number_words"` (the number of words in the responses, default),
#'  `"number_resp"` (the number of responses), or `NULL` (raw count returned).
#' @param pos_filter List of UPOS tags for inclusion, default is `NULL` which
#'  means all word types included.
#' @param name1 An optional "name" for the first plot, default is `"Group 1"`.
#' @param name2 An optional "name" for the second plot, default is `"Group 2"`.
#' @param name3 An optional "name" for the third plot, default is `"Group 3"`.
#' @param name4 An optional "name" for the fourth plot, default is `"Group 4"`.
#' @param unique_colour Colour to display unique words, default is
#'  `"indianred"`.
#' @param strict Whether to strictly cut-off at `number` (ties are
#'  alphabetically ordered), default is `TRUE`.
#'
#' @return Between 2 and 4 plots of Top n-grams in the plots pane with unique
#' n-grams highlighted.
#' @export
#'
#' @examples
#' f <- conllu_dev_q11_1_f_nltk
#' m <- conllu_dev_q11_1_m_nltk
#' na <- conllu_dev_q11_1_na_nltk
#' fst_freq_compare(f, m, number = 10)
#' fst_freq_compare(f, m, na, number = 5, norm = "number_resp")
#' fst_freq_compare(f, m, na, name1 = "F", name2 = "M", name3 = "NA")
#' fst_freq_compare(f, m, na, strict = FALSE)
fst_freq_compare <- function(data1, data2, data3 = NULL, data4 = NULL, number = 10, norm = "number_words", pos_filter = NULL, name1 = "Group 1", name2 = "Group 2", name3 = "Group 3", name4 = "Group 4", unique_colour = "indianred", strict = TRUE) {
  if (!is.null(data3)) {
    if (!is.null(data4)) {
      top4 <- fst_get_top_ngrams2(data4, number = number, norm = norm, pos_filter = pos_filter, strict = strict)
      top3 <- fst_get_top_ngrams2(data3, number = number, norm = norm, pos_filter = pos_filter, strict = strict)
      top2 <- fst_get_top_ngrams2(data2, number = number, norm = norm, pos_filter = pos_filter, strict = strict)
      top1 <- fst_get_top_ngrams2(data1, number = number, norm = norm, pos_filter = pos_filter, strict = strict)
    } else {
      top3 <- fst_get_top_ngrams2(data3, number = number, norm = norm, pos_filter = pos_filter, strict = strict)
      top2 <- fst_get_top_ngrams2(data2, number = number, norm = norm, pos_filter = pos_filter, strict = strict)
      top1 <- fst_get_top_ngrams2(data1, number = number, norm = norm, pos_filter = pos_filter, strict = strict)
    }
  } else {
    top2 <- fst_get_top_ngrams2(data2, number = number, norm = norm, pos_filter = pos_filter, strict = strict)
    top1 <- fst_get_top_ngrams2(data1, number = number, norm = norm, pos_filter = pos_filter, strict = strict)
  }
  num1 <- dplyr::n_distinct(data1$doc_id)
  num2 <- dplyr::n_distinct(data2$doc_id)
  if (!is.null(data3)) {
    num3 <- dplyr::n_distinct(data3$doc_id)
    if (!is.null(data4)) {
      num4 <- dplyr::n_distinct(data4$doc_id)
      message(paste0("Note: \n Consider whether your data is balanced between groups being compared and whether each group contains enough data for analysis. \n The number of responded in each group (including \'NAs\') are listed below: \n\t", name1, "=", num1, ", ", name2, "=", num2, ", ", name3, "=", num3, ", ", name4, "=", num4, "\n\n"))
      unique <- fst_get_unique_ngrams(top1, top2, top3, top4)
      top4_2 <- fst_join_unique(top4, unique)
      top3_2 <- fst_join_unique(top3, unique)
      top2_2 <- fst_join_unique(top2, unique)
      top1_2 <- fst_join_unique(top1, unique)
      plot4 <- fst_ngrams_compare_plot(top4_2, number = number, unique_colour = unique_colour, override_title = name4)
      plot3 <- fst_ngrams_compare_plot(top3_2, number = number, unique_colour = unique_colour, override_title = name3)
      plot2 <- fst_ngrams_compare_plot(top2_2, number = number, unique_colour = unique_colour, override_title = name2)
      plot1 <- fst_ngrams_compare_plot(top1_2, number = number, unique_colour = unique_colour, override_title = name1)
      fst_plot_multiple(plot1 = plot1, plot2 = plot2, plot3 = plot3, plot4 = plot4, main_title = paste("Comparison Plot of", as.character(number), "Most Common Words"))
    } else {
      message(paste0("Note: \n Consider whether your data is balanced between groups being compared and whether each group contains enough data for analysis. \n The number of responded in each group (including \'NAs\') are listed below: \n\t", name1, "=", num1, ", ", name2, "=", num2, ", ", name3, "=", num3, "\n\n"))
      unique <- fst_get_unique_ngrams(top1, top2, top3)
      top3_2 <- fst_join_unique(top3, unique)
      top2_2 <- fst_join_unique(top2, unique)
      top1_2 <- fst_join_unique(top1, unique)
      plot3 <- fst_ngrams_compare_plot(top3_2, number = number, unique_colour = unique_colour, override_title = name3)
      plot2 <- fst_ngrams_compare_plot(top2_2, number = number, unique_colour = unique_colour, override_title = name2)
      plot1 <- fst_ngrams_compare_plot(top1_2, number = number, unique_colour = unique_colour, override_title = name1)
      fst_plot_multiple(plot1 = plot1, plot2 = plot2, plot3 = plot3, main_title = paste("Comparison Plot of", as.character(number), "Most Common Words"))
    }
  } else {
    message(paste0("Note: \n Consider whether your data is balanced between groups being compared and whether each group contains enough data for analysis. \n The number of responded in each group (including \'NAs\') are listed below: \n\t", name1, "=", num1, ", ", name2, "=", num2, "\n\n"))
    unique <- fst_get_unique_ngrams(top1, top2)
    top2_2 <- fst_join_unique(top2, unique)
    top1_2 <- fst_join_unique(top1, unique)
    plot2 <- fst_ngrams_compare_plot(top2_2, number = number, unique_colour = unique_colour, override_title = name2)
    plot1 <- fst_ngrams_compare_plot(top1_2, number = number, unique_colour = unique_colour, override_title = name1)
    fst_plot_multiple(plot1 = plot1, plot2 = plot2, main_title = paste("Comparison Plot of", as.character(number), "Most Common Words"))
  }
  if (strict == TRUE) {
    message("Note:\n Words with equal occurrence are presented in alphabetial order. \n By default, words are presented in order to the `number` cutoff word. \n This means that equally-occurring later-alphabetically words beyond the cutoff will not be displayed. \n\n")
  } else {
    message("Note:\n Words with equal occurrence are presented in alphabetial order. \n With `strict` = FALSE, words occurring equally often as the `number` cutoff word will be displayed. \n\n")
  }
}

#' Compare and plot top n-grams
#'
#' Find top and unique top n-grams for between 2 and 4 sets of prepared data.
#' Results will be shown within the plots pane. If 2 or 3 plots, they will be in
#' a single row, if there are 4 plots, they will be in 2 rows of 2.
#'
#' @param data1 A dataframe of text in CoNLL-U format for the first plot.
#' @param data2 A dataframe of text in CoNLL-U format for the second plot.
#' @param data3 An optional dataframe of text in CoNLL-U format for the third
#'  plot, default is `NULL`.
#' @param data4 An optional dataframe of text in CoNLL-U format for the fourth
#'  plot, default is `NULL`.
#' @param number The number of n-grams to return, default is `10`.
#' @param ngrams The type of n-grams to return, default is `1`.
#' @param norm The method for normalising the data. Valid settings are
#'  `"number_words"` (the number of words in the responses, default),
#'  `"number_resp"` (the number of responses), or `NULL` (raw count returned).
#' @param pos_filter List of UPOS tags for inclusion, default is `NULL` which
#'  means all word types included.
#' @param name1 An optional "name" for the first plot, default is `"Group 1"`.
#' @param name2 An optional "name" for the second plot, default is `"Group 2"`.
#' @param name3 An optional "name" for the third plot, default is `"Group 3"`.
#' @param name4 An optional "name" for the fourth plot, default is `"Group 4"`.
#' @param unique_colour Colour to display unique words, default is
#'  `"indianred"`.
#' @param strict Whether to strictly cut-off at `number` (ties are
#'  alphabetically ordered), default is `TRUE`.
#'
#' @return Between 2 and 4 plots of Top n-grams in the plots pane with unique
#' n-grams highlighted.
#' @export
#'
#' @examples
#' f <- conllu_dev_q11_1_f_nltk
#' m <- conllu_dev_q11_1_m_nltk
#' na <- conllu_dev_q11_1_na_nltk
#' all <- conllu_dev_q11_1_nltk
#' fst_ngrams_compare(f, m, na, all, number = 10, strict = FALSE)
#' fst_ngrams_compare(f, m, ngrams = 2, number = 10, norm = "number_resp")
#' fst_ngrams_compare(f, m, ngrams = 2, number = 10, strict = FALSE)
#' fst_ngrams_compare(f, m, number = 5, ngrams = 3, name1 = "M", name2 = "F")
#' fst_ngrams_compare(f, m, na, number = 20, unique_colour = "slateblue", )
fst_ngrams_compare <- function(data1, data2, data3 = NULL, data4 = NULL, number = 10, ngrams = 1, norm = "number_words", pos_filter = NULL, name1 = "Group 1", name2 = "Group 2", name3 = "Group 3", name4 = "Group 4", unique_colour = "indianred", strict = TRUE) {
  if (ngrams == 1) {
    term <- "Words"
  } else if (ngrams == 2) {
    term <- "Bigrams"
  } else {
    term <- paste0(as.character(ngrams), "-grams")
  }
  num1 <- dplyr::n_distinct(data1$doc_id)
  num2 <- dplyr::n_distinct(data2$doc_id)
  if (!is.null(data3)) {
    num3 <- dplyr::n_distinct(data3$doc_id)
    if (!is.null(data4)) {
      num4 <- dplyr::n_distinct(data4$doc_id)
      message(paste0("Note: \n Consider whether your data is balanced between groups being compared and whether each group contains enough data for analysis. \n The number of responses in each group (including \'NAs\') are listed below: \n\t", name1, "=", num1, ", ", name2, "=", num2, ", ", name3, "=", num3, ", ", name4, "=", num4, "\n\n"))
      top4 <- fst_get_top_ngrams2(data4, number = number, ngrams = ngrams, norm = norm, pos_filter = pos_filter, strict = strict)
      top3 <- fst_get_top_ngrams2(data3, number = number, ngrams = ngrams, norm = norm, pos_filter = pos_filter, strict = strict)
      top2 <- fst_get_top_ngrams2(data2, number = number, ngrams = ngrams, norm = norm, pos_filter = pos_filter, strict = strict)
      top1 <- fst_get_top_ngrams2(data1, number = number, ngrams = ngrams, norm = norm, pos_filter = pos_filter, strict = strict)
    } else {
      message(paste0("Note: \n Consider whether your data is balanced between groups being compared and whether each group contains enough data for analysis. \n The number of responded in each group (including \'NAs\') are listed below: \n\t", name1, "=", num1, ", ", name2, "=", num2, ", ", name3, "=", num3, "\n\n"))
      top3 <- fst_get_top_ngrams2(data3, number = number, ngrams = ngrams, norm = norm, pos_filter = pos_filter, strict = strict)
      top2 <- fst_get_top_ngrams2(data2, number = number, ngrams = ngrams, norm = norm, pos_filter = pos_filter, strict = strict)
      top1 <- fst_get_top_ngrams2(data1, number = number, ngrams = ngrams, norm = norm, pos_filter = pos_filter, strict = strict)
    }
  } else {
    message(paste0("Note: \n Consider whether your data is balanced between groups being compared and whether each group contains enough data for analysis. \n The number of responded in each group (including \'NAs\') are listed below: \n\t", name1, "=", num1, ", ", name2, "=", num2, "\n\n"))
    top2 <- fst_get_top_ngrams2(data2, number = number, ngrams = ngrams, norm = norm, pos_filter = pos_filter, strict = strict)
    top1 <- fst_get_top_ngrams2(data1, number = number, ngrams = ngrams, norm = norm, pos_filter = pos_filter, strict = strict)
  }
  if (!is.null(data3)) {
    if (!is.null(data4)) {
      unique <- fst_get_unique_ngrams(top1, top2, top3, top4)
      top4_2 <- fst_join_unique(top4, unique)
      top3_2 <- fst_join_unique(top3, unique)
      top2_2 <- fst_join_unique(top2, unique)
      top1_2 <- fst_join_unique(top1, unique)
      plot4 <- fst_ngrams_compare_plot(top4_2, number = number, ngrams = ngrams, unique_colour = unique_colour, override_title = name4)
      plot3 <- fst_ngrams_compare_plot(top3_2, number = number, ngrams = ngrams, unique_colour = unique_colour, override_title = name3)
      plot2 <- fst_ngrams_compare_plot(top2_2, number = number, ngrams = ngrams, unique_colour = unique_colour, override_title = name2)
      plot1 <- fst_ngrams_compare_plot(top1_2, number = number, ngrams = ngrams, unique_colour = unique_colour, override_title = name1)
      fst_plot_multiple(plot1 = plot1, plot2 = plot2, plot3 = plot3, plot4 = plot4, main_title = paste("Comparison Plot of", as.character(number), "Most Common", term))
    } else {
      unique <- fst_get_unique_ngrams(top1, top2, top3)
      top3_2 <- fst_join_unique(top3, unique)
      top2_2 <- fst_join_unique(top2, unique)
      top1_2 <- fst_join_unique(top1, unique)
      plot3 <- fst_ngrams_compare_plot(top3_2, number = number, ngrams = ngrams, unique_colour = unique_colour, override_title = name3)
      plot2 <- fst_ngrams_compare_plot(top2_2, number = number, ngrams = ngrams, unique_colour = unique_colour, override_title = name2)
      plot1 <- fst_ngrams_compare_plot(top1_2, number = number, ngrams = ngrams, unique_colour = unique_colour, override_title = name1)
      fst_plot_multiple(plot1 = plot1, plot2 = plot2, plot3 = plot3, main_title = paste("Comparison Plot of", as.character(number), "Most Common", term))
    }
  } else {
    unique <- fst_get_unique_ngrams(top1, top2)
    top2_2 <- fst_join_unique(top2, unique)
    top1_2 <- fst_join_unique(top1, unique)
    plot2 <- fst_ngrams_compare_plot(top2_2, number = number, ngrams = ngrams, unique_colour = unique_colour, override_title = name2)
    plot1 <- fst_ngrams_compare_plot(top1_2, number = number, ngrams = ngrams, unique_colour = unique_colour, override_title = name1)
    fst_plot_multiple(plot1 = plot1, plot2 = plot2, main_title = paste("Comparison Plot of", as.character(number), "Most Common", term))
  }
  if (strict == TRUE) {
    message("Note:\n N-grams with equal occurrence are presented in alphabetial order. \n By default, n-grams are presented in order to the `number` cutoff n-gram \n This means that equally-occurring later-alphabetically n-grams beyond the cutoff n-gram will not be displayed. \n\n")
  } else {
    message("Note:\n N-grams with equal occurrence are presented in alphabetial order. \n With `strict` = FALSE, n-grams occurring equally often as the `number` cutoff n-gram will be displayed. \n\n")
  }
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
      message(paste0("Note: \n Consider whether your data is balanced between groups being compared and whether each group contains enough data for analysis. \n The number of responded in each group (including \'NAs\') are listed below: \n\t", name1, "=", num1, ", ", name2, "=", num2, ", ", name3, "=", num3, ", ", name4, "=", num4, "\n\n"))
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
      message(paste0("Note: \n Consider whether your data is balanced between groups being compared and whether each group contains enough data for analysis. \n The number of responded in each group (including \'NAs\') are listed below: \n\t", name1, "=", num1, ", ", name2, "=", num2, ", ", name3, "=", num3, "\n\n"))
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
    message(paste0("Note: \n Consider whether your data is balanced between groups being compared and whether each group contains enough data for analysis. \n The number of responded in each group (including \'NAs\') are listed below: \n\t", name1, "=", num1, ", ", name2, "=", num2, "\n\n"))
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
