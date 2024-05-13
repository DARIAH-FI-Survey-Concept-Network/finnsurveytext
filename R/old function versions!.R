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
      message(paste0("Note: \n Consider whether your data is balanced between groups being compared and whether each group contains enough data for analysis. \n The number of responses in each group (including \'NAs\') are listed below: \n\t", name1, "=", num1, ", ", name2, "=", num2, ", ", name3, "=", num3, ", ", name4, "=", num4, "\n\n"))
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
      message(paste0("Note: \n Consider whether your data is balanced between groups being compared and whether each group contains enough data for analysis. \n The number of responses in each group (including \'NAs\') are listed below: \n\t", name1, "=", num1, ", ", name2, "=", num2, ", ", name3, "=", num3, "\n\n"))
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
    message(paste0("Note: \n Consider whether your data is balanced between groups being compared and whether each group contains enough data for analysis. \n The number of responses in each group (including \'NAs\') are listed below: \n\t", name1, "=", num1, ", ", name2, "=", num2, "\n\n"))
    unique <- fst_get_unique_ngrams(top1, top2)
    top2_2 <- fst_join_unique(top2, unique)
    top1_2 <- fst_join_unique(top1, unique)
    plot2 <- fst_ngrams_compare_plot(top2_2, number = number, unique_colour = unique_colour, override_title = name2)
    plot1 <- fst_ngrams_compare_plot(top1_2, number = number, unique_colour = unique_colour, override_title = name1)
    fst_plot_multiple(plot1 = plot1, plot2 = plot2, main_title = paste("Comparison Plot of", as.character(number), "Most Common Words"))
  }
  if (strict == TRUE) {
    message("Note:\n Words with equal occurrence are presented in alphabetical order. \n By default, words are presented in order to the `number` cutoff word. \n This means that equally-occurring later-alphabetically words beyond the cutoff will not be displayed. \n\n")
  } else {
    message("Note:\n Words with equal occurrence are presented in alphabetical order. \n With `strict` = FALSE, words occurring equally often as the `number` cutoff word will be displayed. \n\n")
  }
}


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
      message(paste0("Note: \n Consider whether your data is balanced between groups being compared and whether each group contains enough data for analysis. \n The number of responses in each group (including \'NAs\') are listed below: \n\t", name1, "=", num1, ", ", name2, "=", num2, ", ", name3, "=", num3, "\n\n"))
      top3 <- fst_get_top_ngrams2(data3, number = number, ngrams = ngrams, norm = norm, pos_filter = pos_filter, strict = strict)
      top2 <- fst_get_top_ngrams2(data2, number = number, ngrams = ngrams, norm = norm, pos_filter = pos_filter, strict = strict)
      top1 <- fst_get_top_ngrams2(data1, number = number, ngrams = ngrams, norm = norm, pos_filter = pos_filter, strict = strict)
    }
  } else {
    message(paste0("Note: \n Consider whether your data is balanced between groups being compared and whether each group contains enough data for analysis. \n The number of responses in each group (including \'NAs\') are listed below: \n\t", name1, "=", num1, ", ", name2, "=", num2, "\n\n"))
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
    message("Note:\n N-grams with equal occurrence are presented in alphabetical order. \n By default, n-grams are presented in order to the `number` cutoff n-gram \n This means that equally-occurring later-alphabetically n-grams beyond the cutoff n-gram will not be displayed. \n\n")
  } else {
    message("Note:\n N-grams with equal occurrence are presented in alphabetical order. \n With `strict` = FALSE, n-grams occurring equally often as the `number` cutoff n-gram will be displayed. \n\n")
  }
}
