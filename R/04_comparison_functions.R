
#' fst_freq(conllu_bullying_iso)
#' fst_freq(conllu_bullying_iso, 10, c("NOUN", "VERB", "ADJ", "ADV"))
#' fst_freq(conllu_dev_q11_1_nltk, 15, "NOUN")
#' fst_freq(conllu_dev_q11_1_snow, 15, "ADV")
get_top_words <- function(data, number = 20, pos_filter = NULL) {
  if (!is.null(pos_filter)) {
    data <- dplyr::filter(data, .data$upos %in% pos_filter)
  }
  data %>%
    dplyr::filter(.data$dep_rel != "punct") %>%
    dplyr::filter(!is.na(lemma)) %>%
    dplyr::filter(lemma != 'na') %>%
    dplyr::count(lemma, sort = TRUE) %>%
    dplyr::slice_max(n, n = number) %>%
    dplyr::mutate(lemma = reorder(lemma, n))
}


# ggplot2::ggplot(ggplot2::aes(n, lemma)) +
#   ggplot2::geom_col() +
#   ggplot2::labs(y = NULL, title = paste(as.character(number),"Most Common Words"))


fst_freq(conllu_dev_q11_1_nltk, 10, c("NOUN", "VERB", "ADJ", "ADV"))


get_special_words <- function(df1, df2, ...) {
  df <- rbind(df1, df2, ...)
  df <- df %>%
    dplyr::mutate(n = 1) %>%
    dplyr::group_by(lemma) %>%
    dplyr::summarise(n = sum(n)) %>%
    dplyr::mutate(n = ifelse(n == 1, "yes", "no")) %>%
    dplyr::rename(unique_word = n)
  df
}

spec <- get_special_words(q1, q2)
q1_2 <- q1 %>% dplyr::left_join(spec, by='lemma')
q2_2 <- q2 %>% dplyr::left_join(spec, by = 'lemma')

fst_freq_plot <- function(table, number) {
  table %>%
    ggplot2::ggplot(ggplot2::aes(n, lemma, fill = unique_word)) +
    ggplot2::scale_colour_manual(values = c("yes" = "blue", "no" = "black")) +
    ggplot2::geom_col() +
    ggplot2::labs(y = NULL, title = paste(as.character(number),"Most Common Words"))
}

pl1 <- fst_freq_plot(q1_2, 10)
pl2 <- fst_freq_plot(q2_2, 10)



fst_plot_compare <- function(plot1, plot2, plot3 = NULL, plot4 = NULL, number = 10) {
  if (!is.null(plot3)) {
    if (!is.null(plot4)) {
      gridExtra::grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)
    } else {
      gridExtra::grid.arrange(plot1, plot2, plot3, ncol = 3)
    }
  } else {
    gridExtra::grid.arrange(plot1, plot2, ncol = 2)
  }
}

fst_plot_compare(pl1, pl2)










s1 <- setdiff(q1$lemma, q2$lemma)
s2 <- setdiff(q2$lemma, q1$lemma)
s <- s1 + s2

s <- c(q1$lemma, q2$lemma)
df <- data.frame(s)
df %<% group_by(s)
df2<- dplyr::group_by(df, s)



