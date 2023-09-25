
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

get_top_ngrams <- function(data, number = 20, ngrams = 2, pos_filter = NULL){
  if (!is.null(pos_filter)) {
    data <- dplyr::filter(data, .data$upos %in% pos_filter)
  }
  data %>%
    dplyr::filter(.data$dep_rel != "punct") %>%
    dplyr::filter(!is.na(lemma)) %>%
    dplyr::filter(lemma != 'na') %>%
    dplyr::mutate(ngrams = udpipe::txt_nextgram(lemma, n = ngrams)) %>%
    dplyr::count(ngrams, sort = TRUE) %>%
    dplyr::slice_max(n, n = number) %>%
    dplyr::mutate(ngrams = reorder(ngrams, n)) %>%
    dplyr::filter(!is.na(ngrams)) %>%
    dplyr::filter(ngrams != 'na')
}


# ggplot2::ggplot(ggplot2::aes(n, lemma)) +
#   ggplot2::geom_col() +
#   ggplot2::labs(y = NULL, title = paste(as.character(number),"Most Common Words"))


# fst_freq(conllu_dev_q11_1_nltk, 10, c("NOUN", "VERB", "ADJ", "ADV"))


get_special_words <- function(df1, df2, ...) {
  df <- rbind(df1, df2, ...)
  df <- df %>%
    dplyr::mutate(n = 1) %>%
    dplyr::group_by(ngrams) %>%
    dplyr::summarise(n = sum(n)) %>%
    dplyr::mutate(n = ifelse(n == 1, "yes", "no")) %>%
    dplyr::rename(unique_word = n)
  df
}

# spec <- get_special_words(q1, q2)
# q1_2 <- q1 %>% dplyr::left_join(spec, by='lemma')
# q2_2 <- q2 %>% dplyr::left_join(spec, by = 'lemma')

fst_freq_plot <- function(table, number, ngrams = 1, unique_colour = 'indianred', name = NULL) {
  colours <- c("yes" = unique_colour, "no" = "grey50")
  if (ngrams == 1) {
    term = 'Words'
  } else if (ngrams == 2) {
    term = 'Bigrams'
  } else {
    term = paste0(as.character(ngrams), "-grams")
  }
  table %>%
    ggplot2::ggplot(ggplot2::aes(n, ngrams, fill = unique_word)) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_manual(values = colours, guide = "none") +
    ggplot2::labs(y = NULL, title = paste(name, as.character(number),"Most Common", term))
}

# pl1 <- fst_freq_plot(q1_2, 10)
# pl2 <- fst_freq_plot(q2_2, 10)
# fst_freq_plot(q1_2, 10, ngrams = 3)


fst_plot_compare <- function(plot1, plot2, plot3 = NULL, plot4 = NULL, number = 20) {
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

# fst_plot_compare(pl1, pl2)
compare_unigrams <- function(data1, data2, data3 = NULL, data4 = NULL, number = 20, pos_filter = NULL, name1 = NULL, name2 = NULL, name3 = NULL, name4 = NULL, unique_colour = 'indianred') {
  if (!is.null(data3)){
    if (!is.null(data4)){
      top4 <- get_top_words(data4, number = number, pos_filter = pos_filter)
      top3 <- get_top_words(data3, number = number, pos_filter = pos_filter)
      top2 <- get_top_words(data2, number = number, pos_filter = pos_filter)
      top1 <- get_top_words(data1, number = number, pos_filter = pos_filter)
    } else {
      top3 <- get_top_words(data3, number = number, pos_filter = pos_filter)
      top2 <- get_top_words(data2, number = number, pos_filter = pos_filter)
      top1 <- get_top_words(data1, number = number, pos_filter = pos_filter)
    }
  } else {
    top2 <- get_top_words(data2, number = number, pos_filter = pos_filter)
    top1 <- get_top_words(data1, number = number, pos_filter = pos_filter)
  }
  if (!is.null(data3)){
    if (!is.null(data4)){
      unique <- get_special_words(top1, top2, top3, top4)
      top4_2 <- top4 %>% dplyr::left_join(unique, by='lemma')
      top3_2 <- top3 %>% dplyr::left_join(unique, by='lemma')
      top2_2 <- top2 %>% dplyr::left_join(unique, by='lemma')
      top1_2 <- top1 %>% dplyr::left_join(unique, by='lemma')
      plot4 <- fst_freq_plot(top4_2, number = number, unique_colour = unique_colour, name = name4)
      plot3 <- fst_freq_plot(top3_2, number = number, unique_colour = unique_colour, name = name3)
      plot2 <- fst_freq_plot(top2_2, number = number, unique_colour = unique_colour, name = name2)
      plot1 <- fst_freq_plot(top1_2, number = number, unique_colour = unique_colour, name = name1)
      fst_plot_compare(plot1 = plot1, plot2 = plot2, plot3 = plot3, plot4 = plot4, number = number)
    } else {
      unique <- get_special_words(top1, top2, top3)
      top3_2 <- top3 %>% dplyr::left_join(unique, by='lemma')
      top2_2 <- top2 %>% dplyr::left_join(unique, by='lemma')
      top1_2 <- top1 %>% dplyr::left_join(unique, by='lemma')
      plot3 <- fst_freq_plot(top3_2, number = number, unique_colour = unique_colour, name = name3)
      plot2 <- fst_freq_plot(top2_2, number = number, unique_colour = unique_colour, name = name2)
      plot1 <- fst_freq_plot(top1_2, number = number, unique_colour = unique_colour, name = name1)
      fst_plot_compare(plot1 = plot1, plot2 = plot2, plot3 = plot3, number = number)
    }
  } else {
    unique <- get_special_words(top1, top2)
    top2_2 <- top2 %>% dplyr::left_join(unique, by='lemma')
    top1_2 <- top1 %>% dplyr::left_join(unique, by='lemma')
    plot2 <- fst_freq_plot(top2_2, number = number, unique_colour = unique_colour, name = name2)
    plot1 <- fst_freq_plot(top1_2, number = number, unique_colour = unique_colour, name = name1)
    fst_plot_compare(plot1 = plot1, plot2 = plot2, number = number)
  }
}

compare_ngrams <- function(data1, data2, data3 = NULL, data4 = NULL, number = 20, ngrams = 1, pos_filter = NULL, name1 = NULL, name2 = NULL, name3 = NULL, name4 = NULL, unique_colour = 'indianred') {
  if (!is.null(data3)){
    if (!is.null(data4)){
      top4 <- get_top_ngrams(data4, number = number, ngrams = ngrams, pos_filter = pos_filter)
      top3 <- get_top_ngrams(data3, number = number, ngrams = ngrams, pos_filter = pos_filter)
      top2 <- get_top_ngrams(data2, number = number, ngrams = ngrams, pos_filter = pos_filter)
      top1 <- get_top_ngrams(data1, number = number, ngrams = ngrams, pos_filter = pos_filter)
    } else {
      top3 <- get_top_ngrams(data3, number = number, ngrams = ngrams, pos_filter = pos_filter)
      top2 <- get_top_ngrams(data2, number = number, ngrams = ngrams, pos_filter = pos_filter)
      top1 <- get_top_ngrams(data1, number = number, ngrams = ngrams, pos_filter = pos_filter)
    }
  } else {
    top2 <- get_top_ngrams(data2, number = number, ngrams = ngrams, pos_filter = pos_filter)
    top1 <- get_top_ngrams(data1, number = number, ngrams = ngrams, pos_filter = pos_filter)
  }
  if (!is.null(data3)){
    if (!is.null(data4)){
      unique <- get_special_words(top1, top2, top3, top4)
      top4_2 <- top4 %>% dplyr::left_join(unique, by='ngrams')
      top3_2 <- top3 %>% dplyr::left_join(unique, by='ngrams')
      top2_2 <- top2 %>% dplyr::left_join(unique, by='ngrams')
      top1_2 <- top1 %>% dplyr::left_join(unique, by='ngrams')
      plot4 <- fst_freq_plot(top4_2, number = number, ngrams = ngrams, unique_colour = unique_colour, name = name4)
      plot3 <- fst_freq_plot(top3_2, number = number, ngrams = ngrams, unique_colour = unique_colour, name = name3)
      plot2 <- fst_freq_plot(top2_2, number = number, ngrams = ngrams, unique_colour = unique_colour, name = name2)
      plot1 <- fst_freq_plot(top1_2, number = number, ngrams = ngrams, unique_colour = unique_colour, name = name1)
      fst_plot_compare(plot1 = plot1, plot2 = plot2, plot3 = plot3, plot4 = plot4, number = number)
    } else {
      unique <- get_special_words(top1, top2, top3)
      top3_2 <- top3 %>% dplyr::left_join(unique, by='ngrams')
      top2_2 <- top2 %>% dplyr::left_join(unique, by='ngrams')
      top1_2 <- top1 %>% dplyr::left_join(unique, by='ngrams')
      plot3 <- fst_freq_plot(top3_2, number = number, ngrams = ngrams, unique_colour = unique_colour, name = name3)
      plot2 <- fst_freq_plot(top2_2, number = number, ngrams = ngrams, unique_colour = unique_colour, name = name2)
      plot1 <- fst_freq_plot(top1_2, number = number, ngrams = ngrams, unique_colour = unique_colour, name = name1)
      fst_plot_compare(plot1 = plot1, plot2 = plot2, plot3 = plot3, number = number)
    }
  } else {
    unique <- get_special_words(top1, top2)
    top2_2 <- top2 %>% dplyr::left_join(unique, by='ngrams')
    top1_2 <- top1 %>% dplyr::left_join(unique, by='ngrams')
    plot2 <- fst_freq_plot(top2_2, number = number, ngrams = ngrams, unique_colour = unique_colour, name = name2)
    plot1 <- fst_freq_plot(top1_2, number = number, ngrams = ngrams, unique_colour = unique_colour, name = name1)
    fst_plot_compare(plot1 = plot1, plot2 = plot2, number = number)
  }
}

# compare_unigrams(conllu_dev_q11_1_nltk, conllu_dev_q11_2_nltk, number = 10)
# compare_unigrams(conllu_dev_q11_1_nltk, conllu_dev_q11_2_nltk, conllu_dev_q11_3_nltk, conllu_dev_q11_2_nltk,number = 10)
# compare_unigrams(conllu_dev_q11_1_nltk, conllu_dev_q11_2_nltk, conllu_dev_q11_3_nltk,number = 15, unique_colour = 'pink', pos_filter = c("NOUN", "VERB", "ADJ", "ADV"), name1 = 'Q11_1', name2 = 'Q11_2', name3 = 'Q11_3')
# compare_unigrams(conllu_m, conllu_f, conllu_s ,number = 10, unique_colour = 'slateblue', pos_filter = c("NOUN", "VERB", "ADJ", "ADV"), name1 = 'Male', name2 = 'Female', name3 = 'Not Specified')
# compare_unigrams(conllu_m, conllu_f, number = 10, unique_colour = 'slateblue', pos_filter = c("NOUN", "VERB", "ADJ", "ADV"), name1 = 'Male', name2 = 'Female')
# compare_unigrams(conllu_m, conllu_f, number = 15, unique_colour = 'slateblue', pos_filter = c("NOUN", "VERB", "ADJ", "ADV"), name1 = 'Male', name2 = 'Female')
#
# compare_ngrams(conllu_dev_q11_1_nltk, conllu_dev_q11_2_nltk, number = 10)
# compare_ngrams(conllu_dev_q11_1_nltk, conllu_dev_q11_2_nltk, conllu_dev_q11_3_nltk, conllu_dev_q11_2_nltk,number = 10)
# compare_ngrams(conllu_dev_q11_1_nltk, conllu_dev_q11_2_nltk, conllu_dev_q11_3_nltk,number = 15, unique_colour = 'pink', pos_filter = c("NOUN", "VERB", "ADJ", "ADV"), name1 = 'Q11_1', name2 = 'Q11_2', name3 = 'Q11_3')
# compare_ngrams(conllu_m, conllu_f, conllu_s ,number = 10, unique_colour = 'slateblue', pos_filter = c("NOUN", "VERB", "ADJ", "ADV"), name1 = 'Male', name2 = 'Female', name3 = 'Not Specified')
# compare_ngrams(conllu_m, conllu_f, number = 10, unique_colour = 'slateblue', pos_filter = c("NOUN", "VERB", "ADJ", "ADV"), name1 = 'Male', name2 = 'Female')
# compare_ngrams(conllu_m, conllu_f, ngrams = 1, number = 15, unique_colour = 'slateblue', pos_filter = c("NOUN", "VERB", "ADJ", "ADV"), name1 = 'Male', name2 = 'Female')
# compare_ngrams(conllu_m, conllu_f, number = 20, ngrams = 2)
# compare_ngrams(conllu_m, conllu_f, number = 5, ngrams = 3, unique_colour = "black", name1 = 'Male', name2 = 'Female')



fst_wordcloud2 <- function(table, number, ngrams = 1, unique_colour = 'indianred', name = NULL) {
  colours <- c("yes" = unique_colour, "no" = "grey50")
  if (ngrams == 1) {
    term = 'Words'
  } else if (ngrams == 2) {
    term = 'Bigrams'
  } else {
    term = paste0(as.character(ngrams), "-grams")
  }
  table %>%
    ggplot2::ggplot(ggplot2::aes(n, ngrams, fill = unique_word)) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_manual(values = colours, guide = "none") +
    ggplot2::labs(y = NULL, title = paste(name, as.character(number),"Most Common", term))
}

fst_wordcloud <- function(data, pos_filter = NULL){
  if (!is.null(pos_filter)) {
    data <- dplyr::filter(data, upos %in% pos_filter)
  }
  wordcloud_data <- data %>%
    dplyr::filter(.data$dep_rel != "punct") %>%
    dplyr::filter(!is.na(lemma)) %>%
    dplyr::filter(lemma != 'na') %>%
    dplyr::count(lemma, sort = TRUE)
  par(mar = rep(0, 4))
  wordcloud::wordcloud(words = wordcloud_data$lemma,
                       freq = wordcloud_data$n,
                       max.words= 100,
                       random.order=FALSE,
                       rot.per=0.35,
                       colors=RColorBrewer::brewer.pal(8, "Dark2")
  )
}



# fst_wordcloud_compare <- function(data1, data2, data3 = NULL, data4 = NULL, pos_filter = NULL, name1, name2, name3, name4) {
#   if (!is.null(data3)) {
#     if (!is.null(data4)) {
#       par(mfrow=c(2,2))
#       fst_wordcloud(data1, pos_filter = pos_filter)
#       fst_wordcloud(data2, pos_filter = pos_filter)
#       fst_wordcloud(data3, pos_filter = pos_filter)
#       fst_wordcloud(data4, pos_filter = pos_filter)
#     } else {
#       par(mfrow=c(1,3))
#       fst_wordcloud(data1, pos_filter = pos_filter)
#       fst_wordcloud(data2, pos_filter = pos_filter)
#       fst_wordcloud(data3, pos_filter = pos_filter)
#     }
#   } else {
#     par(mfrow=c(1,2))
#     fst_wordcloud(data1, pos_filter = pos_filter)
#     title(main = "Sample Word Cloud with Title")
#     fst_wordcloud(data2, pos_filter = pos_filter)
#   }
# }
# https://rpubs.com/brandonkopp/creating-word-clouds-in-r



fst_pos_comparison <- function(data1, data2, data3 = NULL, data4 = NULL, rename1 = "n1", rename2 = "n2", rename3 = "n3", rename4 = "n4") {
  pos_lookup <- data.frame('upos' =c('ADJ', 'ADP', 'ADV', 'AUX', 'CCONJ', 'DET',
                                     'INTJ', 'NOUN', 'NUM', 'PART', 'PRON',
                                     'PROPN', 'PUNCT', 'SCONJ', 'SYM', 'VERB',
                                     'X'),
                           'upos name' = c(' adjective', ' adposition',
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
      pos_table4 <- data4 %>% dplyr::count(upos, sort = TRUE) %>% dplyr::rename(!!rename4 := n)
      pos_table3 <- data3 %>% dplyr::count(upos, sort = TRUE) %>% dplyr::rename(!!rename3 := n)
      pos_table2 <- data2 %>% dplyr::count(upos, sort = TRUE) %>% dplyr::rename(!!rename2 := n)
      pos_table1 <- data1 %>% dplyr::count(upos, sort = TRUE) %>% dplyr::rename(!!rename1 := n)
      df <- merge(x = pos_lookup, y = pos_table1, by = "upos") %>%
        merge(pos_table2, by = "upos") %>%
        merge(pos_table3, by = "upos") %>%
        merge(pos_table4, by = "upos")
    } else {
      pos_table3 <- data3 %>% dplyr::count(upos, sort = TRUE) %>% dplyr::rename(!!rename3 := n)
      pos_table2 <- data2 %>% dplyr::count(upos, sort = TRUE) %>% dplyr::rename(!!rename2 := n)
      pos_table1 <- data1 %>% dplyr::count(upos, sort = TRUE) %>% dplyr::rename(!!rename1 := n)
      df <- merge(x = pos_lookup, y = pos_table1, by = "upos") %>%
        merge(pos_table2, by = "upos") %>%
        merge(pos_table3, by = "upos")
    }
  } else {
    pos_table2 <- data2 %>% dplyr::count(upos, sort = TRUE) %>% dplyr::rename(!!rename2 := n)
    pos_table1 <- data1 %>% dplyr::count(upos, sort = TRUE) %>% dplyr::rename(!!rename1 := n)
    df <- merge(x = pos_lookup, y = pos_table1, by = "upos") %>%
      merge(pos_table2, by = "upos")
  }
  df
}
# fst_pos_comparison(conllu_m, conllu_f, conllu_s, conllu_dev_q11_1, "Male", "Female", "No Gender Specified", "All")

fst_summarise2 <- function(data, value = 'All respondents') {
  df <- data %>%
    dplyr::summarize('Description' = value,
                     'Respondents' = dplyr::n_distinct(doc_id),
                     'Total Words' = dplyr::n(),
                     'Unique Words' = length(unique(token)),
                     'Unique Lemmas' = length(unique(lemma)))
  df
}

fst_summarise_compare <- function(data1, data2, data3 = NULL, data4 = NULL, desc1 = "Group 1", desc2 = "Group 2", desc3 = "Group 3", desc4 = "Group 4") {
  if (!is.null(data3)) {
    if (!is.null(data4)) {
      sum4 <- fst_summarise2(data4, desc4)
      sum3 <- fst_summarise2(data3, desc3)
      sum2 <- fst_summarise2(data2, desc2)
      sum1 <- fst_summarise2(data1, desc1)
      df <- rbind(sum1, sum2, sum3, sum4)
    } else {
      sum3 <- fst_summarise2(data3, desc3)
      sum2 <- fst_summarise2(data2, desc2)
      sum1 <- fst_summarise2(data1, desc1)
      df <- rbind(sum1, sum2, sum3)
    }
  } else {
    sum2 <- fst_summarise2(data2, desc2)
    sum1 <- fst_summarise2(data1, desc1)
    df <- rbind(sum1, sum2)
  }
  df
}
# fst_summarise_compare(conllu_m, conllu_f, conllu_s, conllu_dev_q11_1_nltk, "Male", "Female", "No Gender Specified", "All")
# fst_summarise_compare(conllu_m, conllu_f)
# fst_summarise_compare(conllu_m, conllu_f,  desc1 = "Male", desc2 = "Female")
