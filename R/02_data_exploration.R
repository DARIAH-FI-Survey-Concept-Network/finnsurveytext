#' Make Simple Summary Table
#'
#' Creates a summary table for the input CoNLL-U data which provides the total
#' number of words, the number of unique words, and the number of unique lemmas.
#'
#' @param data A dataframe of text in CoNLL-U format.
#'
#' @return A dataframe with summary info for the data.
#' @export
#'
#' @examples
#' bullying_summary <- fst_summarise_short(conllu_bullying_iso)
#' q11_1_summary <- fst_summarise_short(conllu_dev_q11_1_nltk)
#' fst_summarise_short(conllu_dev_q11_2_nltk)
fst_summarise_short <- function(data) {
  data %>%
    dplyr::summarize(Respondents = dplyr::n_distinct(doc_id),
              'Total Words' = dplyr::n(),
              'Unique Words' = length(unique(token)),
              'Unique Lemmas' = length(unique(lemma)))
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

#' Make POS Summary Table
#'
#' Creates a summary table for the input CoNLL-U data which counts the number of
#' words of each part-of-speech tag within the data.
#'
#' @param data A dataframe of text in CoNLL-U format.
#'
#' @return A dataframe with a count of each UPOS tag in the data and the full
#'  name of the tag
#' @export
#'
#' @examples
#' bullying_pos_table <- fst_pos(conllu_bullying_iso)
#' q11_3_pos_table <- fst_pos(conllu_dev_q11_3_nltk)
#' fst_pos(conllu_dev_q11_1_snow)
fst_pos <- function(data) {
  pos_table <- data %>%
    dplyr::count(upos, sort = TRUE)
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
  df <- merge(x = pos_lookup, y = pos_table, by = "upos")
  df
}

#' Make Length Summary Table
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


#' Make Top Words Table
#'
#' Creates a table of the most frequently-occurring words (unigrams) within the
#' data.
#'
#' @param data A dataframe of text in CoNLL-U format.
#' @param number The number of top words to return, default is `10`.
#' @param pos_filter List of UPOS tags for inclusion, default is `NULL` which
#'  means all word types included
#' @param strict Whether to strictly cut-off at `number` (ties are
#'  alphabetically ordered), default is `TRUE`
#'
#' @return A table of the most frequently occurring words in the data
#' @export
#'
#' @examples
#' fst_get_top_words(conllu_dev_q11_1_nltk, number = 15)
#' fst_get_top_words(conllu_dev_q11_1_nltk, number = 15, strict = FALSE)
#' top_bullying_words <- fst_get_top_words(conllu_bullying, number = 15, pos_filter = c("NOUN", "VERB", "ADJ", "ADV"))
#' top_f <- fst_get_top_words(conllu_f_nltk, number = 15)
#' top_m <- fst_get_top_words(conllu_m_nltk, number = 15)
#' top_na <- fst_get_top_words(conllu_na_nltk, number = 15)
fst_get_top_words <- function(data, number = 10, pos_filter = NULL, strict = TRUE) {
  with_ties = !strict
  if (strict == TRUE) {
    message("Note:\n Terms with equal occurrence are presented in alphabetial order. \n By default, terms are presented in order to the `number` cutoff word. \n This means that equally-occurring later-alphabetically words beyond the cutoff will not be displayed. \n")
  } else {
    message("Note:\n Terms with equal occurrence are presented in alphabetial order. \n With `strict` = FALSE, words occurring equally often as the `number` cutoff word will be displayed. \n")
  }
  if (!is.null(pos_filter)) {
    data <- dplyr::filter(data, .data$upos %in% pos_filter)
  }
  data %>%
    dplyr::filter(.data$dep_rel != "punct") %>%
    dplyr::filter(!is.na(lemma)) %>%
    dplyr::filter(lemma != 'na') %>%
    dplyr::count(lemma, sort = TRUE) %>%
    dplyr::slice_max(n, n = number, with_ties = with_ties) %>%
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
#' @param strict Whether to strictly cut-off at `number` (ties are
#'  alphabetically ordered), default is `TRUE`
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
fst_get_top_ngrams <- function(data, number = 10, ngrams = 1, pos_filter = NULL, strict = TRUE){
  with_ties = !strict
  if (strict == TRUE) {
    message("Note:\n Terms with equal occurrence are presented in alphabetial order. \n By default, terms are presented in order to the `number` cutoff word. \n This means that equally-occurring later-alphabetically words beyond the cutoff will not be displayed. \n")
  } else {
    message("Note:\n Terms with equal occurrence are presented in alphabetial order. \n With `strict` = FALSE, words occurring equally often as the `number` cutoff word will be displayed. \n")
  }
  if (!is.null(pos_filter)) {
    data <- dplyr::filter(data, .data$upos %in% pos_filter)
  }
  data %>%
    dplyr::filter(.data$dep_rel != "punct") %>%
    dplyr::filter(!is.na(lemma)) %>%
    dplyr::filter(lemma != 'na') %>%
    dplyr::mutate(words = udpipe::txt_nextgram(lemma, n = ngrams)) %>%
    dplyr::count(words, sort = TRUE) %>%
    dplyr::slice_max(n, n = number, with_ties = with_ties) %>%
    dplyr::mutate(words = reorder(words, n)) %>%
    dplyr::filter(!is.na(words)) %>%
    dplyr::filter(words != 'na')
  }

#' Make Top N-grams Table 2
#'
#' Creates a table of the most frequently-occurring ngrams within the
#' data. Equivalent to `fst_get_top_ngrams` but does not print message.
#'
#' @param data A dataframe of text in CoNLL-U format.
#' @param number The number of n-grams to return, default is `10`.
#' @param ngrams The type of n-grams to return, default is `1`.
#' @param pos_filter List of UPOS tags for inclusion, default is `NULL` which
#'  means all word types included
#' @param strict Whether to strictly cut-off at `number` (ties are
#'  alphabetically ordered), default is `TRUE`
#'
#' @return A table of the most frequently occurring n-grams in the data.
#' @export
#'
#' @examples
#' fst_get_top_ngrams2(conllu_dev_q11_1_nltk)
#' fst_get_top_ngrams2(conllu_dev_q11_1_nltk, number = 10, ngrams = 1)
fst_get_top_ngrams2 <- function(data, number = 10, ngrams = 1, pos_filter = NULL, strict = TRUE){
  with_ties = !strict
  if (!is.null(pos_filter)) {
    data <- dplyr::filter(data, .data$upos %in% pos_filter)
  }
  data %>%
    dplyr::filter(.data$dep_rel != "punct") %>%
    dplyr::filter(!is.na(lemma)) %>%
    dplyr::filter(lemma != 'na') %>%
    dplyr::mutate(words = udpipe::txt_nextgram(lemma, n = ngrams)) %>%
    dplyr::count(words, sort = TRUE) %>%
    dplyr::slice_max(n, n = number, with_ties = with_ties) %>%
    dplyr::mutate(words = reorder(words, n)) %>%
    dplyr::filter(!is.na(words)) %>%
    dplyr::filter(words != 'na')
}


#' Make Top Words plot
#'
#' Plots most common words.
#'
#' @param table Output of `fst_get_top_words` or `fst_get_top_ngrams`
#' @param number The number of n-grams, default is `10`.
#' @param name An optional "name" for the plot, default is `NULL`
#'
#' @return Plot of top n-grams with unique terms highlighted.
#' @export
#'
#' @examples
#' fst_freq_plot(top_f, number = 15, name = 'Female')
#' fst_freq_plot(top_m)
#' fst_freq_plot(top_na, number = 15)
fst_freq_plot <- function(table, number = NULL, name = NULL, pos_filter = NULL) {
  table %>%
    ggplot2::ggplot(ggplot2::aes(n, words)) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_manual(values = colours, guide = "none") +
    ggplot2::labs(y = NULL, title = paste(name, as.character(number),"Most Common Words"))

}



#' Make N-grams plot
#'
#' Plots frequency n-grams.
#'
#' @param table Output of `fst_get_top_words` or `fst_get_top_ngrams`
#' @param number The number of n-grams for title, default is `NULL`.
#' @param ngrams The type of n-grams, default is `1`.
#' @param name An optional "name" for the plot, default is `NULL`

#'
#' @return Plot of top n-grams with unique terms highlighted.
#' @export
#'
#' @examples
#' fst_ngrams_plot(topn_f, ngrams = 2, name = 'Female')
#' fst_ngrams_plot(top_f, ngrams = 1, number = 15)
#' fst_ngrams_plot(topn_m, ngrams = 2, number = 15)
#' fst_ngrams_plot(topn_na, ngrams = 2)
fst_ngrams_plot <- function(table, number = NULL, ngrams = 1, name = NULL, pos_filter = NULL) {
  if (ngrams == 1) {
    term = 'Words'
  } else if (ngrams == 2) {
    term = 'Bigrams'
  } else {
    term = paste0(as.character(ngrams), "-grams")
  }
  table %>%
    ggplot2::ggplot(ggplot2::aes(n, words)) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_manual(values = colours, guide = "none") +
    ggplot2::labs(y = NULL, title = paste(name, as.character(number),"Most Common", term))
}


#' Find and Plot Top Words
#'
#' Creates a plot of the most frequently-occurring words (unigrams) within the
#' data.
#'
#' @param data A dataframe of text in CoNLL-U format.
#' @param number The number of top words to return, default is `10`.
#' @param pos_filter List of UPOS tags for inclusion, default is `NULL` which
#'  means all word types included
#' @param strict Whether to strictly cut-off at `number` (ties are
#'  alphabetically ordered), default is `TRUE`
#' @param name An optional "name" for the plot, default is `NULL`
#'
#' @return
#' @export
#'
#' @examples
#' fst_freq(conllu_dev_q11_1, number = 12, strict = FALSE, name = "All")
#' fst_freq(conllu_dev_q11_1_na, number = 15, name = "Not Spec")
fst_freq <- function(data, number = 10, pos_filter = NULL, strict = TRUE, name = NULL){
  words <- fst_get_top_words(data = data, number = number, pos_filter = pos_filter, strict = strict)
  fst_freq_plot(table = words, number = number, name = name, pos_filter = pos_filter)
}

#' Find and Plot Top N-grams
#'
#' Creates a plot of the most frequently-occurring n-grams within the
#' data.
#'
#' @param data A dataframe of text in CoNLL-U format.
#' @param number The number of top words to return, default is `10`.
#' @param ngrams The type of n-grams, default is `1`.
#' @param pos_filter List of UPOS tags for inclusion, default is `NULL` which
#'  means all word types included
#' @param strict Whether to strictly cut-off at `number` (ties are
#'  alphabetically ordered), default is `TRUE`
#' @param name An optional "name" for the plot, default is `NULL`
#'
#' @return
#' @export
#'
#' @examples
#' fst_ngrams(conllu_dev_q11_1, number = 12, ngrams = 2, strict = FALSE, name = "All")
#' fst_ngrams(conllu_dev_q11_1_na, number = 15, ngrams = 3, name = "Not Spec")
fst_ngrams <- function(data, number = 10, ngrams = 1, pos_filter = NULL, strict = TRUE, name = NULL){
  ngram_list <- fst_get_top_ngrams(data = data, number = number, ngrams = ngrams, pos_filter = pos_filter, strict = strict)
  fst_ngrams_plot(table = ngram_list, number = number, ngrams = ngrams, name = name, pos_filter = pos_filter)
}


#' Make Wordcloud
#'
#' Creates a wordcloud from CoNLL-U data of frequently-occuring words.
#'
#' @param data A dataframe of text in CoNLL-U format.
#' @param pos_filter List of UPOS tags for inclusion, default is `NULL` which
#'  means all word types included.
#'
#' @return A wordcloud from the data
#' @export
#'
#' @examples
#' fst_wordcloud(conllu_bullying_iso)
#' fst_wordcloud(conllu_bullying_iso, pos_filter = c("NOUN", "VERB", "ADJ", "ADV"))
#' fst_wordcloud(conllu_dev_q11_1_snow, pos_filter = "VERB")
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

#' Perform Data Exploration
#'
#' This summary function runs all the data exploration functions -
#' `fst_summarise`, `fst_pos`, `fst_freq`, `fst_ngrams`, and `fst_wordcloud`
#' within one step. These can be viewed within the `plots´ pane of RStudio by
#' using the left and right arrows to cycle between.
#'
#' @param data A dataframe of text in CoNLL-U format.
#' @param freq_number The number of top words to return, default is `20`.
#' @param ngram_number The number of top n-grams to return, default is `20`.
#' @param ngrams The type of n-grams to return, default is `2`.
#' @param pos_filter List of UPOS tags for inclusion, default is `NULL` which
#'  means all word types included.
#'
#' @return All the plots from the data exploration functions in the plots pane
#' @export
#'
#' @examples
#' fst_discover(conllu_bullying_iso)
#' fst_discover(conllu_dev_q11_1_nltk, freq_number = 10, ngram_number=8, ngrams = 3, pos_filter = c("NOUN", "VERB", "ADJ", "ADV"))
fst_discover <- function(data, freq_number = 20, ngram_number = 20,
                         ngrams = 2, pos_filter = NULL){
  plot.new()
  x <- fst_summarise(data)
  gridExtra::grid.table(x)
  plot.new()
  y <- fst_pos(data)
  gridExtra::grid.table(y)
  fst_wordcloud(data, pos_filter)
  return(list(fst_freq(data, number = freq_number, pos_filter=pos_filter),
              fst_ngrams(data, number = ngram_number, ngrams = ngrams)
  ))
}
