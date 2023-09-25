#' Make Summary Table
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
#' bullying_summary <- fst_summarise(conllu_bullying_iso)
#' q11_1_summary <- fst_summarise(conllu_dev_q11_1_nltk)
#' fst_summarise(conllu_dev_q11_2_nltk)
fst_summarise <- function(data) {
  data %>%
    dplyr::summarize(Respondents = dplyr::n_distinct(doc_id),
              'Total Words' = dplyr::n(),
              'Unique Words' = length(unique(token)),
              'Unique Lemmas' = length(unique(lemma)))
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


#' Make Top Words Table
#'
#' Creates a table of the most frequently-occurring words (unigrams) within the
#' data.
#'
#' @param data A dataframe of text in CoNLL-U format.
#' @param number The number of top words to return, default is `20`.
#' @param pos_filter List of UPOS tags for inclusion, default is `NULL` which
#'  means all word types included
#'
#' @return A table of the most frequent words (unigrams) in the data
#' @export
#'
#' @examples
#' fst_freq(conllu_bullying_iso)
#' fst_freq(conllu_bullying_iso, 10, c("NOUN", "VERB", "ADJ", "ADV"))
#' fst_freq(conllu_dev_q11_1_nltk, 15, "NOUN")
#' fst_freq(conllu_dev_q11_1_snow, 15, "ADV")
fst_freq <- function(data, number = 20, pos_filter = NULL) {
  if (!is.null(pos_filter)) {
    data <- dplyr::filter(data, .data$upos %in% pos_filter)
  }
  data %>%
    dplyr::filter(.data$dep_rel != "punct") %>%
    dplyr::filter(!is.na(lemma)) %>%
    dplyr::filter(lemma != 'na') %>%
    dplyr::count(lemma, sort = TRUE) %>%
    dplyr::slice_max(n, n = number) %>%
    dplyr::mutate(lemma = reorder(lemma, n)) %>%
    ggplot2::ggplot(ggplot2::aes(n, lemma)) +
    ggplot2::geom_col() +
    ggplot2::labs(y = NULL, title = paste(as.character(number),"Most Common Words"))
}


#' Make Top N-Grams Table
#'
#' Creates a table of the most frequently-occurring sets of words (n-grams)
#' within the data.
#'
#' @param data A dataframe of text in CoNLL-U format.
#' @param number The number of top n-grams to return, default is `20`.
#' @param ngrams The type of n-grams to return, default is `2`.
#'
#' @return A table of the most frequent n-grams in the data
#' @export
#'
#' @examples
#' fst_ngrams(conllu_bullying_iso)
#' fst_ngrams(conllu_dev_q11_2_nltk, number = 10, ngrams=3)
#' fst_ngrams(conllu_dev_q11_3_nltk, ngrams = 4)
fst_ngrams <- function(data, number = 20, ngrams = 2){
  data %>%
    dplyr::filter(.data$dep_rel != "punct") %>%
    dplyr::filter(!is.na(lemma)) %>%
    dplyr::filter(lemma != 'na') %>%
    dplyr::mutate(n_gram = udpipe::txt_nextgram(lemma, n = ngrams)) %>%
    dplyr::count(n_gram, sort = TRUE) %>%
    dplyr::slice_max(n, n = number) %>%
    dplyr::mutate(n_gram = reorder(n_gram, n)) %>%
    ggplot2::ggplot(ggplot2::aes(n, n_gram)) +
    ggplot2::geom_col() +
    if (ngrams == 2) {
      ggplot2::labs(y = NULL, title = paste0(as.character(number), " Most Common Bigrams"))
    } else {
      ggplot2::labs(y = NULL, title = paste0(as.character(number), " Most Common ", as.character(ngrams), "-grams"))
    }
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
#' fst_discover(bullying_data)
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

