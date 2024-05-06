# NOTES: Should be able to produce the same results using:
# a) svy_dev and fst_dev_coop_2 (or svy_child and fst_child_2)
# and b) fst_dev_coop (or fst_child)

# fst-* are in the data
child$paino <- as.numeric((gsub(",", ".", child$paino)))
svy_child <- svydesign(id=~1, weights= ~paino, data = child)

dev_coop$paino <- as.numeric((gsub(",", ".", dev_coop$paino)))
svy_dev <- svydesign(id = ~1, weights = ~paino, data = dev_coop)


### READY ###

#' Add weights to CoNLL-U data
#'
#' This function takes data in CoNLL-U format and a svydesign (from survey
#' packge) object with weights in it and merges the weights into the formatted
#' data.
#'
#' @param data A dataframe of text in CoNLL-U format
#' @param svydesign A svydesign object containing the raw data which produced
#'  the `data`
#' @param id ID column from raw data, must match the `docid` in formatted `data`
#'
#' @return A dataframe of text in CoNLL-U format plus a `'weight'` column
#' @export
#'
#' @examples
#' child$paino <- as.numeric((gsub(",", ".", child$paino)))
#' svy_child <- svydesign(id=~1, weights= ~paino, data = child)
#' fst_use_svydesign(data = fst_child_2, svydesign = svy_child, id = 'fsd_id')
#'
#' dev_coop$paino <- as.numeric((gsub(",", ".", dev_coop$paino)))
#' svy_dev <- svydesign(id = ~1, weights = ~paino, data = dev_coop)
#' fst_use_svydesign(data = fst_dev_coop_2, svydesign = svy_dev, id = 'fsd_id')
fst_use_svydesign <- function(data, svydesign, id) {
  weight_data <- svydesign$allprob
  colnames(weight_data) <- c("weight")
  weight_data['weight'] = 1/weight_data['weight']
  data2 <- svydesign$variables %>%
    dplyr::select(all_of(id))
  weight_data2 <- dplyr::bind_cols(data2, weight_data)
  annotated_data <- merge(x = data,
                          y = weight_data2,
                          by.x = 'doc_id',
                          by.y = id
  )
}

### READY ###

#' Make Wordcloud
#'
#' Creates a wordcloud from CoNLL-U data of frequently-occurring words, INCLUDING WEIGHTS.
#'
#' @param data A dataframe of text in CoNLL-U format.
#' @param pos_filter List of UPOS tags for inclusion, default is `NULL` which
#'  means all word types included.
#' @param max The maximum number of words to display, default is `100`.
#' @param use_svydesign_weights Option to weight words in the wordcloud using
#'  weights from  a svydesign object containing the raw data, default is `FALSE`
#' @param id ID column from raw data, required if `use_svydesign_weights = TRUE`
#'  and must match the `docid` in formatted `data`.
#' @param use_column_weights Option to weight words in the wordcloud using
#'  weights from  formatted data which includes addition `weight` column,
#'  default is `FALSE`.
#'
#' @return A wordcloud from the data.
#' @export
#'
#' @examples
#' fst_wordcloud(fst_child)
#' fst_wordcloud(fst_child, pos_filter = c("NOUN", "VERB", "ADJ", "ADV"))
#' fst_wordcloud(fst_child, use_column_weights = TRUE)
#' i <- 'fsd_id'
#' s <- svy_child
#' fst_wordcloud(fst_child, use_svydesign_weights = TRUE, id = i, svydesign = s)
fst_wordcloud <- function(data,
                          pos_filter = NULL,
                          max = 100,
                          use_svydesign_weights = FALSE,
                          id = "",
                          svydesign = NULL,
                          use_column_weights = FALSE) {
  if (use_svydesign_weights == TRUE) {
    fst_use_svydesign(data = data, svydesign = svydesign, id = id)
  }
  if (!is.null(pos_filter)) {
    data <- dplyr::filter(data, upos %in% pos_filter)
  }
  wordcloud_data <- data %>%
    dplyr::filter(.data$dep_rel != "punct") %>%
    dplyr::filter(!is.na(lemma)) %>%
    dplyr::filter(lemma != "na")
  if (use_svydesign_weights == TRUE) {
    wordcloud_data <- dplyr::count(wordcloud_data, lemma, sort = TRUE, wt = weight)
  } else if (use_column_weights == TRUE) {
    wordcloud_data <- dplyr::count(wordcloud_data, lemma, sort = TRUE, wt = weight)
  } else {
    wordcloud_data <- dplyr::count(wordcloud_data, lemma, sort = TRUE)
  }
  wordcloud::wordcloud(
    words = wordcloud_data$lemma,
    freq = wordcloud_data$n,
    max.words = max,
    random.order = FALSE,
    rot.per = 0.35,
    colors = RColorBrewer::brewer.pal(8, "Dark2")
  )
}


### BELOW IS NOT READY ###

#' Make Top Words Table WEIGHTS
#'
#' Creates a table of the most frequently-occurring words (unigrams) within the
#' data.
#'
#' @param data A dataframe of text in CoNLL-U format.
#' @param number The number of top words to return, default is `10`.
#' @param norm The method for normalising the data. Valid settings are
#'  `"number_words"` (the number of words in the responses, default),
#'  `"number_resp"` (the number of responses), or `NULL` (raw count returned).
#' @param pos_filter List of UPOS tags for inclusion, default is `NULL` which
#'  means all word types included.
#' @param strict Whether to strictly cut-off at `number` (ties are
#'  alphabetically ordered), default is `TRUE`.
#'
#' @return A table of the most frequently occurring words in the data.
#' @export
#'
#' @examples
#' fst_get_top_words(conllu_dev_q11_1_nltk, number = 15, strict = FALSE)
#' cb <- conllu_cb_bullying
#' pf <- c("NOUN", "VERB", "ADJ", "ADV")
#' fst_get_top_words(cb, number = 5, norm = "number_resp", pos_filter = pf)
fst_get_top_words_WEIGHTS <- function(data,
                              number = 10,
                              norm = "number_words",
                              pos_filter = NULL,
                              strict = TRUE,
                              weight_col = NULL) {
  with_ties <- !strict
  if (strict == TRUE) {
    message("Note:\n Words with equal occurrence are presented in alphabetical order. \n By default, words are presented in order to the `number` cutoff word. \n This means that equally-occurring later-alphabetically words beyond the cutoff word will not be displayed.\n\n")
  } else {
    message("Note:\n Words with equal occurrence are presented in alphabetical order. \n With `strict` = FALSE, words occurring equally often as the `number` cutoff word will be displayed. \n\n")
  }
  if (is.null(norm)) {
    denom <- 1
  } else if (norm == "number_words") {
    data %>%
      dplyr::filter(.data$dep_rel != "punct") %>%
      dplyr::filter(!is.na(lemma)) %>%
      dplyr::filter(lemma != "na")
    denom <- nrow(data)
  } else if (norm == "number_resp") {
    denom <- dplyr::n_distinct(data$doc_id)
  } else if (norm == "use_weights") {
    denom <- 1
  } else {
    message("NOTE: A recognised normalisation method has not been provided. \n Function has defaulted to normalisation method 'number_of_words'")
    data %>%
      dplyr::filter(.data$dep_rel != "punct") %>%
      dplyr::filter(!is.na(lemma)) %>%
      dplyr::filter(lemma != "na")
    denom <- nrow(data)
  }
  if (!is.null(pos_filter)) {
    data <- dplyr::filter(data, .data$upos %in% pos_filter)
  }
  data <- data %>%
    dplyr::filter(.data$dep_rel != "punct") %>%
    dplyr::filter(!is.na(lemma)) %>%
    dplyr::filter(lemma != "na")
  if (is.null(weight_col)) {
    data <- dplyr::count(data, lemma, sort = TRUE)
  } else {
    data <- dplyr::count(data, lemma, sort = TRUE, wt = !!as.name(weight_col))
  }
  data %>%
    dplyr::mutate(n = round(n / denom, 3)) %>%
    dplyr::slice_max(n, n = number, with_ties = with_ties) %>%
    dplyr::mutate(lemma = reorder(lemma, n)) %>%
    dplyr::rename(words = lemma, occurrence = n)
}

#' Make Top N-grams Table WEIGHTS
#'
#' Creates a table of the most frequently-occurring n-grams within the
#' data.
#'
#' @param data A dataframe of text in CoNLL-U format.
#' @param number The number of n-grams to return, default is `10`.
#' @param ngrams The type of n-grams to return, default is `1`.
#' @param norm The method for normalising the data. Valid settings are
#'  `"number_words"` (the number of words in the responses, default),
#'  `"number_resp"` (the number of responses), or `NULL` (raw count returned).
#' @param pos_filter List of UPOS tags for inclusion, default is `NULL` which
#'  means all word types included.
#' @param strict Whether to strictly cut-off at `number` (ties are
#'  alphabetically ordered), default is `TRUE`.
#'
#' @return A table of the most frequently occurring n-grams in the data.
#' @export
#'
#' @examples
#' q11_1 <- conllu_dev_q11_1_nltk
#' fst_get_top_ngrams(q11_1, norm = NULL)
#' fst_get_top_ngrams(q11_1, number = 10, ngrams = 1, norm = "number_resp")
#' cb <- conllu_cb_bullying
#' pf <- c("NOUN", "VERB", "ADJ", "ADV")
#' fst_get_top_ngrams(cb, number = 15, pos_filter = pf)
fst_get_top_ngrams_WEIGHTS <- function(data,
                               number = 10,
                               ngrams = 1,
                               norm = "number_words",
                               pos_filter = NULL,
                               strict = TRUE,
                               weight_col = NULL) {
  with_ties <- !strict
  if (strict == TRUE) {
    message("Note:\n N-grams with equal occurrence are presented in alphabetical order. \n By default, n-grams are presented in order to the `number` cutoff n-gram. \n This means that equally-occurring later-alphabetically n-grams beyond the cutoff n-gram will not be displayed. \n\n")
  } else {
    message("Note:\n N-grams with equal occurrence are presented in alphabetical order. \n With `strict` = FALSE, n-grams occurring equally often as the `number` cutoff n-gram will be displayed. \n\n")
  }
  if (is.null(norm)) {
    denom <- 1
  } else if (norm == "number_words") {
    data %>%
      dplyr::filter(.data$dep_rel != "punct") %>%
      dplyr::filter(!is.na(lemma)) %>%
      dplyr::filter(lemma != "na")
    denom <- nrow(data)
  } else if (norm == "number_resp") {
    denom <- dplyr::n_distinct(data$doc_id)
  } else if (norm == "use_weights") {
    denom <- 1
  } else {
    message("NOTE: A recognised normalisation method has not been provided. \n Function has defaulted to normalisation method 'number_of_words'")
    data %>%
      dplyr::filter(.data$dep_rel != "punct") %>%
      dplyr::filter(!is.na(lemma)) %>%
      dplyr::filter(lemma != "na")
    denom <- nrow(data)
  }
  if (!is.null(pos_filter)) {
    data <- dplyr::filter(data, .data$upos %in% pos_filter)
  }
  data <- data %>%
    dplyr::filter(.data$dep_rel != "punct") %>%
    dplyr::filter(!is.na(lemma)) %>%
    dplyr::filter(lemma != "na") %>%
    dplyr::mutate(words = udpipe::txt_nextgram(lemma, n = ngrams))
  if (is.null(weight_col)) {
    data <- dplyr::count(data, words, sort = TRUE)
  } else {
    data <- dplyr::count(data, words, sort = TRUE, wt = !!as.name(weight_col))
  }
  data %>%
    dplyr::mutate(n = round(n / denom, 3)) %>%
    dplyr::slice_max(n, n = number, with_ties = with_ties) %>%
    dplyr::mutate(words = reorder(words, n)) %>%
    dplyr::filter(!is.na(words)) %>%
    dplyr::filter(words != "na") %>%
    dplyr::rename(occurrence = n)
}

#' Make Top N-grams Table 2 WEIGHTS
#'
#' Creates a table of the most frequently-occurring n-grams within the
#' data.
#'
#' @param data A dataframe of text in CoNLL-U format.
#' @param number The number of n-grams to return, default is `10`.
#' @param ngrams The type of n-grams to return, default is `1`.
#' @param norm The method for normalising the data. Valid settings are
#'  `"number_words"` (the number of words in the responses, default),
#'  `"number_resp"` (the number of responses), or `NULL` (raw count returned).
#' @param pos_filter List of UPOS tags for inclusion, default is `NULL` which
#'  means all word types included.
#' @param strict Whether to strictly cut-off at `number` (ties are
#'  alphabetically ordered), default is `TRUE`.
#'
#' @return A table of the most frequently occurring n-grams in the data.
#' @export
#'
#' @examples
#' q11_1 <- conllu_dev_q11_1_nltk
#' fst_get_top_ngrams(q11_1, norm = NULL)
#' fst_get_top_ngrams(q11_1, number = 10, ngrams = 1, norm = "number_resp")
#' cb <- conllu_cb_bullying
#' pf <- c("NOUN", "VERB", "ADJ", "ADV")
#' fst_get_top_ngrams(cb, number = 15, pos_filter = pf)
fst_get_top_ngrams2_WEIGHTS <- function(data,
                                       number = 10,
                                       ngrams = 1,
                                       norm = "number_words",
                                       pos_filter = NULL,
                                       strict = TRUE,
                                       weight_col = NULL) {
  with_ties <- !strict
  if (is.null(norm)) {
    denom <- 1
  } else if (norm == "number_words") {
    data %>%
      dplyr::filter(.data$dep_rel != "punct") %>%
      dplyr::filter(!is.na(lemma)) %>%
      dplyr::filter(lemma != "na")
    denom <- nrow(data)
  } else if (norm == "number_resp") {
    denom <- dplyr::n_distinct(data$doc_id)
  } else if (norm == "use_weights") {
    denom <- 1
  } else {
    message("NOTE: A recognised normalisation method has not been provided. \n Function has defaulted to normalisation method 'number_of_words'")
    data %>%
      dplyr::filter(.data$dep_rel != "punct") %>%
      dplyr::filter(!is.na(lemma)) %>%
      dplyr::filter(lemma != "na")
    denom <- nrow(data)
  }
  if (!is.null(pos_filter)) {
    data <- dplyr::filter(data, .data$upos %in% pos_filter)
  }
  data <- data %>%
    dplyr::filter(.data$dep_rel != "punct") %>%
    dplyr::filter(!is.na(lemma)) %>%
    dplyr::filter(lemma != "na") %>%
    dplyr::mutate(words = udpipe::txt_nextgram(lemma, n = ngrams))
  if (is.null(weight_col)) {
    data <- dplyr::count(data, words, sort = TRUE)
  } else {
    data <- dplyr::count(data, words, sort = TRUE, wt = !!as.name(weight_col))
  }
  data %>%
    dplyr::mutate(n = round(n / denom, 3)) %>%
    dplyr::slice_max(n, n = number, with_ties = with_ties) %>%
    dplyr::mutate(words = reorder(words, n)) %>%
    dplyr::filter(!is.na(words)) %>%
    dplyr::filter(words != "na") %>%
    dplyr::rename(occurrence = n)
}

#' Find and Plot Top Words WEIGHTS
#'
#' Creates a plot of the most frequently-occurring words (unigrams) within the
#' data.
#'
#' @param data A dataframe of text in CoNLL-U format.
#' @param number The number of top words to return, default is `10`.
#' @param norm The method for normalising the data. Valid settings are
#'  `"number_words"` (the number of words in the responses, default),
#'  `"number_resp"` (the number of responses), or `NULL` (raw count returned).
#' @param pos_filter List of UPOS tags for inclusion, default is `NULL` which
#'  means all word types included.
#' @param strict Whether to strictly cut-off at `number` (ties are
#'  alphabetically ordered), default is `TRUE`.
#' @param name An optional "name" for the plot to add to title, default is
#'  `NULL`.
#'
#' @return Plot of top words.
#' @export
#'
#' @examples
#' q11_1 <- conllu_dev_q11_1
#' n1 <- "number_resp"
#' fst_freq(q11_1, number = 12, norm = n1, strict = FALSE, name = "All")
#' fst_freq(q11_1, number = 15, name = "Not Spec")
fst_freq_WEIGHTS <- function(data,
                     number = 10,
                     norm = "number_words",
                     pos_filter = NULL,
                     strict = TRUE,
                     name = NULL,
                     weight_col = NULL) {
  words <- fst_get_top_words_WEIGHTS(
    data = data,
    number = number,
    norm = norm,
    pos_filter = pos_filter,
    strict = strict,
    weight_col = weight_col
  )
  fst_freq_plot(table = words, number = number, name = name)
}


#' Find and Plot Top N-grams
#'
#' Creates a plot of the most frequently-occurring n-grams within the
#' data.
#'
#' @param data A dataframe of text in CoNLL-U format.
#' @param number The number of top words to return, default is `10`.
#' @param ngrams The type of n-grams, default is `1`.
#' @param norm The method for normalising the data. Valid settings are
#'  `"number_words"` (the number of words in the responses, default),
#'  `"number_resp"` (the number of responses), or `NULL` (raw count returned).
#' @param pos_filter List of UPOS tags for inclusion, default is `NULL` which
#'  means all word types included.
#' @param strict Whether to strictly cut-off at `number` (ties are
#'  alphabetically ordered), default is `TRUE`.
#' @param name An optional "name" for the plot to add to title, default is
#'  `NULL`.
#'
#' @return Plot of top n-grams
#' @export
#'
#' @examples
#' q11_1 <- conllu_dev_q11_1
#' fst_ngrams(q11_1, 12, ngrams = 2, norm = NULL, strict = FALSE, name = "All")
#' fst_ngrams(conllu_dev_q11_1_na, number = 15, ngrams = 3, name = "Not Spec")
fst_ngrams_WEIGHTS <- function(data,
                       number = 10,
                       ngrams = 1,
                       norm = "number_words",
                       pos_filter = NULL,
                       strict = TRUE,
                       name = NULL,
                       weight_col = NULL) {
  ngram_list <- fst_get_top_ngrams_WEIGHTS(
    data = data,
    number = number,
    ngrams = ngrams,
    norm = norm,
    pos_filter = pos_filter,
    strict = strict,
    weight_col = weight_col
  )
  fst_ngrams_plot(
    table = ngram_list,
    number = number,
    ngrams = ngrams,
    name = name
  )
}
