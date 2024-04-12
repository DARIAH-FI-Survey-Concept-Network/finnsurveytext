# Paino, incorporating weights into the data exploration functions
# In this document we manually add the weights but in paino2 we will incorporate from the start.


weightingsdf <- read.csv('/Users/adelineclarke/Documents/Uni Helsinki Work/2024/data/daF2821_fin.csv', sep = ';')

weightingsdf <- subset(weightingsdf, select= c(fsd_id, paino))

# We're going to do a potentially problematic join here as a POC. We will need to get the CoNLL-U dataframe to include a weight or link better
df <- conllu_dev_q11_3

# get the new id column
df$new_id <- substring(df$doc_id, 4)

df2 = merge(x = df, y = weightingsdf, by.x = "new_id", by.y = "fsd_id")

df2$paino <- as.numeric((gsub(",", ".", df2$paino)))


# id <- 'paino'
# df2$id <- as.numeric((gsub(",", ".", df2$id)))

#' Make Wordcloud VERSION2
#'
#' Creates a wordcloud from CoNLL-U data of frequently-occurring words, INCLUDING WEIGHTS.
#'
#' @param data A dataframe of text in CoNLL-U format.
#' @param pos_filter List of UPOS tags for inclusion, default is `NULL` which
#'  means all word types included.
#' @param max The maximum number of words to display, default is `100`
#'
#' @return A wordcloud from the data.
#' @export
#'
#' @examples
#' cb <- conllu_cb_bullying_iso
#' fst_wordcloud(cb)
#' fst_wordcloud(cb, pos_filter = c("NOUN", "VERB", "ADJ", "ADV"))
#' fst_wordcloud(conllu_dev_q11_1_snow, pos_filter = "VERB", max = 50)
#' fst_wordcloud(conllu_dev_q11_1_nltk)
fst_wordcloud_WEIGHTS <- function(data, pos_filter = NULL, max = 100, weight_col=NULL) {
  if (!is.null(pos_filter)) {
    data <- dplyr::filter(data, upos %in% pos_filter)
  }
  wordcloud_data <- data %>%
    dplyr::filter(.data$dep_rel != "punct") %>%
    dplyr::filter(!is.na(lemma)) %>%
    dplyr::filter(lemma != "na")
  if (is.null(weight_col)) {
    wordcloud_data <- dplyr::count(wordcloud_data, lemma, sort = TRUE)
  } else {
    wordcloud_data <- dplyr::count(wordcloud_data, lemma, sort = TRUE, wt = !!as.name(weight_col))
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
