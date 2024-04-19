#' Annotate open-ended survey responses in Finnish into CoNLL-U format
#'
#' Creates a dataframe in CoNLL-U format from a list of strings of Finnish text
#' using the [udpipe] package and a Finnish language model.
#'
#' @param data A dataframe of survey responses which contains an open-ended
#'  question.
#' @param question The column in the dataframe which contains the open-ended
#'  question.
#' @param id The column in the dataframe which contains the ids for the
#'  responses.
#' @param model A Finnish language model available for [udpipe], `"ftb"`
#'  (default) or `"tdt"`.
#' @param weights Optional, the column of the dataframe which contains the
#'  respective weights for each response.
#' @param add_cols Optional, a column (or columns) from the dataframe which contain
#'  other information you'd like to retain (for instance, dimension columnns for
#'  splitting the data for comparison plots).
#'
#' @return Dataframe of annotated text in CoNLL-U format.
#' @export
#'
#' @examples
#' \donttest{
#' i <- "fsd_id"
#' fst_format(data = child, question = "q7", id = i)
#' fst_format(data = child, question = "q7", id = i, model = "tdt")
#' fst_format(data = child, question = "q7", id = i, weights="paino")
#' fst_format(child, question = "q7", id = i, add_cols = c("bv1", "bv3"))
#' unlink("finnish-ftb-ud-2.5-191206.udpipe")
#' unlink("finnish-tdt-ud-2.5-191206.udpipe")
#' }
fst_format <- function(data,
                       question,
                       id,
                       model = "ftb",
                       weights = NULL,
                       add_cols = NULL) {
  data <- data %>%
    dplyr::mutate(new_col = trimws(.data[[question]])) %>%
    dplyr::mutate_if(is.character, dplyr::na_if, "")
  if (model == "ftb") {
    if (!file.exists("finnish-ftb-ud-2.5-191206.udpipe")) {
      udpipe::udpipe_download_model(language = "finnish-ftb")
    }
    model_ftb <- udpipe::udpipe_load_model(
      file = "finnish-ftb-ud-2.5-191206.udpipe"
    )
    annotated_data <- as.data.frame(
      udpipe::udpipe_annotate(model_ftb, x = data$new_col, doc_id = data[[id]])
    )
  } else if (model == "tdt") {
    if (!file.exists("finnish-tdt-ud-2.5-191206.udpipe")) {
      udpipe::udpipe_download_model(language = "finnish-tdt")
    }
    model_tdt <- udpipe::udpipe_load_model(
      file = "finnish-tdt-ud-2.5-191206.udpipe"
    )
    annotated_data <- as.data.frame(
      udpipe::udpipe_annotate(model_tdt, x = data$new_col, doc_id = data$fsd_id)
    )
  }
  annotated_data <- annotated_data %>%
    dplyr::mutate(token = tolower(token)) %>%
    dplyr::mutate(lemma = tolower(lemma))
  if (!is.null(weights)) {
    weight_data <- subset(data, select= c(id, weights))
    weight_data[[weights]] <- as.numeric((gsub(",", ".", weight_data[[weights]])))
    annotated_data <- merge(x = annotated_data,
                            y = weight_data,
                            by.x = 'doc_id',
                            by.y = id
                            )
  }
  if (!is.null(add_cols)) {
    new_cols <- c(id, add_cols)
    add_data <- subset(data, select= new_cols)
    annotated_data <- merge(x = annotated_data,
                            y = add_data,
                            by.x = 'doc_id',
                            by.y = id
                            )
  }
  annotated_data
}

#' Get available Finnish stopwords lists
#'
#' Returns a tibble containing available Finnish stopword lists, their contents,
#' and the size of the lists.
#'
#' @return A tibble containing the stopwords lists.
#' @export
#'
#' @examples
#' fst_find_stopwords()
fst_find_stopwords <- function() {
  Name <- sort(stopwords::stopwords_getsources()[unlist(lapply(
    stopwords::stopwords_getsources(),
    function(x) {
      ifelse("fi" %in% stopwords::stopwords_getlanguages(x),
        TRUE, FALSE
      )
    }
  ))])
  Stopwords <- c(lapply(Name, function(y) stopwords::stopwords("fi", y)))
  Length <- lapply(Stopwords, function(z) ifelse(length(z) > 1, length(z), 0))
  dplyr::tibble(Name, Stopwords, Length)
}

#' Remove Finnish stopwords and punctuation from CoNLL-U dataframe
#'
#' Removes stopwords and punctuation from a dataframe containing Finnish survey
#' text data which is already in CoNLL-U format.
#'
#' @param data A dataframe of Finnish text in CoNLL-U format.
#' @param stopword_list A valid Finnish stopword list, default is `"nltk"`.
#'
#' @return A dataframe of Finnish text in CoNLL-U format without stopwords and
#'  punctuation.
#' @export
#'
#' @examples
#' fst_rm_stop_punct(conllu_dev_q11_3)
#' fst_rm_stop_punct(conllu_dev_q11_1, stopword_list <- "snowball")
#' fst_rm_stop_punct(conllu_cb_bullying, "stopwords-iso")
fst_rm_stop_punct <- function(data, stopword_list = "nltk") {
  swords <- stopwords::stopwords("fi", stopword_list)
  output <- data %>%
    dplyr::mutate(lemma = stringr::str_replace(.data$lemma, "#", "")) %>%
    dplyr::filter(!.data$lemma %in% swords) %>%
    dplyr::filter(.data$upos != "PUNCT")
  output
}


#' Read In and format Finnish survey text responses
#'
#' `fst_prepare_conllu()` produces a dataframe containing Finnish survey text
#'  responses in CoNLL-U format with stopwords optionally removed.
#' @param data A dataframe of survey responses which contains an open-ended
#'  question.
#' @param field The field in the dataframe which contains the open-ended
#'  question.
#' @param model A Finnish language model available for [udpipe], `"ftb"`
#'  (default) or `"tdt"`.
#' @param stopword_list A valid Finnish stopword list, default is `"nltk"`, or
#'  `"none"`.
#'
#' @return A dataframe of Finnish text in CoNLL-U format.
#' @export
#'
#' @examples
#' \donttest{
#' cb <- child
#' dev <- dev_coop
#' fst_prepare(data = cb, question = "q7", id = 'fsd_id')
#' fst_prepare(data = dev, question = "q11_2", id = 'fsd_id', weights = 'paino', add_cols = c('q1'))
#' unlink("finnish-ftb-ud-2.5-191206.udpipe")
#' unlink("finnish-tdt-ud-2.5-191206.udpipe")
#' }
fst_prepare <- function(data,
                        question,
                        id,
                        model = "ftb",
                        stopword_list = "nltk",
                        weights = NULL,
                        add_cols = NULL) {
  an_data <- fst_format(data = data,
                               question = question,
                               id = id,
                               model = model,
                               weights = weights,
                               add_cols = add_cols)
  if (stopword_list != "none") {
    an_data <- fst_rm_stop_punct(data = an_data, stopword_list = stopword_list)
  }
  an_data
}
