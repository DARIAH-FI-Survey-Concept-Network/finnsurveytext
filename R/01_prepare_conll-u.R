#' Annotate open-ended survey responses in Finnish into CoNLL-U format
#'
#' Creates a dataframe in CoNLL-U format from a list of strings of Finnish text
#' using the [udpipe] package and a Finnish language model
#'
#' @param data A dataframe of survey responses which contains an open-ended question.
#' @param field The field in the dataframe which contains the open-ended question.
#' @param model A Finnish language model available for [udpipe], `"ftb"` (default)
#'  or `"tdt"`.
#'
#' @return Dataframe of annotated text in CoNLL-U format.
#' @export
#'
#' @examples
#' conllu_dev_q11_1 <- fst_format_conllu(data = dev_data, field = "q11_1")
#' conllu_dev_q11_2 <- fst_format_conllu(data = dev_data, field = "q11_2")
#' conllu_dev_q11_3 <- fst_format_conllu(data = dev_data, field = "q11_3")
#' conllu_cb_bullying <- fst_format_conllu(data = child_barometer_data, field = "q7", model = "tdt")
fst_format_conllu <- function(data, field, model = "ftb") {
  data <- data %>%
    dplyr::mutate(new_col = trimws(.data[[field]])) %>%
    dplyr::mutate_if(is.character, dplyr::na_if, "")
  data <- data$new_col
  if (model == "ftb") {
    if (!file.exists("finnish-ftb-ud-2.5-191206.udpipe")) {
      udpipe::udpipe_download_model(language = "finnish-ftb")
    }
    model_ftb <- udpipe::udpipe_load_model(file = "finnish-ftb-ud-2.5-191206.udpipe")
    annotated_data <- as.data.frame(udpipe::udpipe_annotate(model_ftb, x = data))
  } else if (model == "tdt") {
    if (!file.exists("finnish-tdt-ud-2.5-191206.udpipe")) {
      udpipe::udpipe_download_model(language = "finnish-tdt")
    }
    model_tdt <- udpipe::udpipe_load_model(file = "finnish-tdt-ud-2.5-191206.udpipe")
    annotated_data <- as.data.frame(udpipe::udpipe_annotate(model_tdt, x = data))
  }
  annotated_data %>%
    dplyr::mutate(token = tolower(token)) %>%
    dplyr::mutate(lemma = tolower(lemma))
}

#' Get available Finnish stopwords lists
#'
#' Returns a tibble containing available Finnish stopword lists, their contents,
#' and the size of the lists.
#'
#' @return A tibble containing the stopwords lists
#' @export
#'
#' @examples
#' stopwords <- fst_find_stopwords()
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
#' @return A dataframe of Finnish text in CoNLL-U format.
#' @export
#'
#' @examples
#' conllu_dev_q11_1_nltk <- fst_rm_stop_punct(conllu_dev_q11_1)
#' conllu_dev_q11_2_nltk <- fst_rm_stop_punct(conllu_dev_q11_2)
#' conllu_dev_q11_3_nltk <- fst_rm_stop_punct(conllu_dev_q11_3)
#' conllu_dev_q11_1_snow <- fst_rm_stop_punct(conllu_dev_q11_1, stopword_list = 'snowball')
#' conllu_cb_bullying_iso <- fst_rm_stop_punct(conllu_cb_bullying, 'stopwords-iso')
fst_rm_stop_punct <- function(data, stopword_list = "nltk") {
  swords <- stopwords::stopwords("fi", stopword_list)
  output <- data %>%
    dplyr::mutate(lemma = stringr::str_replace(.data$lemma, "#", "")) %>%
    dplyr::filter(!.data$lemma %in% swords) %>%
    dplyr::filter(.data$upos != 'PUNCT')
  output
}


#' Read In and format Finnish survey text responses
#'
#' `fst_prepare_conllu` produces a dataframe (and saves as csv) containing
#' Finnish survey text reponses in CoNLL-U format with stopwords removed.
#'
#' @param data A dataframe of survey responses which contains an open-ended question.
#' @param field The field in the dataframe which contains the open-ended question.
#' @param model A Finnish language model available for [udpipe], `ftb` (default)
#'  or `tdt`.
#' @param stopword_list A valid Finnish stopword list, default is `nltk`, or
#'  "none".
#'
#' @return CoNLL-U dataframe
#' @export
#'
#' @examples
#' prepd_bullying_iso <- fst_prepare_conllu(data = child_barometer_data, field = "q7", stopword_list = "stopwords-iso")
#' prepd_dev_q11_2_snow <- fst_prepare_conllu(data = dev_data, field = "q11_2", stopword_list = "snowball")
#' conllu_dev_q11_1_f_nltk <- fst_prepare_conllu(data = dev_data_f, field = "q11_1", stopword_list = "nltk")
fst_prepare_conllu <- function(data,
                               field,
                               model = "ftb",
                               stopword_list = "nltk") {
  an_data <- fst_format_conllu(model = model, data = data, field = field)
  if (stopword_list != "none") {
    an_data <- fst_rm_stop_punct(data = an_data, stopword_list = stopword_list)
  }
  an_data
}
