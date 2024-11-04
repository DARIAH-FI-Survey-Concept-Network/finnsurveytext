#' Annotate open-ended survey responses in Finnish into CoNLL-U format
#'
#' Creates a dataframe in CoNLL-U format from a dataframe containing Finnish
#' text from using the [udpipe] package and a Finnish language model plus any
#' additional columns that are included such as `weights` or columns added
#' through `add_cols`.
#'
#' @param data A dataframe of survey responses which contains an open-ended
#'  question.
#' @param question The column in the dataframe which contains the open-ended
#'  question.
#' @param id The column in the dataframe which contains the ids for the
#'  responses.
#' @param model A language model available for [udpipe]. `"ftb"`
#'  (default) or `"tdt"` are recognised as shorthand for "finnish-ftb" and
#'  "finnish-tdt". The full list is available in the [udpipe] documentation.
#' @param weights Optional, the column of the dataframe which contains the
#'  respective weights for each response.
#' @param add_cols Optional, a column (or columns) from the dataframe which
#'  contain other information you'd like to retain (for instance, covariate
#'  columnns for splitting the data for comparison plots).
#'
#' @return Dataframe of annotated text in CoNLL-U format plus any additional
#'  columns.
#' @export
#'
#' @examples
#' \donttest{
#' i <- "fsd_id"
#' fst_format(data = child, question = "q7", id = i)
#' fst_format(data = child, question = "q7", id = i, model = "tdt")
#' fst_format(data = child, question = "q7", id = i, weights="paino")
#' cols <- c("gender", "major_region", "daycare_before_school")
#' fst_format(child, question = "q7", id = i, add_cols = cols)
#' fst_format(child, question = "q7", id = i, add_cols = "gender, major_region")
#' fst_format(child, question = 'q7', id = i, model = 'swedish-talbanken')
#' unlink("finnish-ftb-ud-2.5-191206.udpipe")
#' unlink("finnish-tdt-ud-2.5-191206.udpipe")
#' unlink("swedish-talkbanken-ud-2.5-191206.udpipe")
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
      udpipe::udpipe_annotate(model_tdt, x = data$new_col, doc_id = data[[id]])
    )
  } else {
    name2 <- paste0(model, '-ud-2.5-191206.udpipe')
    if (!file.exists(name2)) {
      udpipe::udpipe_download_model(language = model)
    }
    model_2 <- udpipe::udpipe_load_model(
      file = name2
    )
    annotated_data <- as.data.frame(
      udpipe::udpipe_annotate(model_2, x = data$new_col, doc_id = data[[id]])
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
    annotated_data <- dplyr::rename(annotated_data, weight = !!as.name(weights))
  }
  if (!is.null(add_cols)) {
    if (length(add_cols) == 1) {
      add_cols <- add_cols %>%
        stringr::str_extract_all(pattern = "\\w+") %>%
        unlist()
    }
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

#' Find treebanks available for use
#'
#' @param search An optional string for filtering the list, name of language in
#'  English, eg. 'estonian'
#'
#' @return List of available treebanks, filtered
#' @export
#'
#' @examples
#' fst_print_available_models()
#' fst_print_available_models(search = "swedish")
fst_print_available_models <- function(search = NULL) {
  models <- models <- c("afrikaans-afribooms", "ancient_greek-perseus", "ancient_greek-proiel",
                        "arabic-padt", "armenian-armtdp", "basque-bdt", "belarusian-hse", "bulgarian-btb",
                        "buryat-bdt", "catalan-ancora", "chinese-gsd", "chinese-gsdsimp",
                        "classical_chinese-kyoto", "coptic-scriptorium", "croatian-set", "czech-cac",
                        "czech-cltt", "czech-fictree", "czech-pdt", "danish-ddt", "dutch-alpino",
                        "dutch-lassysmall", "english-ewt", "english-gum", "english-lines", "english-partut",
                        "estonian-edt", "estonian-ewt", "finnish-ftb", "finnish-tdt", "french-gsd",
                        "french-partut", "french-sequoia", "french-spoken", "galician-ctg",
                        "galician-treegal", "german-gsd", "german-hdt", "gothic-proiel", "greek-gdt",
                        "hebrew-htb", "hindi-hdtb", "hungarian-szeged", "indonesian-gsd", "irish-idt",
                        "italian-isdt", "italian-partut", "italian-postwita", "italian-twittiro",
                        "italian-vit", "japanese-gsd", "kazakh-ktb", "korean-gsd", "korean-kaist",
                        "kurmanji-mg", "latin-ittb", "latin-perseus", "latin-proiel", "latvian-lvtb",
                        "lithuanian-alksnis", "lithuanian-hse", "maltese-mudt", "marathi-ufal",
                        "north_sami-giella", "norwegian-bokmaal", "norwegian-nynorsk",
                        "norwegian-nynorsklia", "old_church_slavonic-proiel", "old_french-srcmf",
                        "old_russian-torot", "persian-seraji", "polish-lfg", "polish-pdb", "polish-sz",
                        "portuguese-bosque", "portuguese-br", "portuguese-gsd", "romanian-nonstandard",
                        "romanian-rrt", "russian-gsd", "russian-syntagrus", "russian-taiga", "sanskrit-ufal",
                        "scottish_gaelic-arcosg", "serbian-set", "slovak-snk", "slovenian-ssj",
                        "slovenian-sst", "spanish-ancora", "spanish-gsd", "swedish-lines",
                        "swedish-talbanken", "tamil-ttb", "telugu-mtg", "turkish-imst", "ukrainian-iu",
                        "upper_sorbian-ufal", "urdu-udtb", "uyghur-udt", "vietnamese-vtb", "wolof-wtb")
  if (is.null(search)) {
    models
  } else {
    grep(search, models, value = TRUE)
  }
}

#' Get available Finnish stopwords lists
#'
#' Returns a tibble containing all available stopword lists for the language,
#' their contents, and the size of the lists.
#'
#' @param language two-letter ISO code of the language for the stopword list
#'
#' @return A tibble containing the stopwords lists.
#' @export
#'
#' @examples
#' fst_find_stopwords()
#' fst_find_stopwords(language = 'et')
fst_find_stopwords <- function(language = 'fi') {
  Name <- sort(stopwords::stopwords_getsources()[unlist(lapply(
    stopwords::stopwords_getsources(),
    function(x) {
      ifelse(language %in% stopwords::stopwords_getlanguages(x),
        TRUE, FALSE
      )
    }
  ))])
  Stopwords <- c(lapply(Name, function(y) stopwords::stopwords(language, y)))
  Length <- lapply(Stopwords, function(z) ifelse(length(z) > 1, length(z), 0))
  dplyr::tibble(Name, Stopwords, Length)
}

#' Remove Finnish stopwords and punctuation from CoNLL-U dataframe
#'
#' Removes stopwords and punctuation from a dataframe containing Finnish survey
#' text data which is already in CoNLL-U format.
#'
#' @param data A dataframe of Finnish text in CoNLL-U format.
#' @param stopword_list A valid stopword list, default is `"nltk"`,
#'  `"manual"` can be used to indicate that a manual list will be provided, or
#'  `"none"` if you don't want to remove stopwords, known as 'source' in
#'  `stopwords::stopwords`
#' @param language two-letter ISO code of the language for the stopword list
#' @param manual An optional boolean to indicate that a manual list will be
#'  provided, `stopword_list = "manual"` can also or instead be used.
#' @param manual_list A manual list of stopwords.
#'
#' @return A dataframe of text in CoNLL-U format without stopwords and
#'  punctuation.
#' @export
#'
#' @examples
#' \donttest{
#' c <- fst_format(child, question = 'q7', id = 'fsd_id')
#' fst_rm_stop_punct(c)
#' fst_rm_stop_punct(c, stopword_list = "snowball")
#' fst_rm_stop_punct(c, "stopwords-iso")
#'
#' mlist <- c('en', 'et', 'ei', 'emme', 'ette', 'eivät', 'minä', 'minum')
#' mlist2 <- "en, et, ei, emme, ette, eivät, minä, minum"
#' fst_rm_stop_punct(c, manual = TRUE, manual_list = mlist)
#' fst_rm_stop_punct(c, stopword_list = "manual", manual_list = mlist)
#' unlink("finnish-ftb-ud-2.5-191206.udpipe")
#' }
fst_rm_stop_punct <- function(data,
                              stopword_list = "nltk",
                              language = 'fi',
                              manual = FALSE,
                              manual_list = "") {
  if (stopword_list == 'none') {
    swords <- ""
  } else if (!(manual == TRUE || stopword_list == 'manual')) {
    swords <- stopwords::stopwords(language, stopword_list)
  } else {
    if (length(manual_list) == 1) {
      manual_list <- manual_list %>%
        lapply(tolower) %>%
        stringr::str_extract_all(pattern = "\\w+") %>%
        unlist()
    } else {
      manual_list <- manual_list %>%
        lapply(tolower)
    }
    swords <- manual_list
  }
  output <- data %>%
    dplyr::mutate(lemma = stringr::str_replace(.data$lemma, "#", "")) %>%
    dplyr::filter(!.data$lemma %in% swords) %>%
    dplyr::filter(.data$upos != "PUNCT")
  output
}


#' Read In and format Finnish survey text responses
#'
#' Creates a dataframe in CoNLL-U format from a dataframe containing Finnish
#' text from using the [udpipe] package and a Finnish language model plus any
#' additional columns that are included such as `weights` or columns added
#' through `add_cols`. Stopwords and punctuation are optionally removed if the
#' the `stopword_list` argument is not "none".
#'
#' `fst_prepare_conllu()` produces a dataframe containing Finnish survey text
#'  responses in CoNLL-U format with stopwords optionally removed.
#' @param data A dataframe of survey responses which contains an open-ended
#'  question.
#' @param question The column in the dataframe which contains the open-ended
#'  question.
#' @param id The column in the dataframe which contains the ids for the
#'  responses.
#' @param model A language model available for [udpipe]. `"ftb"`
#'  (default) or `"tdt"` are recognised as shorthand for "finnish-ftb" and
#'  "finnish-tdt". The full list is available in the [udpipe] documentation.
#' @param stopword_list A valid stopword list, default is `"nltk"`,
#'  `"manual"` can be used to indicate that a manual list will be provided, or
#'  `"none"` if you don't want to remove stopwords known as 'source' in
#'  `stopwords::stopwords`
#' @param language two-letter ISO code for the language for the stopword list
#' @param weights Optional, the column of the dataframe which contains the
#'  respective weights for each response.
#' @param add_cols Optional, a column (or columns) from the dataframe which
#'  contain other information you'd like to retain (for instance, dimension
#'  columnns for splitting the data for comparison plots).
#' @param manual An optional boolean to indicate that a manual list will be
#'  provided, `stopword_list = "manual"` can also or instead be used.
#' @param manual_list A manual list of stopwords.
#'
#' @return A dataframe of Finnish text in CoNLL-U format.
#' @export
#'
#' @examples
#' \donttest{
#' i <- "fsd_id"
#' cb <- child
#' dev <- dev_coop
#' fst_prepare(data = cb, question = "q7", id = 'fsd_id', weights = 'paino')
#' fst_prepare(data = dev, question = "q11_2", id = i, add_cols = c('gender'))
#' fst_prepare(data = dev, question = "q11_3", id = i, add_cols = 'gender')
#' fst_prepare(data = child, question = "q7", id = i, model = 'swedish-lines')
#' unlink("finnish-ftb-ud-2.5-191206.udpipe")
#' unlink("finnish-tdt-ud-2.5-191206.udpipe")
#' unlink("swedish-lines-ud-2.5-191206.udpipe")
#' }
fst_prepare <- function(data,
                        question,
                        id,
                        model = "ftb",
                        stopword_list = "nltk",
                        language = 'fi',
                        weights = NULL,
                        add_cols = NULL,
                        manual = FALSE,
                        manual_list = "") {
  an_data <- fst_format(data = data,
                               question = question,
                               id = id,
                               model = model,
                               weights = weights,
                               add_cols = add_cols)
  an_data <- fst_rm_stop_punct(data = an_data,
                               stopword_list = stopword_list,
                               language = language,
                               manual = manual,
                               manual_list = manual_list)
  an_data
}
