child2 <- child
child2$paino <- as.numeric((gsub(",", ".", child2$paino)))
svy_child <- svydesign(id=~1, weights= ~paino, data = child2)
svy_child <- svydesign(id=~1, weights= ~paino, data = child)
dev_coop2 <- dev_coop
dev_coop2$paino <- as.numeric((gsub(",", ".", dev_coop2$paino)))
svy_dev2 <- svydesign(id = ~1, weights = ~'paino', data =dev_coop2)

formatted_svy_child <- fst_format_svydesign(svy_child, question = 'q7', id = 'fsd_id')
formatted_svy_child2 <- fst_format_svydesign(svy_child, question = 'q7', id = 'fsd_id', use_weights = FALSE)
formatted_svy_child3 <- fst_format_svydesign(svy_child, 'q7', 'fsd_id', add_cols = c('bv3', 'bv9'))
formatted_svy_dev <- fst_format_svydesign(svy_dev2, 'q11_1', 'fsd_id', add_cols = 'q1')

fst_format_svydesign <- function(svydesign,
                                 question,
                                 id,
                                 model = "ftb",
                                 use_weights = TRUE,
                                 add_cols = NULL
                                 ) {
  data <- svydesign$variables %>%
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
  }
  annotated_data <- annotated_data %>%
    dplyr::mutate(token = tolower(token)) %>%
    dplyr::mutate(lemma = tolower(lemma))
  if (use_weights) {
    weight_data <- svydesign$allprob
    colnames(weight_data) <- c("weight")
    weight_data['weight'] = 1/weight_data['weight']
    data2 <- data %>%
      dplyr::select(all_of(id))
    weight_data2 <- dplyr::bind_cols(data2, weight_data)
    annotated_data <- merge(x = annotated_data,
                            y = weight_data2,
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


# fst_prepare_svydesign(svydesign = svy_child, question = "q7", id = 'fsd_id')
# fst_prepare_svydesign(svy_dev2, question = "q11_2", id = 'fsd_id', add_cols = c('q1'))
fst_prepare_svydesign <- function(svydesign,
                                  question,
                                  id,
                                  model = "ftb",
                                  stopword_list = "nltk",
                                  use_weights = TRUE,
                                  add_cols = NULL) {
  an_data <- fst_format_svydesign(svydesign = svydesign,
                                  question = question,
                                  id = id,
                                  model = model,
                                  use_weights = use_weights,
                                  add_cols = add_cols)
  if (stopword_list != "none") {
    an_data <- fst_rm_stop_punct(data = an_data, stopword_list = stopword_list)
  }
  an_data
}


