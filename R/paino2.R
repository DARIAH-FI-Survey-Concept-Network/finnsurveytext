#PAINO2, incorporating weights from the start.
# Also incorporating dimensions from the start.

weightingsdf <- read.csv('/Users/adelineclarke/Documents/Uni Helsinki Work/2024/data/daF2821_fin.csv', sep = ';')

# new vars are id_field, weight_field, dimension_field
# id_field used for join,

df1 <- fst_format_conllu(data = weightingsdf, field = "q11_3")
df2 <- fst_format_conllu_WEIGHTS(data = weightingsdf, field = "q11_3")
df3 <- fst_format_conllu_WEIGHTS(data = weightingsdf, field = "q11_3", id_field = 'fsd_id')
df4 <- fst_format_conllu_WEIGHTS(data = weightingsdf, model = "tdt", field = "q11_3", id_field = 'fsd_id')
df5 <- fst_format_conllu_WEIGHTS(data = weightingsdf, field = "q11_3", id_field = 'fsd_id', weight_field = 'paino')
df6 <- fst_format_conllu_WEIGHTS(data = weightingsdf, field = "q11_3", id_field = 'fsd_id', weight_field = 'paino', dimension_field = 'q1')
df7 <- fst_format_conllu_WEIGHTS(data = weightingsdf, field = "q11_3", id_field = 'fsd_id', weight_field = 'paino', dimension_field = c('q1', 'q2', 'q3'))


fst_format_conllu_WEIGHTS <- function(data, field, id_field = NULL, weight_field = NULL, dimension_field = NULL, model = "ftb") {
  data <- data %>%
    dplyr::mutate(new_col = trimws(.data[[field]])) %>%
    dplyr::mutate_if(is.character, dplyr::na_if, "")
  #data <- data$new_col
  if (model == "ftb") {
    if (!file.exists("finnish-ftb-ud-2.5-191206.udpipe")) {
      udpipe::udpipe_download_model(language = "finnish-ftb")
    }
    model_ftb <- udpipe::udpipe_load_model(
      file = "finnish-ftb-ud-2.5-191206.udpipe"
    )
    if (is.null(id_field)) {
      annotated_data <- as.data.frame(
        udpipe::udpipe_annotate(model_ftb, x = data$new_col)
      )
    } else {
      annotated_data <- as.data.frame(
        udpipe::udpipe_annotate(model_ftb, x = data$new_col, doc_id = data$fsd_id)
      )
    }
  } else if (model == "tdt") {
    if (!file.exists("finnish-tdt-ud-2.5-191206.udpipe")) {
      udpipe::udpipe_download_model(language = "finnish-tdt")
    }
    model_tdt <- udpipe::udpipe_load_model(
      file = "finnish-tdt-ud-2.5-191206.udpipe"
    )
    if (is.null(id_field)) {
      annotated_data <- as.data.frame(
        udpipe::udpipe_annotate(model_tdt, x = data$new_col)
      )
    } else {
      annotated_data <- as.data.frame(
        udpipe::udpipe_annotate(model_tdt, x = data$new_col, doc_id = data$fsd_id)
      )
    }
  }
  annotated_data %>%
    dplyr::mutate(token = tolower(token)) %>%
    dplyr::mutate(lemma = tolower(lemma))
  if (!is.null(weight_field)) {
    weight_data <- subset(data, select= c(id_field, weight_field))
    weight_data[[weight_field]] <- as.numeric((gsub(",", ".", weight_data[[weight_field]])))
    annotated_data <- merge(x = annotated_data, y = weight_data, by.x = 'doc_id', by.y = id_field)
  }
  if (!is.null(dimension_field)) {
    dim_data <- c(id_field, dimension_field)
    dim_data <- subset(data, select= dim_data)
    annotated_data <- merge(x = annotated_data, y = dim_data, by.x = 'doc_id', by.y = id_field)
  }
  annotated_data
}
