#PAINO2, incorporating weights from the start.
# Also incorporating dimensions from the start.

weightingsdf <- read.csv('/Users/adelineclarke/Documents/Uni Helsinki Work/2024/data/daF2821_fin.csv', sep = ';')

# new vars are id_field, weight_field, dimension_field
# id_field used for join,

a <- fst_format_conllu(data = child_barometer_data, field = "q7")
b <- fst_format_conllu_WEIGHTS(data = child, question = "q7", id = "fsd_id")
c <- fst_format_conllu_WEIGHTS(data = child, question = "q7", id = 'fsd_id', weight= 'paino')
d <- fst_format_conllu_WEIGHTS(data = child, question = "q7", id = 'fsd_id', weight= 'paino', dim = 'bv1')
e <- fst_format_conllu_WEIGHTS(data = child, question = "q7", id = 'fsd_id', dim = c('bv1', 'bv9', 'bv3'))

df1 <- fst_format_conllu(data = dev_coop, field = "q11_3")
df1b <- fst_format_conllu_WEIGHTS(data = dev_data, question = 'q11_3', id = 'fsd_id')
df2 <- fst_format_conllu_WEIGHTS(data = dev_coop, question = "q11_3", id = 'fsd_id', dim = c('q1', 'q2'))
df3 <- fst_format_conllu_WEIGHTS(data = dev_coop, question = "q11_3", id_field = 'fsd_id')
df4 <- fst_format_conllu_WEIGHTS(data = dev_coop, model = "tdt", question = "q11_3", id_field = 'fsd_id')
df5 <- fst_format_conllu_WEIGHTS(data = dev_coop, question = "q11_3", id_field = 'fsd_id', weight_field = 'paino')
df6 <- fst_format_conllu_WEIGHTS(data = dev_coop, question = "q11_3", id_field = 'fsd_id', weight_field = 'paino', dimension_field = 'q1')
df7 <- fst_format_conllu_WEIGHTS(data = dev_coop, question = "q11_3", id_field = 'fsd_id', weight_field = 'paino', dimension_field = c('q1', 'q2', 'q3'))


fst_format_conllu_WEIGHTS <- function(data, question, id, model = "ftb", weight = NULL, dim = NULL) {
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
  if (!is.null(weight)) {
    weight_data <- subset(data, select= c(id, weight))
    weight_data[[weight]] <- as.numeric((gsub(",", ".", weight_data[[weight]])))
    annotated_data <- merge(x = annotated_data, y = weight_data, by.x = 'doc_id', by.y = id)
  }
  if (!is.null(dim)) {
    dim_cols <- c(id, dim)
    dim_data <- subset(data, select= dim_cols)
    annotated_data <- merge(x = annotated_data, y = dim_data, by.x = 'doc_id', by.y = id)
  }
  annotated_data
}
