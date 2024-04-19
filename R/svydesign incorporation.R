child2 <- child
child2$paino <- as.numeric((gsub(",", ".", child2$paino)))
svy_child <- svydesign(id=~1, weights= ~paino, data = child2)

# We have a survey object in svy_child and a conllu table we can use in conllu_cv_bullying
# We have a conllu+weights in child_wghts
child_wghts <- fst_format(data = child, question = "q7", id = 'fsd_id', weights="paino")
child_con <- conllu_cb_bullying


fst_wordcloud_WEIGHTS_SVY <- function(data,
                                      pos_filter = NULL,
                                      max = 100,
                                      use_weights = FALSE
                                      svydesign = NULL,
                                      weight_col = NULL) {
  if (!is.null(pos_filter)) {
    data <- dplyr::filter(data, upos %in% pos_filter)
  }
  wordcloud_data <- data %>%
    dplyr::filter(.data$dep_rel != "punct") %>%
    dplyr::filter(!is.na(lemma)) %>%
    dplyr::filter(lemma != "na")
  if (!(use_weights)) {
    wordcloud_data <- dplyr::count(wordcloud_data, lemma, sort = TRUE)
  } else {
   ##### THIS IS WHERE I WAS
#     if (!is.null(svydesign()))
#     wordcloud_data <- dplyr::count(wordcloud_data, lemma, sort = TRUE, wt = !!as.name(weight_col))
#   }
#   wordcloud::wordcloud(
#     words = wordcloud_data$lemma,
#     freq = wordcloud_data$n,
#     max.words = max,
#     random.order = FALSE,
#     rot.per = 0.35,
#     colors = RColorBrewer::brewer.pal(8, "Dark2")
#   )
# }
