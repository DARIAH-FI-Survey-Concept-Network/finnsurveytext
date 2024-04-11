weightingsdf <- read.csv('/Users/adelineclarke/Documents/Uni Helsinki Work/2024/data/daF2821_fin.csv', sep = ';')

weightingsdf <- subset(weightingsdf, select= c(fsd_id, paino))

# We're going to do a potentially problematic join here as a POC. We will need to get the CoNLL-U dataframe to include a weight or link better
df <- conllu_dev_q11_3

# get the new id column
df$new_id <- substring(df$doc_id, 4)

df2 = merge(x = df, y = weightingsdf, by.x = "new_id", by.y = "fsd_id")

df2$paino <- as.numeric((gsub(",", ".", df2$paino)))

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
