#' Concept Network - Search TextRank for concepts
#'
#' This function takes a string of terms (separated by commas) or a single term
#' and, using `textrank_keywords()` from `textrank` package, filters data based
#' on `pos_filter` and finds words connected to search terms.
#'
#' @param data A dataframe of text in CoNLL-U format, with optional additional
#'  columns.
#' @param concepts String of terms to search for, separated by commas.
#' @param pos_filter List of UPOS tags for inclusion, default is `NULL` to
#' include all UPOS tags.
#'
#' @return Dataframe of n-grams containing searched terms.
#' @export
#'
#' @examples
#' con <- "kiusata, lyöminen, lyödä, potkia"
#' pf <- c("NOUN", "VERB", "ADJ", "ADV")
#' pf2 <- "NOUN, VERB, ADJ, ADV"
#' fst_cn_search(fst_child, concepts = con, pos_filter = pf)
#' fst_cn_search(fst_child, concepts = con, pos_filter = pf2)
#' fst_cn_search(fst_child, concepts = con)
fst_cn_search <- function(data,
                          concepts,
                          pos_filter = NULL) {
  if (is.null(pos_filter)) {
    pos_filter <- c(
      "ADJ", "ADP", "ADV", "AUX", "CCONJ", "DET", "INTJ", "NOUN",
      "NUM", "PART", "PRON", "PROPN", "SCONJ", "SYM",
      "VERB", "X"
    )
  }
  if (length(pos_filter) == 1) {
    pos_filter <- pos_filter %>%
      stringr::str_extract_all(pattern = "\\w+") %>%
      unlist()
  }
  if (stringr::str_detect(concepts, ",")) {
    concepts <- stringr::str_extract_all(concepts, pattern = "\\w+") %>%
      unlist()
  }
  data <- dplyr::filter(data, token != "na")
  data$lemma <- stringr::str_replace_all(data$lemma, "-", "@")
  x <- textrank::textrank_keywords(data$lemma,
    relevant = data$upos %in% pos_filter
  )
  keyword_data <- x$keywords %>%
    dplyr::filter(ngram > 1 & freq > 1) %>%
    dplyr::mutate(word2 = strsplit(keyword, "-")) %>%
    tidyr::unnest(word2) %>%
    dplyr::group_by(keyword) %>%
    dplyr::mutate(word1 = dplyr::lag(word2)) %>%
    dplyr::relocate(word1, .before = word2) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(word1))
  keyword_data$word2 <- stringr::str_replace_all(keyword_data$word2, "@", "-")
  keyword_data$word1 <- stringr::str_replace_all(keyword_data$word1, "@", "-")
  concept_keywords <- keyword_data %>%
    dplyr::filter(word1 %in% concepts) %>%
    dplyr::pull(keyword)
  all_concepts <- keyword_data %>%
    dplyr::filter(keyword %in% concept_keywords)
  return(all_concepts)
}

#' Concept Network - Get TextRank edges
#'
#' This function takes a string of terms (separated by commas) or a single term
#' and, using `fst_cn_search()` find words connected to these searched terms.
#' Then, a dataframe is returned of 'edges' between two words which are
#' connected together in an frequently-occurring n-gram containing a concept
#' term.
#'
#' @param data A dataframe of text in CoNLL-U format, with optional additional
#'  columns.
#' @param concepts List of terms to search for, separated by commas.
#' @param threshold A minimum number of occurrences threshold for 'edge' between
#'  searched term and other word, default is `NULL`. Note, the threshold is
#'  applied before normalisation.
#' @param norm The method for normalising the data. Valid settings are
#'  `"number_words"` (the number of words in the responses), `"number_resp"`
#'  (the number of responses), or `NULL` (raw count returned, default, also used
#'  when weights are applied).
#' @param pos_filter List of UPOS tags for inclusion, default is `NULL` to
#' include all UPOS tags.
#'
#' @return Dataframe of co-occurrences between two connected words.
#' @export
#'
#' @examples
#' con <- "kiusata, lyöminen"
#' fst_cn_edges(fst_child, con, pos_filter = c("NOUN", "VERB", "ADJ", "ADV"))
#' fst_cn_edges(fst_child, con, pos_filter = 'VERB, NOUN')
#' fst_cn_edges(fst_child, "lyöminen", threshold = 2, norm = "number_resp")
fst_cn_edges <- function(data,
                         concepts,
                         threshold = NULL,
                         norm = "number_words",
                         pos_filter = NULL) {
  data <- dplyr::filter(data, token != "na")
  if (is.null(norm)) {
    denom <- 1
  } else if (norm == "number_words") {
    data %>%
      dplyr::filter(.data$dep_rel != "punct") %>%
      dplyr::filter(!is.na(lemma)) %>%
      dplyr::filter(lemma != "na")
    denom <- nrow(data)
  } else if (norm == "number_resp") {
    no_resp_count <- length(which(data$sentence %in% c("NA", "na")))
    count <- dplyr::n_distinct(data$doc_id) - no_resp_count
    denom <- count
  } else {
    message("NOTE: A recognised normalisation method has not been provided. \n
            Function has defaulted to has defaulted to provide raw counts")
    denom <- 1
  }
  df <- data %>%
    fst_cn_search(concepts = concepts, pos_filter = pos_filter) %>%
    dplyr::select(word1, word2, freq) %>%
    dplyr::group_by(word1, word2) %>%
    dplyr::summarize(n = sum(freq), .groups = "drop") %>%
    dplyr::rename(
      from = word1,
      to = word2
    )
  if (!is.null(threshold)) {
    df <- df %>% dplyr::filter(n >= threshold)
  }
  df <- df %>%
    dplyr::mutate(n = signif(n / denom, 3)) %>%
    dplyr::rename(co_occurrence = n)
  return(df)
}

#' Concept Network - Get TextRank nodes
#'
#' This function takes a string of terms (separated by commas) or a single term
#' and, using `textrank_keywords()` from `textrank` package, filters data based
#' on `pos_filter` ranks words which are the filtered for those connected to
#' search terms.
#'
#' @param data A dataframe of text in CoNLL-U format, with optional additional
#'  columns.
#' @param edges Output of `fst_cn_edges()`, dataframe of co-occurrences between
#'  two words.
#' @param pos_filter List of UPOS tags for inclusion, default is `NULL` to
#' include all UPOS tags.
#'
#' @return A dataframe containing relevant lemmas and their associated pagerank.
#' @export
#'
#' @examples
#' con <- "kiusata, lyöminen"
#' cb <- fst_child
#' edges <- fst_cn_edges(cb, con, pos_filter = c("NOUN", "VERB", "ADJ", "ADV"))
#' edges2 <- fst_cn_edges(cb, con, pos_filter = 'NOUN, VERB, ADJ, ADV')
#' fst_cn_nodes(cb, edges, c("NOUN", "VERB", "ADJ", "ADV"))
#' fst_cn_nodes(cb, edges, 'NOUN, VERB, ADJ, ADV')
fst_cn_nodes <- function(data,
                         edges,
                         pos_filter = NULL) {
  if (is.null(pos_filter)) {
    pos_filter <- c(
      "ADJ", "ADP", "ADV", "AUX", "CCONJ", "DET", "INTJ", "NOUN",
      "NUM", "PART", "PRON", "PROPN", "SCONJ", "SYM",
      "VERB", "X"
    )
  }
  if (length(pos_filter) == 1) {
    pos_filter <- pos_filter %>%
      stringr::str_extract_all(pattern = "\\w+") %>%
      unlist()
  }
  data <- dplyr::filter(data, token != "na")
  keyw <- textrank::textrank_keywords(data$lemma,
    relevant = data$upos %in% pos_filter
  )
  textrank_data <- data.frame(pagerank = keyw$pagerank$vector) %>%
    tibble::rownames_to_column("lemma")
  keyword_vocab <- unique(c(edges$from, edges$to))
  df <- textrank_data %>% dplyr::filter(lemma %in% keyword_vocab)
  return(df)
}

#' Plot Concept Network
#'
#' Creates a Concept Network plot from a list of edges and nodes (and their
#' respective weights).
#'
#' @param edges Output of `fst_cn_edges()`, dataframe of 'edges' connecting two
#'  words.
#' @param nodes Output of `fst_cn_nodes()`, dataframe of relevant lemmas and
#'  their associated pagerank.
#' @param concepts List of terms which have been searched for, separated by
#'  commas.
#' @param title Optional title for plot, default is `NULL` and a generic title
#'  ("TextRank extracted keyword occurrences") will be used.
#'
#' @return Plot of Concept Network.
#' @export
#'
#' @examples
#' con <- "kiusata, lyöminen"
#' cb <- fst_child
#' edges <- fst_cn_edges(cb, con, pos_filter = c("NOUN", "VERB", "ADJ", "ADV"))
#' nodes <- fst_cn_nodes(cb, edges, c("NOUN", "VERB", "ADJ", "ADV"))
#' fst_cn_plot(edges = edges, nodes = nodes, concepts = con)
fst_cn_plot <- function(edges, nodes, concepts, title = NULL) {
  if (stringr::str_detect(concepts, ",")) {
    concepts <- concepts %>%
      lapply(tolower) %>%
      stringr::str_extract_all(pattern = "\\w+") %>%
      unlist()
  }
  if (is.null(title)) {
    title <- "Concept Network of TextRank extracted keyword occurrences"
  }
  nodes <- nodes %>%
    dplyr::mutate(is_concept = factor(ifelse(lemma %in% concepts, 0, 1),
      levels = 0:1,
      labels = c(
        "Concept word",
        "Regular word"
      )
    ))
  p <- igraph::graph_from_data_frame(edges,
    directed = FALSE,
    vertices = nodes
  ) %>%
    ggraph::ggraph(layout = "kk") +
    ggraph::geom_edge_link(
      ggplot2::aes(
        width = co_occurrence,
        alpha = co_occurrence
      ),
      colour = "#6da5d3"
    ) +
    ggraph::scale_edge_width(range = c(1, 5), limits = c(min(edges$co_occurrence), max(edges$co_occurrence))) +
    ggraph::scale_edge_alpha(range = c(0.2, 1), limits = c(min(edges$co_occurrence), max(edges$co_occurrence))) +
    ggraph::geom_node_point(ggplot2::aes(size = pagerank)) +
    ggplot2::scale_size(limit = c(min(nodes$pagerank), max(nodes$pagerank))) +
    ggraph::geom_node_text(ggplot2::aes(label = name, col = is_concept),
      check_overlap = TRUE, repel = TRUE
    ) +
    ggplot2::scale_color_manual("Word Type",
      values = c(
        "Concept word" = "#cd1719",
        "Regular word" = "black"
      )
    ) +
    ggraph::theme_graph(base_family = "sans") +
    ggplot2::labs(
      title = title
    ) +
    ggplot2::theme(legend.position = "right")

  return(p)
}

#' Concept Network - Make Concept Network plot
#'
#' This function takes a string of terms (separated by commas) or a single term
#' and, using `textrank_keywords()` from `textrank` package, filters data based
#' on `pos_filter` and finds words connected to search terms. Then it plots a
#' Concept Network based on the calculated weights of these terms and the
#' frequency of co-occurrences.
#'
#' @param data A dataframe of text in CoNLL-U format, with optional additional
#'  columns.
#' @param concepts List of terms to search for, separated by commas.
#' @param threshold A minimum number of occurrences threshold for 'edge' between
#'  searched term and other word, default is `NULL`. Note, the threshold is
#'  applied before normalisation.
#' @param norm The method for normalising the data. Valid settings are
#'  `"number_words"` (the number of words in the responses), `"number_resp"`
#'  (the number of responses), or `NULL` (raw count returned, default, also used
#'  when weights are applied).
#' @param pos_filter List of UPOS tags for inclusion, default is `NULL` to
#'  include all UPOS tags.
#' @param title Optional title for plot, default is `NULL` and a generic title
#'  ("TextRank extracted keyword occurrences") will be used.
#'
#' @return Plot of Concept Network.
#' @export
#'
#' @examples
#' data <- fst_child
#' con <- "kiusata, lyöminen"
#' pf <- c("NOUN", "VERB", "ADJ", "ADV")
#' title <- "Bullying Concept Network"
#' fst_concept_network(data, concepts = con, pos_filter = pf, title = title)
fst_concept_network <- function(data,
                                concepts,
                                threshold = NULL,
                                norm = "number_words",
                                pos_filter = NULL,
                                title = NULL) {
  edges <- fst_cn_edges(
    data = data,
    concepts = concepts,
    threshold = threshold,
    norm = norm,
    pos_filter = pos_filter
  )
  nodes <- fst_cn_nodes(data = data, edges, pos_filter = pos_filter)
  fst_cn_plot(edges, nodes, concepts = concepts, title = title)
}
