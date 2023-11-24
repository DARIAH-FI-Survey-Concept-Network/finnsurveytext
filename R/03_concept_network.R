#' Concept Network - Search textrank for concepts
#'
#' This function takes a string of terms (separated by commas) or a single term
#' and, using `textrank_keywords` from `textrank` package, filters data based on
#' `pos_filter` and finds words connected to search terms.
#'
#' @param data A dataframe of text in CoNLL-U format.
#' @param concepts String of terms to search for, separated by commas.
#' @param pos_filter List of UPOS tags for inclusion, default is `NULL` to
#' include all UPOS tags.
#'
#' @return Dataframe of n-grams containing searched terms
#' @export
#'
#' @examples
#' con1 <- "elintaso, köyhä, ihminen"
#' fst_cn_search(conllu_dev_q11_1_nltk, con1)
#'
#' con2 <- "kehitysmaa, auttaa, pyrkiä, maa, ihminen"
#' pf1 <- c("NOUN", "ADV", "ADJ")
#' fst_cn_search(conllu_dev_q11_2_nltk, con2, pos_filter = pf1)
#'
#' con3 <- "köyhyys, nälänhätä, sota, ilmastonmuutos, puute"
#' fst_cn_search(conllu_dev_q11_3_nltk, con3, pos_filter = "NOUN")
#'
#' con4 <- "kiusata, lyöminen, lyödä, potkia"
#' pf2 <- c("NOUN", "VERB", "ADJ", "ADV")
#' fst_cn_search(conllu_cb_bullying_iso, con4, pos_filter = pf2)
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

#' Concept Network - Get textrank edges
#'
#' This function takes a string of terms (separated by commas) or a single term
#' and, using `fst_cn_search` find words conected to these searched terms. Then,
#' a dataframe is returned of 'edges' between two words which are connected
#' together in an frequently-occurring n-gram containing a concept term
#'
#' @param data A dataframe of text in CoNLL-U format.
#' @param concepts List of terms to search for, separated by commas.
#' @param threshold A minimum number of occurrences threshold for 'edge' between
#'  searched term and other word, default is `NULL`. Note, the threshold is
#'  applied before normalisation.
#' @param norm The method for normalising the data. Valid settings are
#'  `'number_words'` (the number of words in the responses, default),
#'  `'number_resp'` (the number of responses), or `NULL` (raw count returned).
#' @param pos_filter List of UPOS tags for inclusion, default is `NULL` to
#' include all UPOS tags.
#'
#' @return Dataframe of 'edges' between two connected words.
#' @export
#'
#' @examples
#' con1 <- "elintaso, köyhä, ihminen"
#' fst_cn_edges(data = conllu_dev_q11_1_nltk, concepts = con1, threshold = 3)
#'
#' con2 <- "kehitysmaa, auttaa, pyrkiä, maa, ihminen"
#' fst_cn_edges(conllu_dev_q11_2_nltk, con2, threshold = 5)
#'
#' con3 <- "köyhyys, nälänhätä, sota, ilmastonmuutos, puute"
#' fst_cn_edges(conllu_dev_q11_3_nltk, con3, threshold = 2)
#'
#' con4 <- "kiusata, lyöminen"
#' cb <- conllu_cb_bullying_iso
#' fst_cn_edges(cb, con4, pos_filter = c("NOUN", "VERB", "ADJ", "ADV"))
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
    denom <- dplyr::n_distinct(data$doc_id)
  } else {
    message("NOTE: A recognised normalisation method has not been provided. \n
            Function has defaulted to normalisation method 'number_of_words'")
    data %>%
      dplyr::filter(.data$dep_rel != "punct") %>%
      dplyr::filter(!is.na(lemma)) %>%
      dplyr::filter(lemma != "na")
    denom <- nrow(data)
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

#' Concept Network - Get textrank nodes
#'
#' @param data A dataframe of text in CoNLL-U format.
#' @param edges Output of fst_cn_edges, dataframe of 'edges' connecting two words
#' @param pos_filter List of UPOS tags for inclusion, default is `NULL` to
#' include all UPOS tags.
#'
#' @return A dataframe containing relevant lemmas and their associated pagerank
#' @export
#'
#' @examples
#' con1 <- "elintaso, köyhä, ihminen"
#' data1 <- conllu_dev_q11_1_nltk
#' e1 <- fst_cn_edges(data = data1, concepts = con1, threshold = 3)
#' fst_cn_nodes(conllu_dev_q11_1_nltk, e1)
#'
#' con2 <- "kehitysmaa, auttaa, pyrkiä, maa, ihminen"
#' e2 <- fst_cn_edges(conllu_dev_q11_2_nltk, con2, threshold = 5)
#' fst_cn_nodes(conllu_dev_q11_2_nltk, e2)
#'
#' con3 <- "köyhyys, nälänhätä, sota, ilmastonmuutos, puute"
#' e3 <- fst_cn_edges(conllu_dev_q11_3_nltk, con3, threshold = 2)
#' fst_cn_nodes(conllu_dev_q11_3_nltk, e3)
#'
#' con4 <- "kiusata, lyöminen"
#' cb <- conllu_cb_bullying_iso
#' e4 <- fst_cn_edges(cb, con4, pos_filter = c("NOUN", "VERB", "ADJ", "ADV"))
#' fst_cn_nodes(conllu_cb_bullying_iso, e4, c("NOUN", "VERB", "ADJ", "ADV"))
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
#' @param edges Output of `fst_cn_edges`, dataframe of 'edges' connecting two
#'  words
#' @param nodes Output of `fst_cn_nodes`, dataframe of relevant lemmas and their
#'  associated pagerank
#' @param concepts List of terms which have been searched for, separated by
#'  commas.
#' @param title Optional title for plot, default is `NULL` and a generic title
#'  ('Textrank extracted keyword occurrences) will be used.
#'
#' @return Plot of Concept Network
#' @export
#'
#' @examples
#' con1 <- "elintaso, köyhä, ihminen"
#' data1 <- conllu_dev_q11_1_nltk
#' e1 <- fst_cn_edges(data = data1, concepts = con1, threshold = 3)
#' n1 <- fst_cn_nodes(data1, e1)
#' fst_cn_plot(edges = e1, nodes = n1, concepts = con1)
#'
#' con2 <- "kehitysmaa, auttaa, pyrkiä, maa, ihminen"
#' e2 <- fst_cn_edges(conllu_dev_q11_2_nltk, con2, threshold = 5)
#' n2 <- fst_cn_nodes(conllu_dev_q11_2_nltk, e2)
#' fst_cn_plot(edges = e2, nodes = n2, concepts = con2)
#'
#' con3 <- "köyhyys, nälänhätä, sota, ilmastonmuutos, puute"
#' e3 <- fst_cn_edges(conllu_dev_q11_3_nltk, con3, threshold = 2)
#' n3 <- fst_cn_nodes(conllu_dev_q11_3_nltk, e3)
#' fst_cn_plot(edges = e3, nodes = n3, concepts = con3)
#'
#' con4 <- "kiusata, lyöminen"
#' cb <- conllu_cb_bullying_iso
#' e4 <- fst_cn_edges(cb, con4, pos_filter = c("NOUN", "VERB", "ADJ", "ADV"))
#' n4 <- fst_cn_nodes(cb, e4, c("NOUN", "VERB", "ADJ", "ADV"))
#' fst_cn_plot(edges = e4, nodes = n4, concepts = con4)
fst_cn_plot <- function(edges, nodes, concepts, title = NULL) {
  if (stringr::str_detect(concepts, ",")) {
    concepts <- concepts %>%
      lapply(tolower) %>%
      stringr::str_extract_all(pattern = "\\w+") %>%
      unlist()
  }
  if (is.null(title)) {
    title <- "Concept Network of Textrank extracted keyword occurrences"
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
#' @param data A dataframe of text in CoNLL-U format.
#' @param concepts List of terms to search for, separated by commas.
#' @param threshold A minimum number of occurrences threshold for 'edge' between
#'  searched term and other word, default is `NULL`. Note, the threshold is
#'  applied before normalisation.
#' @param norm The method for normalising the data. Valid settings are
#'  `'number_words'` (the number of words in the responses, default),
#'  `'number_resp'` (the number of responses), or `NULL` (raw count returned).
#' @param pos_filter List of UPOS tags for inclusion, default is `NULL` to
#'  include all UPOS tags.
#' @param title Optional title for plot, default is `NULL` and a generic title
#'  ('Textrank extracted keyword occurences) will be used.
#'
#' @return Plot of concept network
#' @export
#'
#' @examples
#' fst_concept_network(conllu_dev_q11_3, concepts = "köyhyys, puute")
#'
#' con2 <- "köyhyys, nälänhätä, sota, ilmastonmuutos, puute"
#' fst_concept_network(conllu_dev_q11_3, concepts = con2)
#'
#' con3 <- "köyhyys, nälänhätä, sota, ilmastonmuutos, puute"
#' fst_concept_network(conllu_dev_q11_3, concepts = con3, threshold = 3)
#'
#' data4 <- conllu_cb_bullying_iso
#' con4 <- "kiusata, lyöminen"
#' pf4 <- c("NOUN", "VERB", "ADJ", "ADV")
#' title4 <- "Bullying Concept Network"
#' fst_concept_network(data4, concepts = con4, pos_filter = pf4, title = title4)
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
