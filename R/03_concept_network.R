#' Concept Network - Search Textrank for Concepts
#'
#' This function takes a string of terms (separated by commas) or a single term
#' and, using `textrank_keywords` from `textrank` package, filters data based on
#' `relevant_pos` and finds words connected to search terms.
#'
#' @param data A dataframe of text in CoNLL-U format.
#' @param concept String of terms to search for, separated by commas.
#' @param relevant_pos List of UPOS tags for inclusion, default is c("NOUN",
#' "VERB", "ADJ", "ADV").
#'
#' @return Dataframe of n-grams containing searched terms
#' @export
#'
#' @examples
#' lonely_concepts_2 <- fst_cn_search(conllu_lonely, 'yksinäisyys, tunne, tuntea')
#' lonely_concepts <- fst_cn_search(conllu_lonely_nltk, 'yksinäisyys, tunne, tuntea')
#' bullying_concepts2 <- fst_cn_search(conllu_bullying_iso, 'kiusata, lyöminen, lyödä, potkia')
fst_cn_search <- function(data,
                          concept,
                          relevant_pos = c("NOUN", "VERB", "ADJ", "ADV")) {
  if(stringr::str_detect(concept, ",")){
    concept <- stringr::str_extract_all(concept, pattern = "\\w+") %>%
      unlist()
  }
  data <- dplyr::filter(data, token != 'NA')
  x <- textrank::textrank_keywords(data$lemma, relevant=data$upos %in% relevant_pos)
  keyword_data <- x$keywords %>%
    dplyr::filter(ngram > 1 & freq > 1) %>%
    dplyr::mutate(word2 = strsplit(keyword, "-")) %>%
    tidyr::unnest(word2) %>%
    dplyr::group_by(keyword) %>%
    dplyr::mutate(word1 = dplyr::lag(word2)) %>%
    dplyr::relocate(word1, .before = word2) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(word1))
  concept_keywords <- keyword_data %>%
    dplyr::filter(word1 %in% concept)  %>%
    dplyr::pull(keyword)
  all_concepts <- keyword_data %>%
    dplyr::filter(keyword %in% concept_keywords)
  return(all_concepts)
}

#' Concept Network - Get Textrank Edges
#'
#' This function takes a string of terms (separated by commas) or a single term
#' and, using `fst_cn_search` find words conected to these searched terms. Then,
#' a dataframe is returned of 'edges' between two words which are connected
#' together in an frequently-occurring n-gram containing a concept term
#'
#' @param data A dataframe of text in CoNLL-U format.
#' @param concept List of terms to search for, separated by commas.
#' @param threshold A minimum number of occurrences threshold for 'edge' between
#' searched term and other word, default is NULL.
#' @param relevant_pos List of UPOS tags for inclusion, default is c("NOUN",
#' "VERB", "ADJ", "ADV").
#'
#' @return Dataframe of 'edges' between two connected words.
#' @export
#'
#' @examples
#' lonely_edges_2 <- fst_cn_edges(conllu_lonely, concept = 'yksinäisyys, tunne, tuntea', threshold=5)
#' lonely_edges <- fst_cn_edges(conllu_lonely_nltk, 'yksinäisyys, tunne, tuntea', threshold = 3)
#' bullying_edges <-fst_cn_edges(conllu_bullying_iso, 'kiusata, lyöminen')
fst_cn_edges <- function(data,
                         concept,
                         threshold = NULL,
                         relevant_pos = c("NOUN", "VERB", "ADJ", "ADV")) {
  data <- dplyr::filter(data, token != 'NA')
  df <-  data %>%
    fst_cn_search(concept = concept, relevant_pos = relevant_pos) %>%
    dplyr::select(word1, word2, freq) %>%
    dplyr::group_by(word1,word2) %>%
    dplyr::summarize(n = sum(freq), .groups = "drop") %>%
    dplyr::rename(from = word1,
           to = word2)
  if(!is.null(threshold)) {
    df <- df %>% dplyr::filter(n >= threshold)
  }
  return(df)
}

#' Concept Network - Get Textrank Nodes
#'
#' @param data A dataframe of text in CoNLL-U format.
#' @param edges Output of fst_cn_edges, dataframe of 'edges' connecting two words
#' @param relevant_pos List of UPOS tags for inclusion, default is c("NOUN",
#' "VERB", "ADJ", "ADV").
#'
#' @return A dataframe containing relevant lemmas and their associated pagerank
#' @export
#'
#' @examples
#' lonely_nodes_2 <- fst_cn_nodes(conllu_lonely, lonely_edges_2)
#' lonely_nodes <- fst_cn_nodes(conllu_lonely_nltk, lonely_edges)
#' bullying_nodes <- fst_cn_nodes(conllu_bullying, bullying_edges)
fst_cn_nodes <- function(data,
                         edges,
                         relevant_pos = c("NOUN", "VERB", "ADJ", "ADV")) {
  data <- dplyr::filter(data, token != 'NA')
  keyw <- textrank::textrank_keywords(data$lemma, relevant=data$upos %in% relevant_pos)
  textrank_data <- data.frame(pagerank = keyw$pagerank$vector) %>%
    tibble::rownames_to_column("lemma")
  keyword_vocab <- unique(c(edges$from, edges$to))
  df <- textrank_data %>% dplyr::filter(lemma %in% keyword_vocab)
  return(df)
}



#' Plot Concept Network
#'
#' @param edges Output of `fst_cn_edges`, dataframe of 'edges' connecting two words
#' @param nodes Output of `fst_cn_nodes`, dataframe of relevant lemmas and their associated pagerank
#' @param concepts List of terms which have been searched for, separated by commas.
#'
#' @return Plot of Concept Network
#' @export
#'
#' @examples
#' fst_cn_plot(edges = lonely_edges_2, nodes = lonely_nodes_2, concepts = 'yksinäisyys, tunne, tuntea')
#' fst_cn_plot(edges = lonely_edges, nodes = lonely_nodes, concepts = 'yksinäisyys, tunne, tuntea')
#' fst_cn_plot(edges = bullying_edges, nodes = bullying_nodes, concepts = 'kiusata, lyöminen')
fst_cn_plot <- function(edges, nodes, concepts) {
  if(stringr::str_detect(concepts, ",")){
    concepts <- concepts  %>% lapply(tolower) %>%
      stringr::str_extract_all(pattern = "\\w+") %>%
      unlist()
  }
  nodes <- nodes %>%
    dplyr::mutate(is_concept = factor(ifelse(lemma %in% concepts, 0, 1),
                                      levels = 0:1,
                                      labels = c("Concept word", "Regular word")))
  p <- igraph::graph_from_data_frame(edges,
                                     directed = FALSE, vertices = nodes) %>%
    ggraph::ggraph(layout = "kk") +
    ggraph::geom_edge_link( ggplot2::aes(width = n, alpha = n), colour = "lightblue") +
    ggraph::scale_edge_width(range=c(1, 5))+
    ggraph::scale_edge_alpha(range = c(0.2, 1)) +
    ggraph::geom_node_point( ggplot2::aes(size = pagerank)) +
    ggraph::geom_node_text(ggplot2::aes(label = name, col = is_concept), check_overlap = TRUE, repel = TRUE) +
    ggplot2::scale_color_manual("Word Type", values = c("Concept word" = "red", "Regular word" = "black")) +
    ggraph::theme_graph() +
    ggplot2::labs(
      title = "Textrank extracted keyword occurrences") +
    ggplot2::theme(legend.position = "right")

  return(p)
}

#' Concept Network - Make Concept Network Plot
#'
#' @param data A dataframe of text in CoNLL-U format.
#' @param input_word List of terms to search for, separated by commas.
#' @param threshold A minimum number of occurrences threshold for 'edge' between
#' searched term and other word, default is NULL.
#' @param relevant_pos List of UPOS tags for inclusion, default is c("NOUN",
#' "VERB", "ADJ", "ADV").
#'
#' @return Plot of concept network
#' @export
#'
#' @examples
#' fst_concept_network(conllu_lonely_nltk, concepts = "yksinäisyys, tunne, tuntea", threshold=3)
#' fst_concept_network(conllu_lonely_nltk, concepts = "tunne, tuntea", threshold=3)
#' fst_concept_network(conllu_lonely_nltk, concepts = "tunne, tuntea")
#' fst_concept_network(conllu_lonely, concepts = "yksinäisyys, tunne, tuntea", threshold=5)
#' fst_concept_network(conllu_bullying_iso concepts = 'kiusata, lyöminen')
fst_concept_network <- function(data,
                                concepts,
                                threshold = NULL,
                                relevant_pos = c("NOUN", "VERB", "ADJ", "ADV")) {
  edges <- fst_cn_edges(data = data,
                        concept = concepts,
                        threshold = threshold,
                        relevant_pos = relevant_pos)
  nodes <- fst_cn_nodes(data = data, edges, relevant_pos = relevant_pos)
  fst_cn_plot(edges, nodes, concepts = concepts)
}

