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
#' q11_1_concepts <- fst_cn_search(conllu_dev_q11_1_nltk, "elintaso, köyhä, ihminen", relevant_pos = "NOUN")
#' q11_2_concepts <- fst_cn_search(conllu_dev_q11_2_nltk, "kehitysmaa, auttaa, pyrkiä, maa, ihminen", relevant_pos = c("ADV", "ADJ"))
#' q11_3_concepts <- fst_cn_search(conllu_dev_q11_3_nltk, "köyhyys, nälänhätä, sota, ilmastonmuutos, puute")
#' bullying_concepts <- fst_cn_search(conllu_bullying_iso, 'kiusata, lyöminen, lyödä, potkia')
fst_cn_search <- function(data,
                          concept,
                          relevant_pos = c("NOUN", "VERB", "ADJ", "ADV")) {
  if(stringr::str_detect(concept, ",")){
    concept <- stringr::str_extract_all(concept, pattern = "\\w+") %>%
      unlist()
  }
  data <- dplyr::filter(data, token != 'na')
  data$lemma <- stringr::str_replace_all(data$lemma,'-','@')
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
  keyword_data$word2 <- stringr::str_replace_all(keyword_data$word2, '@', '-')
  keyword_data$word1 <- stringr::str_replace_all(keyword_data$word1, '@', '-')
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
#' q11_1_edges <- fst_cn_edges(conllu_dev_q11_1_nltk, 'elintaso, köyhä, ihminen', threshold = 3)
#' q11_2_edges <- fst_cn_edges(conllu_dev_q11_2_nltk, "kehitysmaa, auttaa, pyrkiä, maa, ihminen", threshold = 5)
#' q11_3_edges <- fst_cn_edges(conllu_dev_q11_3_nltk, "köyhyys, nälänhätä, sota, ilmastonmuutos, puute", threshold = 2)
#' bullying_edges <-fst_cn_edges(conllu_bullying_iso, 'kiusata, lyöminen')
fst_cn_edges <- function(data,
                         concept,
                         threshold = NULL,
                         relevant_pos = c("NOUN", "VERB", "ADJ", "ADV")) {
  data <- dplyr::filter(data, token != 'na')
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
#' q11_1_nodes <- fst_cn_nodes(conllu_dev_q11_1_nltk, q11_1_edges)
#' q11_2_nodes <- fst_cn_nodes(conllu_dev_q11_2_nltk, q11_2_edges)
#' q11_3_nodes <- fst_cn_nodes(conllu_dev_q11_3_nltk, q11_3_edges)
#' bullying_nodes <- fst_cn_nodes(conllu_bullying_iso, bullying_edges)
fst_cn_nodes <- function(data,
                         edges,
                         relevant_pos = c("NOUN", "VERB", "ADJ", "ADV")) {
  data <- dplyr::filter(data, token != 'na')
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
#' fst_cn_plot(edges = q11_1_edges, nodes = q11_1_nodes, concepts = 'elintaso, köyhä, ihminen')
#' fst_cn_plot(edges = q11_2_edges, nodes = q11_2_nodes, concepts = "kehitysmaa, auttaa, pyrkiä, maa, ihminen")
#' fst_cn_plot(edges = q11_3_edges, nodes = q11_3_nodes, concepts = "köyhyys, nälänhätä, sota, ilmastonmuutos, puute")
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
#' fst_concept_network(conllu_dev_q11_3, concepts = "köyhyys, puute")
#' fst_concept_network(conllu_dev_q11_3, concepts = "köyhyys, nälänhätä, sota, ilmastonmuutos, puute")
#' fst_concept_network(conllu_dev_q11_3, concepts = "köyhyys, nälänhätä, sota, ilmastonmuutos, puute", threshold = 3)
#' fst_concept_network(conllu_bullying_iso, concepts = 'kiusata, lyöminen')
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

