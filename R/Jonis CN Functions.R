find_concepts <- function(data, concept) {

  if(stringr::str_detect(concept, ",")){
    concept <- stringr::str_extract_all(concept, pattern = "\\w+") %>%
      unlist()

    cat("Displaying results including following concepts:", "\n",
        paste(paste(1:length(concept) ,"\t", concept, "\n"), sep = "\n"), "\n")
  }

  concept_keywords <- data %>%
    dplyr::filter(word1 %in% concept)  %>%
    dplyr::pull(keyword)

  all_concepts <- data %>%
    dplyr::filter(keyword %in% concept_keywords)

  return(all_concepts)
}

get_edges <- function(data, concept, threshold = NULL) {
  df <-  data %>%
    find_concepts(concept = concept) %>%
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

get_nodes <- function(data, vocab) {
  df <- data %>% dplyr::filter(lemma %in% vocab)

  return(df)
}

plot_graph <- function(edges, nodes, concepts, ...) {

  nodes <- nodes %>%
    dplyr::mutate(is_concept = factor(ifelse(lemma %in% concepts, 0, 1), levels = 0:1, labels = c("Concept word", "Regular word")))

  p <- igraph::graph_from_data_frame(edges, directed = FALSE, vertices = nodes) %>%
    ggraph::ggraph(layout = "kk") +
    ggraph::geom_edge_link(aes(width = n, edge_alpha = n), edge_colour = "lightblue") +
    ggraph::geom_node_point(aes(size = pagerank)) +
    ggraph::geom_node_text(aes(label = name, col = is_concept), check_overlap = TRUE, repel = TRUE) +
    ggplot2::scale_color_manual("", values = c("Concept word" = "red", "Regular word" = "black")) +
    ggraph::theme_graph() +
    ggplot2::labs(
      title = "Textrank extracted keyword occurences",
      subtitle = "Adjectives, Nouns, Verbs",
      ... ) +
    ggplot2::theme(legend.position = "right")

  return(p)
}

textrank_table <- function(x, n = 25) {
  keyword_table <- x$keywords %>%
    dplyr::filter(ngram > 1 & freq > 1) %>%
    dplyr::slice_max(freq, n = n)

  return(keyword_table)
}

textrank_graph <- function(x, input_word, threshold) {

  keyword_data <- x$keywords %>%
    dplyr::filter(ngram > 1 & freq > 1) %>%
    dplyr::mutate(word2 = strsplit(keyword, "-")) %>%
    tidyr::unnest(word2) %>%
    dplyr::group_by(keyword) %>%
    dplyr::mutate(word1 = lag(word2)) %>%
    dplyr::relocate(word1, .before = word2) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(word1))


  textrank_data <- data.frame(pagerank = keyw$pagerank$vector) %>%
    tibble::rownames_to_column("lemma")


  edges <- get_edges(keyword_data, concept = input_word, threshold = threshold)
  keyword_vocab <- unique(c(edges$from, edges$to))
  nodes <- get_nodes(textrank_data, vocab = keyword_vocab)
  plot_graph(edges, nodes, concepts = query_list, caption = paste("Min.", threshold, "occurences."))
}


# # #### RUN FUNCTIONS
# df <- conllu_lonely %>% dplyr::filter(lemma != 'NA')
# relevant_pos <- c("NOUN", "VERB", "ADJ", "ADV")
#
# keyw <- textrank::textrank_keywords(df$lemma, relevant = df$upos %in% relevant_pos)
# textrank_table(keyw, n = 20)
#
# query <- tolower(readline(prompt="Enter words (separated by a comma):"))
# #yksinäisyys, tunne, tunte
# query_list <- stringr::str_extract_all(query, pattern = "\\w+") %>% unlist()
#
# set.seed(2022)
# vocab <- unique(df$lemma)
# min_occurences <- 5
#
# while(TRUE) {
#   valid_words = query_list %in% vocab
#
#   if(tolower(query) == "c"){
#     interactive_status <- FALSE
#     cat("Stopping.")
#     break
#   }
#
#
#
#   if(any(valid_words != TRUE)){
#
#     cat("Following words not found in vocabulary:", paste(query_list[!valid_words], collapse = ", "), "\n")
#     flush.console()
#
#     query <- tolower(readline(prompt="Re-enter words: (or C to stop)"))
#     query_list <- str_extract_all(query, pattern = "\\w+") %>% unlist()
#
#   }
#
#   if(all(valid_words) == TRUE){
#     interactive_status <- TRUE
#     found_data = df %>% dplyr::filter(lemma %in% query_list)
#     break
#   }
#
#
# }
#
# textrank_graph(keyw, input_word = query, threshold = min_occurences)
