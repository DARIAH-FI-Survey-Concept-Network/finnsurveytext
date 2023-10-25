#' Concept Network- Get Unique Nodes
#'
#' Takes at least two tables of nodes and pagerank (output of `fst_cn_nodes`)
#' and finds nodes unique to one table.
#'
#' @param table1 The first table
#' @param table2 The second table
#' @param ... Any other tables you want to include
#'
#' @return Dataframe of words and whether word is unique or not
#' @export
#'
#' @examples
#' unique <- fst_cn_get_unique(female_nodes, male_nodes, na_nodes)
#' unique_2 <- fst_cn_get_unique(q11_1_nodes, q11_2_nodes)
#' unique_3 <- fst_cn_get_unique(q11_1_nodes, q11_2_nodes, q11_3_nodes)
fst_cn_get_unique <- function(table1, table2, ...) {
  df <- rbind(table1, table2, ...)
  df <- df %>%
    dplyr::mutate(n = 1) %>%
    dplyr::group_by(lemma) %>%
    dplyr::summarise(n = sum(n))
  unique <- dplyr::filter(df, n == 1)
  unique <- unique$lemma
  unique
}



#' Concept Network- Plot comparison Concept Network
#'
#' @param edges Output of `fst_cn_edges`, dataframe of 'edges' connecting two words
#' @param nodes Output of `fst_cn_nodes`, dataframe of relevant lemmas and their associated pagerank
#' @param concepts List of terms which have been searched for, separated by commas.
#' @param unique_lemmas List of unique lemmas, output of `fst_cn_get_unique`
#' @param name An optional "name" for the plot, default is `NULL` and a generic
#' title ('Textrank extracted keyword occurences) will be used.
#' @param concept_colour Colour to display concept words, default is `indianred`.
#' @param unique_colour Colour to display unique words, default is `darkgreen`.
#'
#' @return Plot of concept network with concept and unique words (nodes) highlighted
#' @export
#'
#' @examples
#' fst_cn_compare_plot(edges = q11_1_edges, nodes = q11_1_nodes, concepts = 'elintaso, köyhä, ihminen', unique_lemma = unique_2, name = "Q11_1")
#' fst_cn_compare_plot(q11_2_edges, q11_2_nodes, concepts = "kehitysmaa, auttaa, pyrkiä, maa, ihminen", unique_lemma = unique_2, unique_colour = 'purple' )
fst_cn_compare_plot <- function(edges, nodes, concepts, unique_lemmas, name = NULL,
                                concept_colour = "indianred", unique_colour = "darkgreen") {
  if(stringr::str_detect(concepts, ",")){
    concepts <- concepts  %>% lapply(tolower) %>%
      stringr::str_extract_all(pattern = "\\w+") %>%
      unlist()
  }
  if (is.null(name)){
    name = "Textrank extracted keyword occurences"
  }
  nodes <- nodes %>%
    dplyr::mutate(is_concept = factor(ifelse(lemma %in% concepts, 0, ifelse(lemma %in% unique_lemmas, 1, 2)),
                                      levels = c(0,1,2),
                                      labels = c("Concept word", "Unique Word", "Common word")))
  p <- igraph::graph_from_data_frame(edges,
                                     directed = FALSE, vertices = nodes) %>%
    ggraph::ggraph(layout = "kk") +
    ggraph::geom_edge_link( ggplot2::aes(width = n, alpha = n), colour = "lightblue") +
    ggraph::scale_edge_width(range=c(1, 5))+
    ggraph::scale_edge_alpha(range = c(0.2, 1)) +
    ggraph::geom_node_point( ggplot2::aes(size = pagerank)) +
    ggraph::geom_node_text(ggplot2::aes(label = name, col = is_concept), check_overlap = TRUE, repel = TRUE) +
    ggplot2::scale_color_manual("Word Type", values = c("Concept word" = concept_colour, "Unique Word" = unique_colour, "Common word" = "black")) +
    ggraph::theme_graph() +
    ggplot2::labs(
      title = name) +
    ggplot2::theme(legend.position = "right")
  return(p)
}



#' Concept Network- Compare and plot Concept Network
#'
#' @param data1 A dataframe of text in CoNLL-U format for the first concept
#' network
#' @param data2 A dataframe of text in CoNLL-U format for the second concept
#' network
#' @param data3 An optional dataframe of text in CoNLL-U format for the third
#' concept network, default is `NULL`
#' @param data4 An optional dataframe of text in CoNLL-U format for the fourth
#' concept network, default is `NULL`
#' @param pos_filter List of UPOS tags for inclusion, default is `NULL` which
#'  means all word types included
#' @param name1 A string describing data1, default is `Group 1`
#' @param name2 A string describing data2, default is `Group 2`
#' @param name3 A string describing data3, default is `Group 3`
#' @param name4 A string describing data4, default is `Group 4`
#' @param concepts List of terms to search for, separated by commas.
#' @param threshold A minimum number of occurrences threshold for 'edge' between
#' searched term and other word, default is `NULL`.
#'
#' @return Between 2 and 4 concept network plots with concept and unique words
#' highlighted.
#' @export
#'
#' @examples
#' fst_concept_network_compare(conllu_dev_q11_1_nltk, conllu_dev_q11_2_nltk, concepts = 'elintaso, köyhä, ihminen', name1 = 'Q1', name2 = 'Q2', pos_filter = c("NOUN", "VERB", "ADJ", "ADV"))
#' fst_concept_network_compare(conllu_dev_q11_1_nltk, conllu_dev_q11_2_nltk, conllu_dev_q11_3_nltk, concepts = 'elintaso, köyhä, ihminen', name1 = 'Q1', name2 = 'Q2', name3 = 'Q3', pos_filter = c("NOUN", "VERB", "ADJ", "ADV"))
#' fst_concept_network_compare(conllu_dev_q11_1_f, conllu_dev_q11_1_m, concepts = 'elintaso, köyhä, ihminen', pos_filter = c("NOUN", "VERB", "ADJ", "ADV"))
fst_concept_network_compare <- function(data1, data2, data3 = NULL, data4 = NULL, pos_filter = NULL, name1 = "Group 1", name2 = "Group 2", name3 = "Group 3", name4 = "Group 4", concepts, threshold = NULL) {
  if (!is.null(data3)){
    if (!is.null(data4)){
      edges4 <- fst_cn_edges(data = data4, concepts = concepts, threshold = threshold, pos_filter = pos_filter)
      edges3 <- fst_cn_edges(data = data3, concepts = concepts, threshold = threshold, pos_filter = pos_filter)
      edges2 <- fst_cn_edges(data = data2, concepts = concepts, threshold = threshold, pos_filter = pos_filter)
      edges1 <- fst_cn_edges(data = data1, concepts = concepts, threshold = threshold, pos_filter = pos_filter)
      nodes4 <- fst_cn_nodes(data = data4, edges4, pos_filter = pos_filter)
      nodes3 <- fst_cn_nodes(data = data3, edges3, pos_filter = pos_filter)
      nodes2 <- fst_cn_nodes(data = data2, edges2, pos_filter = pos_filter)
      nodes1 <- fst_cn_nodes(data = data1, edges1, pos_filter = pos_filter)
    } else {
      edges3 <- fst_cn_edges(data = data3, concepts = concepts, threshold = threshold, pos_filter = pos_filter)
      edges2 <- fst_cn_edges(data = data2, concepts = concepts, threshold = threshold, pos_filter = pos_filter)
      edges1 <- fst_cn_edges(data = data1, concepts = concepts, threshold = threshold, pos_filter = pos_filter)
      nodes3 <- fst_cn_nodes(data = data3, edges3, pos_filter = pos_filter)
      nodes2 <- fst_cn_nodes(data = data2, edges2, pos_filter = pos_filter)
      nodes1 <- fst_cn_nodes(data = data1, edges1, pos_filter = pos_filter)
    }
  } else {
    edges2 <- fst_cn_edges(data = data2, concepts = concepts, threshold = threshold, pos_filter = pos_filter)
    edges1 <- fst_cn_edges(data = data1, concepts = concepts, threshold = threshold, pos_filter = pos_filter)
    nodes2 <- fst_cn_nodes(data = data2, edges2, pos_filter = pos_filter)
    nodes1 <- fst_cn_nodes(data = data1, edges1, pos_filter = pos_filter)
  }
  num1 <- dplyr::n_distinct(data1$doc_id)
  num2 <- dplyr::n_distinct(data2$doc_id)
  if (!is.null(data3)){
    num3 <- dplyr::n_distinct(data3$doc_id)
    if (!is.null(data4)){
      num4 <- dplyr::n_distinct(data4$doc_id)
      message(paste0("Note: \n Consider whether your data is balanced between groups being compared and whether each group contains enough data for analysis. \n The number of responses in each group (including \'NAs\') are listed below: \n\t", name1, "=", num1, ", ", name2, "=", num2, ", ", name3, "=", num3, ", ", name4, "=", num4, "\n\n"))
      unique <- fst_cn_get_unique(nodes1, nodes2, nodes3, nodes4)
      plot4 <- fst_cn_compare_plot(edges4, nodes4, name = name4, concepts = concepts, unique_lemma = unique)
      plot3 <- fst_cn_compare_plot(edges3, nodes3, name = name3, concepts = concepts, unique_lemma = unique)
      plot2 <- fst_cn_compare_plot(edges2, nodes2, name = name2, concepts = concepts, unique_lemma = unique)
      plot1 <- fst_cn_compare_plot(edges1, nodes1, name = name1, concepts = concepts, unique_lemma = unique)
      fst_plot_multiple(plot1 = plot1, plot2 = plot2, plot3 = plot3, plot4 = plot4, main_title = paste("Comparison Plot of Concept Networks"))
    } else {
      message(paste0("Note: \n Consider whether your data is balanced between groups being compared and whether each group contains enough data for analysis. \n The number of responses in each group (including \'NAs\') are listed below: \n\t", name1, "=", num1, ", ", name2, "=", num2, ", ", name3, "=", num3, "\n\n"))
      unique <- fst_cn_get_unique(nodes1, nodes2, nodes3)
      plot3 <- fst_cn_compare_plot(edges3, nodes3, name = name3, concepts = concepts, unique_lemma = unique)
      plot2 <- fst_cn_compare_plot(edges2, nodes2, name = name2, concepts = concepts, unique_lemma = unique)
      plot1 <- fst_cn_compare_plot(edges1, nodes1, name = name1, concepts = concepts, unique_lemma = unique)
      fst_plot_multiple(plot1 = plot1, plot2 = plot2, plot3 = plot3, main_title = paste("Comparison Plot of Concept Networks"))
    }
  } else {
    message(paste0("Note: \n Consider whether your data is balanced between groups being compared and whether each group contains enough data for analysis. \n The number of responses in each group (including \'NAs\') are listed below: \n\t", name1, "=", num1, ", ", name2, "=", num2, "\n\n"))
    unique <- fst_cn_get_unique(nodes1, nodes2)
    plot2 <- fst_cn_compare_plot(edges2, nodes2, name = name2, concepts = concepts, unique_lemma = unique)
    plot1 <- fst_cn_compare_plot(edges1, nodes1, name = name1, concepts = concepts, unique_lemma = unique)
    fst_plot_multiple(plot1 = plot1, plot2 = plot2, main_title = paste("Comparison Plot of Concept Networks"))
  }
}
