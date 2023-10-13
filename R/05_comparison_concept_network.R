# female_concepts <- fst_cn_search(conllu_dev_q11_1_f, "elintaso, köyhä, ihminen")
# male_concepts <- fst_cn_search(conllu_dev_q11_1_m, "elintaso, köyhä, ihminen")
# na_concepts <- fst_cn_search(conllu_dev_q11_1_na, "elintaso, köyhä, ihminen")
# all_concepts <- fst_cn_search(conllu_dev_q11_1, "elintaso, köyhä, ihminen")
#
# female_edges <- fst_cn_edges(conllu_dev_q11_1_f, 'elintaso, köyhä, ihminen', threshold = 1)
# male_edges <- fst_cn_edges(conllu_dev_q11_1_m, 'elintaso, köyhä, ihminen', threshold = 1)
# na_edges <- fst_cn_edges(conllu_dev_q11_1_na, 'elintaso, köyhä, ihminen', threshold = 1)
# all_edges <- fst_cn_edges(conllu_dev_q11_1, 'elintaso, köyhä, ihminen', threshold = 1)
#
# female_nodes <- fst_cn_nodes(conllu_dev_q11_1_f, female_edges)
# male_nodes <- fst_cn_nodes(conllu_dev_q11_1_m, male_edges)
# na_nodes <- fst_cn_nodes(conllu_dev_q11_1_na, na_edges)
# all_nodes <- fst_cn_nodes(conllu_dev_q11_1, all_edges)


#' Concept Network- Get Unique Nodes
#'
#' Takes at least two tables of nodes and pagerank (output of `fst_cn_nodes`)
#' and finds nodes unique to one table.
#'
#' @param table1 The first table
#' @param table2 The second table
#' @param ... Any other tables you want to include
#'
#' @return
#' @export
#'
#' @examples
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
}

# uni <- fst_get_unique_nodes_list(female_nodes, male_nodes, na_nodes)
# f_plot <- fst_cn_compare_plot(female_edges, female_nodes, name = 'Female', concepts = 'elintaso, köyhä, ihminen', unique_lemma = uni)
# m_plot <- fst_cn_compare_plot(male_edges, male_nodes, name = 'Male', concepts = 'elintaso, köyhä, ihminen', unique_lemma = uni)
# na_plot <- fst_cn_compare_plot(na_edges, na_nodes, name = 'NA', concepts = 'elintaso, köyhä, ihminen', unique_lemma = uni)
# all_plot <- fst_cn_compare_plot(all_edges, all_nodes, name = 'All', concepts = 'elintaso, köyhä, ihminen', unique_lemma = uni)
# fst_plot_multiple(all_plot, f_plot, m_plot, na_plot, "Q11_1 Split by Gender")
# fst_plot_multiple(all_plot, f_plot, m_plot, na_plot, "Q11_1 Split by Gender")
# fst_plot_multiple(all_plot, f_plot)
# fst_plot_multiple(m_plot, f_plot)


# fst_cn_compare_plot(edges = q11_1_edges, nodes = q11_1_nodes, concepts = 'elintaso, köyhä, ihminen', unique_lemma = unique_2, name = "Q11_1")

# Concept Network- Plot Comparison Concept Network
fst_cn_compare_plot <- function(edges, nodes, concepts, unique_lemmas, name = NULL) {
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
                                      labels = c("Concept word", "Unique Word", "Regular word")))
  p <- igraph::graph_from_data_frame(edges,
                                     directed = FALSE, vertices = nodes) %>%
    ggraph::ggraph(layout = "kk") +
    ggraph::geom_edge_link( ggplot2::aes(width = n, alpha = n), colour = "lightblue") +
    ggraph::scale_edge_width(range=c(1, 5))+
    ggraph::scale_edge_alpha(range = c(0.2, 1)) +
    ggraph::geom_node_point( ggplot2::aes(size = pagerank)) +
    ggraph::geom_node_text(ggplot2::aes(label = name, col = is_concept), check_overlap = TRUE, repel = TRUE) +
    ggplot2::scale_color_manual("Word Type", values = c("Concept word" = "red", "Unique Word" = "purple3", "Common word" = "black")) +
    ggraph::theme_graph() +
    ggplot2::labs(
      title = name) +
    ggplot2::theme(legend.position = "right")
  return(p)
}

# fst_concept_network_compare(conllu_dev_q11_1_nltk, conllu_dev_q11_2_nltk, concepts = 'elintaso, köyhä, ihminen', name1 = 'Q1', name2 = 'Q2', pos_filter = c("NOUN", "VERB", "ADJ", "ADV"))
# fst_concept_network_compare(conllu_dev_q11_1_nltk, conllu_dev_q11_2_nltk, conllu_dev_q11_3_nltk, concepts = 'elintaso, köyhä, ihminen', name1 = 'Q1', name2 = 'Q2', name3 = 'Q3', pos_filter = c("NOUN", "VERB", "ADJ", "ADV"))
# fst_concept_network_compare(conllu_dev_q11_1_f, conllu_dev_q11_1_m, concepts = 'elintaso, köyhä, ihminen', pos_filter = c("NOUN", "VERB", "ADJ", "ADV"))

# Concept Network- Compare and Plot Concept Network
fst_concept_network_compare <- function(data1, data2, data3 = NULL, data4 = NULL, pos_filter = NULL, name1 = "Group 1", name2 = "Group 2", name3 = "Group 3", name4 = "Group 4", concepts, threshold = NULL) {
  if (!is.null(data3)){
    if (!is.null(data4)){
      edges4 <- fst_cn_edges(data = data4, concepts = concepts, threshold = threshold, relevant_pos = pos_filter)
      edges3 <- fst_cn_edges(data = data3, concepts = concepts, threshold = threshold, relevant_pos = pos_filter)
      edges2 <- fst_cn_edges(data = data2, concepts = concepts, threshold = threshold, relevant_pos = pos_filter)
      edges1 <- fst_cn_edges(data = data1, concepts = concepts, threshold = threshold, relevant_pos = pos_filter)
      nodes4 <- fst_cn_nodes(data = data4, edges4, relevant_pos = pos_filter)
      nodes3 <- fst_cn_nodes(data = data3, edges3, relevant_pos = pos_filter)
      nodes2 <- fst_cn_nodes(data = data2, edges2, relevant_pos = pos_filter)
      nodes1 <- fst_cn_nodes(data = data1, edges1, relevant_pos = pos_filter)
    } else {
      edges3 <- fst_cn_edges(data = data3, concepts = concepts, threshold = threshold, relevant_pos = pos_filter)
      edges2 <- fst_cn_edges(data = data2, concepts = concepts, threshold = threshold, relevant_pos = pos_filter)
      edges1 <- fst_cn_edges(data = data1, concepts = concepts, threshold = threshold, relevant_pos = pos_filter)
      nodes3 <- fst_cn_nodes(data = data3, edges3, relevant_pos = pos_filter)
      nodes2 <- fst_cn_nodes(data = data2, edges2, relevant_pos = pos_filter)
      nodes1 <- fst_cn_nodes(data = data1, edges1, relevant_pos = pos_filter)
    }
  } else {
    edges2 <- fst_cn_edges(data = data2, concepts = concepts, threshold = threshold, relevant_pos = pos_filter)
    edges1 <- fst_cn_edges(data = data1, concepts = concepts, threshold = threshold, relevant_pos = pos_filter)
    nodes2 <- fst_cn_nodes(data = data2, edges2, relevant_pos = pos_filter)
    nodes1 <- fst_cn_nodes(data = data1, edges1, relevant_pos = pos_filter)
  }
  num1 <- dplyr::n_distinct(data1$doc_id)
  num2 <- dplyr::n_distinct(data2$doc_id)
  if (!is.null(data3)){
    num3 <- dplyr::n_distinct(data3$doc_id)
    if (!is.null(data4)){
      num4 <- dplyr::n_distinct(data4$doc_id)
      message(paste0("Note: \n Consider whether your data is balanced between groups being compared and whether each group contains enough data for analysis. \n The number of responded in each group (including \'NAs\') are listed below: \n\t", name1, "=", num1, ", ", name2, "=", num2, ", ", name3, "=", num3, ", ", name4, "=", num4, "\n"))
      unique <- fst_cn_get_unique(nodes1, nodes2, nodes3, nodes4)
      plot4 <- fst_cn_compare_plot(edges4, nodes4, name = name4, concepts = concepts, unique_lemma = unique)
      plot3 <- fst_cn_compare_plot(edges3, nodes3, name = name3, concepts = concepts, unique_lemma = unique)
      plot2 <- fst_cn_compare_plot(edges2, nodes2, name = name2, concepts = concepts, unique_lemma = unique)
      plot1 <- fst_cn_compare_plot(edges1, nodes1, name = name1, concepts = concepts, unique_lemma = unique)
      fst_plot_multiple(plot1 = plot1, plot2 = plot2, plot3 = plot3, plot4 = plot4, main_title = paste("Comparison Plot of Concept Networks"))
    } else {
      message(paste0("Note: \n Consider whether your data is balanced between groups being compared and whether each group contains enough data for analysis. \n The number of responded in each group (including \'NAs\') are listed below: \n\t", name1, "=", num1, ", ", name2, "=", num2, ", ", name3, "=", num3, "\n"))
      unique <- fst_cn_get_unique(nodes1, nodes2, nodes3)
      plot3 <- fst_cn_compare_plot(edges3, nodes3, name = name3, concepts = concepts, unique_lemma = unique)
      plot2 <- fst_cn_compare_plot(edges2, nodes2, name = name2, concepts = concepts, unique_lemma = unique)
      plot1 <- fst_cn_compare_plot(edges1, nodes1, name = name1, concepts = concepts, unique_lemma = unique)
      fst_plot_multiple(plot1 = plot1, plot2 = plot2, plot3 = plot3, main_title = paste("Comparison Plot of Concept Networks"))
    }
  } else {
    message(paste0("Note: \n Consider whether your data is balanced between groups being compared and whether each group contains enough data for analysis. \n The number of responded in each group (including \'NAs\') are listed below: \n\t", name1, "=", num1, ", ", name2, "=", num2, "\n"))
    unique <- fst_cn_get_unique(nodes1, nodes2)
    plot2 <- fst_cn_compare_plot(edges2, nodes2, name = name2, concepts = concepts, unique_lemma = unique)
    plot1 <- fst_cn_compare_plot(edges1, nodes1, name = name1, concepts = concepts, unique_lemma = unique)
    fst_plot_multiple(plot1 = plot1, plot2 = plot2, main_title = paste("Comparison Plot of Concept Networks"))
  }
}
