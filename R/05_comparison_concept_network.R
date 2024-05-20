#' Concept Network- Get Unique Nodes
#'
#' Takes at least two tables of nodes and pagerank (output of `fst_cn_nodes()`)
#' and finds nodes unique to one table.
#'
#' @param table1 The first table.
#' @param table2 The second table.
#' @param ... Any other tables you want to include.
#'
#' @return Dataframe of words and whether word is unique or not.
#' @export
#'
#' @examples
#' pos_filter <- c("NOUN", "VERB", "ADJ", "ADV")
#' e1 <- fst_cn_edges(fst_child, "lyödä", pos_filter = pos_filter)
#' e2 <- fst_cn_edges(fst_child, "lyöminen", pos_filter = pos_filter)
#' n1 <- fst_cn_nodes(fst_child, e1)
#' n2 <- fst_cn_nodes(fst_child, e2)
#' fst_cn_get_unique(n1, n2)
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
#' Creates a Concept Network plot from a list of edges and nodes (and their
#' respective weights) which indicates unique words in this plot in comparison
#' to another Network.
#'
#' @param edges Output of `fst_cn_edges()`, dataframe of 'edges' connecting
#'  two words.
#' @param nodes Output of `fst_cn_nodes()`, dataframe of relevant lemmas and
#'  their associated pagerank.
#' @param concepts List of terms which have been searched for, separated by
#'  commas.
#' @param unique_lemmas List of unique lemmas, output of `fst_cn_get_unique()`
#' @param name An optional "name" for the plot, default is `NULL` and a generic
#'  title ("TextRank extracted keyword occurrences") will be used.
#' @param concept_colour Colour to display concept words, default is
#'  `"indianred"`.
#' @param unique_colour Colour to display unique words, default is `"darkgreen"`.
#' @param min_edge A numeric value for the scale of the edges, the smallest
#'  co_occurrence value for an edge across all Networks to be plotted together.
#' @param max_edge A numeric value for the scale of the edges, the largest
#'  co_occurrence value for an edge across all Networks to be plotted together.
#' @param min_node A numeric value for the scale of the nodes, the smallest
#'  pagerank value for a node across all Networks to be plotted together.
#' @param max_node A numeric value for the scale of the nodes, the largest
#'  pagerank value for a node across all Networks to be plotted together.
#'
#' @return Plot of concept network with concept and unique words (nodes)
#'  highlighted.
#' @export
#'
#' @examples
#' pos_filter <- c("NOUN", "VERB", "ADJ", "ADV")
#' e1 <- fst_cn_edges(fst_child, "lyödä", pos_filter = pos_filter)
#' e2 <- fst_cn_edges(fst_child, "lyöminen", pos_filter = pos_filter)
#' n1 <- fst_cn_nodes(fst_child, e1)
#' n2 <- fst_cn_nodes(fst_child, e2)
#' u <- fst_cn_get_unique(n1, n2)
#'
#' fst_cn_compare_plot(e1, n1, "lyödä", unique_lemma = u)
#' fst_cn_compare_plot(e2, n2, "lyöminen", u, unique_colour = "purple")
fst_cn_compare_plot <- function(edges,
                                nodes,
                                concepts,
                                unique_lemmas,
                                name = NULL,
                                concept_colour = "#cd1719",
                                unique_colour = "#4DAF4A",
                                min_edge = NULL,
                                max_edge = NULL,
                                min_node = NULL,
                                max_node = NULL) {
  if (is.null(min_edge)) {
    min_edge <- min(edges$co_occurrence)
  }
  if (is.null(max_edge)) {
    max_edge <- max(edges$co_occurrence)
  }
  if (is.null(min_node)) {
    min_node <- min(nodes$pagerank)
  }
  if (is.null(max_node)) {
    max_node <- max(nodes$pagerank)
  }
  if (stringr::str_detect(concepts, ",")) {
    concepts <- concepts %>%
      lapply(tolower) %>%
      stringr::str_extract_all(pattern = "\\w+") %>%
      unlist()
  }
  if (is.null(name)) {
    name <- "TextRank extracted keyword occurences"
  }
  nodes <- nodes %>%
    dplyr::mutate(is_concept = factor(ifelse(lemma %in% concepts, 0, ifelse(lemma %in% unique_lemmas, 1, 2)),
      levels = c(0, 1, 2),
      labels = c("Concept word", "Unique Word", "Common word")
    ))
  p <- igraph::graph_from_data_frame(edges,
    directed = FALSE, vertices = nodes
  ) %>%
    ggraph::ggraph(layout = "kk") +
    ggraph::geom_edge_link(ggplot2::aes(width = co_occurrence, alpha = co_occurrence), colour = "#6da5d3") +
    ggraph::scale_edge_width_continuous(range = c(1, 5), limits = c(min_edge, max_edge)) +
    ggraph::scale_edge_alpha_continuous(range = c(0.2, 1), limits = c(min_edge, max_edge)) +
    ggraph::geom_node_point(ggplot2::aes(size = pagerank)) +
    ggplot2::scale_size(limit = c(min_node, max_node)) +
    ggraph::geom_node_text(ggplot2::aes(label = name, col = is_concept), check_overlap = TRUE, repel = TRUE) +
    ggplot2::scale_color_manual("Word Type", values = c("Concept word" = concept_colour, "Unique Word" = unique_colour, "Common word" = "black")) +
    ggraph::theme_graph(base_family = "sans") +
    ggplot2::labs(
      title = name
    ) #+
  # ggplot2::theme(legend.position = "none")
  return(p)
}



#' Concept Network- Compare and plot Concept Network
#'
#' This function takes a string of terms (separated by commas) or a single term
#' and, using `textrank_keywords()` from `textrank` package, filters data based
#' on `pos_filter` and finds words connected to search terms for each group.
#' Then it plots a Concept Network for each group based on the calculated
#' weights of these terms and the frequency of co-occurrences, indicating any
#' words that are unique to each group's Network plot.
#'
#' @param data1 A dataframe of text in CoNLL-U format for the first concept
#'  network.
#' @param data2 A dataframe of text in CoNLL-U format for the second concept
#'  network.
#' @param data3 An optional dataframe of text in CoNLL-U format for the third
#'  concept network, default is `NULL`.
#' @param data4 An optional dataframe of text in CoNLL-U format for the fourth
#'  concept network, default is `NULL`.
#' @param pos_filter List of UPOS tags for inclusion, default is `NULL` which
#'  means all word types included.
#' @param name1 A string describing data1, default is `"Group 1"`.
#' @param name2 A string describing data2, default is `"Group 2"`.
#' @param name3 A string describing data3, default is `"Group 3"`.
#' @param name4 A string describing data4, default is `"Group 4"`.
#' @param concepts List of terms to search for, separated by commas.
#' @param norm The method for normalising the data. Valid settings are
#'  `"number_words"` (the number of words in the responses, default),
#'  `"number_resp"` (the number of responses), or `NULL` (raw count returned).
#' @param threshold A minimum number of occurrences threshold for 'edge' between
#'  searched term and other word, default is `NULL`.
#'
#' @return Between 2 and 4 concept network plots with concept and unique words
#' highlighted.
#' @export
#'
#' @examples
#' con1 <- "lyödä, lyöminen"
#' fst_concept_network_compare(fst_child, fst_child_2, concepts = con1)
fst_concept_network_compare <- function(data1, data2, data3 = NULL, data4 = NULL, pos_filter = NULL, name1 = "Group 1", name2 = "Group 2", name3 = "Group 3", name4 = "Group 4", concepts, norm = "number_words", threshold = NULL) {
  if (!is.null(data3)) {
    if (!is.null(data4)) {
      edges4 <- fst_cn_edges(data = data4, concepts = concepts, norm = norm, threshold = threshold, pos_filter = pos_filter)
      edges3 <- fst_cn_edges(data = data3, concepts = concepts, norm = norm, threshold = threshold, pos_filter = pos_filter)
      edges2 <- fst_cn_edges(data = data2, concepts = concepts, norm = norm, threshold = threshold, pos_filter = pos_filter)
      edges1 <- fst_cn_edges(data = data1, concepts = concepts, norm = norm, threshold = threshold, pos_filter = pos_filter)
      nodes4 <- fst_cn_nodes(data = data4, edges4, pos_filter = pos_filter)
      nodes3 <- fst_cn_nodes(data = data3, edges3, pos_filter = pos_filter)
      nodes2 <- fst_cn_nodes(data = data2, edges2, pos_filter = pos_filter)
      nodes1 <- fst_cn_nodes(data = data1, edges1, pos_filter = pos_filter)
      min_edge <- min(min(edges1$co_occurrence), min(edges2$co_occurrence), min(edges3$co_occurrence), min(edges4$co_occurrence))
      max_edge <- max(max(edges1$co_occurrence), max(edges2$co_occurrence), max(edges3$co_occurrence), max(edges4$co_occurrence))
      min_node <- min(min(nodes1$pagerank), min(nodes2$pagerank), min(nodes3$pagerank), min(nodes4$pagerank))
      max_node <- max(max(nodes1$pagerank), max(nodes2$pagerank), max(nodes3$pagerank), max(nodes4$pagerank))
    } else {
      edges3 <- fst_cn_edges(data = data3, concepts = concepts, norm = norm, threshold = threshold, pos_filter = pos_filter)
      edges2 <- fst_cn_edges(data = data2, concepts = concepts, norm = norm, threshold = threshold, pos_filter = pos_filter)
      edges1 <- fst_cn_edges(data = data1, concepts = concepts, norm = norm, threshold = threshold, pos_filter = pos_filter)
      nodes3 <- fst_cn_nodes(data = data3, edges3, pos_filter = pos_filter)
      nodes2 <- fst_cn_nodes(data = data2, edges2, pos_filter = pos_filter)
      nodes1 <- fst_cn_nodes(data = data1, edges1, pos_filter = pos_filter)
      min_edge <- min(min(edges1$co_occurrence), min(edges2$co_occurrence), min(edges3$co_occurrence))
      max_edge <- max(max(edges1$co_occurrence), max(edges2$co_occurrence), max(edges3$co_occurrence))
      min_node <- min(min(nodes1$pagerank), min(nodes2$pagerank), min(nodes3$pagerank))
      max_node <- max(max(nodes1$pagerank), max(nodes2$pagerank), max(nodes3$pagerank))
    }
  } else {
    edges2 <- fst_cn_edges(data = data2, concepts = concepts, norm = norm, threshold = threshold, pos_filter = pos_filter)
    edges1 <- fst_cn_edges(data = data1, concepts = concepts, norm = norm, threshold = threshold, pos_filter = pos_filter)
    nodes2 <- fst_cn_nodes(data = data2, edges2, pos_filter = pos_filter)
    nodes1 <- fst_cn_nodes(data = data1, edges1, pos_filter = pos_filter)
    min_edge <- min(min(edges1$co_occurrence), min(edges2$co_occurrence))
    max_edge <- max(max(edges1$co_occurrence), max(edges2$co_occurrence))
    min_node <- min(min(nodes1$pagerank), min(nodes2$pagerank))
    max_node <- max(max(nodes1$pagerank), max(nodes2$pagerank))
  }
  num1 <- dplyr::n_distinct(data1$doc_id)
  num2 <- dplyr::n_distinct(data2$doc_id)
  if (!is.null(data3)) {
    num3 <- dplyr::n_distinct(data3$doc_id)
    if (!is.null(data4)) {
      num4 <- dplyr::n_distinct(data4$doc_id)
      message(paste0("Note: \n Consider whether your data is balanced between groups being compared and whether each group contains enough data for analysis. \n The number of responses in each group (including \'NAs\') are listed below: \n\t", name1, "=", num1, ", ", name2, "=", num2, ", ", name3, "=", num3, ", ", name4, "=", num4, "\n\n"))
      unique <- fst_cn_get_unique(nodes1, nodes2, nodes3, nodes4)
      plot4 <- fst_cn_compare_plot(edges4, nodes4, name = name4, concepts = concepts, unique_lemmas = unique, min_edge = min_edge, max_edge = max_edge, min_node = min_node, max_node = max_node)
      plot3 <- fst_cn_compare_plot(edges3, nodes3, name = name3, concepts = concepts, unique_lemmas = unique, min_edge = min_edge, max_edge = max_edge, min_node = min_node, max_node = max_node)
      plot2 <- fst_cn_compare_plot(edges2, nodes2, name = name2, concepts = concepts, unique_lemmas = unique, min_edge = min_edge, max_edge = max_edge, min_node = min_node, max_node = max_node)
      plot1 <- fst_cn_compare_plot(edges1, nodes1, name = name1, concepts = concepts, unique_lemmas = unique, min_edge = min_edge, max_edge = max_edge, min_node = min_node, max_node = max_node)
      plot <- ggpubr::ggarrange(plot1, plot2, plot3, plot4, ncol = 2, nrow = 2, common.legend = TRUE, legend = "right")
      ggpubr::annotate_figure(plot, top = ggpubr::text_grob("Comparison Plot of Concept Networks",
        face = "bold", size = 20
      ))
    } else {
      message(paste0("Note: \n Consider whether your data is balanced between groups being compared and whether each group contains enough data for analysis. \n The number of responses in each group (including \'NAs\') are listed below: \n\t", name1, "=", num1, ", ", name2, "=", num2, ", ", name3, "=", num3, "\n\n"))
      unique <- fst_cn_get_unique(nodes1, nodes2, nodes3)
      plot3 <- fst_cn_compare_plot(edges3, nodes3, name = name3, concepts = concepts, unique_lemmas = unique, min_edge = min_edge, max_edge = max_edge, min_node = min_node, max_node = max_node)
      plot2 <- fst_cn_compare_plot(edges2, nodes2, name = name2, concepts = concepts, unique_lemmas = unique, min_edge = min_edge, max_edge = max_edge, min_node = min_node, max_node = max_node)
      plot1 <- fst_cn_compare_plot(edges1, nodes1, name = name1, concepts = concepts, unique_lemmas = unique, min_edge = min_edge, max_edge = max_edge, min_node = min_node, max_node = max_node)
      plot <- ggpubr::ggarrange(plot1, plot2, plot3, ncol = 3, nrow = 1, common.legend = TRUE, legend = "right")
      ggpubr::annotate_figure(plot, top = ggpubr::text_grob("Comparison Plot of Concept Networks",
        face = "bold", size = 20
      ))
    }
  } else {
    message(paste0("Note: \n Consider whether your data is balanced between groups being compared and whether each group contains enough data for analysis. \n The number of responses in each group (including \'NAs\') are listed below: \n\t", name1, "=", num1, ", ", name2, "=", num2, "\n\n"))
    unique <- fst_cn_get_unique(nodes1, nodes2)
    plot2 <- fst_cn_compare_plot(edges2, nodes2, name = name2, concepts = concepts, unique_lemmas = unique, min_edge = min_edge, max_edge = max_edge, min_node = min_node, max_node = max_node)
    plot1 <- fst_cn_compare_plot(edges1, nodes1, name = name1, concepts = concepts, unique_lemmas = unique, min_edge = min_edge, max_edge = max_edge, min_node = min_node, max_node = max_node)
    plot <- ggpubr::ggarrange(plot1, plot2, ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
    ggpubr::annotate_figure(plot, top = ggpubr::text_grob("Comparison Plot of Concept Networks",
      face = "bold", size = 20
    ))
  }
}
