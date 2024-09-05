#' Concept Network- Get unique nodes from separate top n-grams tables
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
#' fst_cn_get_unique_separate(n1, n2)
fst_cn_get_unique_separate <- function(table1, table2, ...) {
  df <- rbind(table1, table2, ...)
  df <- df %>%
    dplyr::mutate(n = 1) %>%
    dplyr::group_by(lemma) %>%
    dplyr::summarise(n = sum(n))
  unique <- dplyr::filter(df, n == 1)
  unique <- unique$lemma
  unique
}

#' Concept Network- Get unique nodes from a list of top n-grams tables
#'
#' Takes at least two tables of nodes and pagerank (output of `fst_cn_nodes()`)
#' and finds nodes unique to one table.
#'
#' @param list A list of top nodes
#'
#' @return Dataframe of words and whether word is unique or not.
#' @export
#'
#' @examples
#' pos_filter <- 'NOUN, VERB, ADJ, ADV'
#' e1 <- fst_cn_edges(fst_child, "lyödä", pos_filter = pos_filter)
#' e2 <- fst_cn_edges(fst_child, "lyöminen", pos_filter = pos_filter)
#' n1 <- fst_cn_nodes(fst_child, e1)
#' n2 <- fst_cn_nodes(fst_child, e2)
#' list_of_nodes <- list()
#' list_of_nodes <- append(list_of_nodes, list(n1))
#' list_of_nodes <- append(list_of_nodes, list(n2))
#' fst_cn_get_unique(list_of_nodes)
fst_cn_get_unique <- function(list) {
  df <- data.table::rbindlist(list)
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
#' @param title_size size to display plot title
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
#' u <- fst_cn_get_unique_separate(n1, n2)
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
                                max_node = NULL,
                                title_size = 20) {
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
    ) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = title_size))
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
#' @param data A dataframe of text in CoNLL-U format with additional `field`
#'  column for splitting data.
#' @param concepts List of terms to search for, separated by commas.
#' @param field Column in `data` used for splitting groups
#' @param norm The method for normalising the data. Valid settings are
#'  `"number_words"` (the number of words in the responses, default),
#'  `"number_resp"` (the number of responses), or `NULL` (raw count returned).
#' @param threshold A minimum number of occurrences threshold for 'edge' between
#'  searched term and other word, default is `NULL`. Note, the threshold is
#'  applied before normalisation.
#' @param pos_filter List of UPOS tags for inclusion, default is `NULL` to
#' include all UPOS tags.
#' @param use_svydesign_field Option to get `field` for splitting the data from
#'  a svydesign object, default is `FALSE`
#' @param id ID column from raw data, required if `use_svydesign_weights = TRUE`
#'  and must match the `docid` in formatted `data`.
#' @param svydesign A svydesign object which contains the raw data and weights.
#' @param exclude_nulls Whether to include NULLs in `field` column, default is
#'  `FALSE`
#' @param rename_nulls What to fill NULL values with if `exclude_nulls = FALSE`.
#' @param title_size size to display plot title
#' @param subtitle_size size to display title of individual concept network
#'
#' @return Multiple concept network plots with concept and unique words
#' highlighted.
#' @export
#'
#' @examples
#' con1 <- "lyödä, lyöminen"
#' fst_concept_network_compare(fst_child, concepts = con1, field = 'gender')
#' s <- survey::svydesign(id=~1, weights= ~paino, data = child)
#' c2 <- fst_child_2
#' i <- 'fsd_id'
#' fst_concept_network_compare(c2, con1, 'gender', NULL, NULL, NULL, TRUE, i, s)
#' con2 <- "köyhyys, nälänhätä, sota"
#' fst_concept_network_compare(fst_dev_coop, con2, 'gender')
fst_concept_network_compare <- function(data,
                                        concepts,
                                        field,
                                        norm = NULL,
                                        threshold = NULL,
                                        pos_filter = NULL,
                                        use_svydesign_field = FALSE,
                                        id = "",
                                        svydesign = NULL,
                                        exclude_nulls = FALSE,
                                        rename_nulls = 'null_data',
                                        title_size = 20,
                                        subtitle_size = 15){
  if (use_svydesign_field == TRUE) {
    data <- fst_use_svydesign(data,
                              svydesign = svydesign,
                              id = id,
                              add_cols = field,
                              add_weights = FALSE
    )
  }
  if (exclude_nulls == TRUE) {
    data <- data %>% tidyr::drop_na(field)
  } else {
    data[is.na(data)] <- rename_nulls
  }
  group_data <- data %>% dplyr::group_by_at(field)
  split_data <- dplyr::group_split(group_data)
  names <- dplyr:: group_keys(group_data)
  names(split_data) <- names[[field]]
  names2 <- names[[field]]
  list_of_edges <- list()
  list_of_nodes <- list()
  list_of_plots <- list()
  min_edge <- NULL
  max_edge <- NULL
  min_node <- NULL
  max_node <- NULL
  for (i in 1:length(split_data)) {
    data <- split_data[[i]]
    edges <- fst_cn_edges(data = data, concepts = concepts, norm = norm, threshold = threshold, pos_filter = pos_filter)
    nodes <- fst_cn_nodes(data = data, edges, pos_filter = pos_filter)
    list_of_edges <- append(list_of_edges, list(edges))
    list_of_nodes <- append(list_of_nodes, list(nodes))
    if (is.null(min_edge)) {
      min_edge <- min(edges$co_occurrence)
    } else {
      min_edge <- min(min_edge, min(edges$co_occurrence))
    }
    if (is.null(max_edge)) {
      max_edge <- max(edges$co_occurrence)
    } else {
      max_edge <- max(max_edge, max(edges$co_occurrence))
    }
    if (is.null(min_node)) {
      min_node <- min(nodes$pagerank)
    } else {
      min_node <- min(min_node, min(nodes$pagerank))
    }
    if (is.null(max_node)) {
      max_node <- max(nodes$pagerank)
    } else {
      max_node <- max(max_node, max(nodes$pagerank))
    }
  }
  unique <- fst_cn_get_unique(list_of_nodes)
  for (i in 1:length(list_of_edges)) {
    edges <- list_of_edges[[i]]
    nodes <- list_of_nodes[[i]]
    list_of_plots[[i]] <- fst_cn_compare_plot(edges, nodes, name = paste(field, '=', names2[i]), concepts = concepts, unique_lemmas = unique, min_edge = min_edge, max_edge = max_edge, min_node = min_node, max_node = max_node, title_size = subtitle_size)
  }
  plot <- ggpubr::ggarrange(plotlist = list_of_plots, common.legend = TRUE, legend = "right")
  ggpubr::annotate_figure(plot, top = ggpubr::text_grob("Comparison Plot of Concept Networks",
                                                        face = "bold", size = title_size
  ))
}
