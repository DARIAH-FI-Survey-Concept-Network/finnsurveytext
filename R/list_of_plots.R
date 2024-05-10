fun <- function(table1, table2, ...) {
  my_list <- list(table1, table2, ...)
  my_list
}

listy <- fun(top_words, top_dev, top_child, top_dev, top_dev)


split <- function(data, field) {
  group_data <- data %>% dplyr::group_by_at(field)
  split_data <- dplyr::group_split(group_data)
  names <- dplyr:: group_keys(group_data)
  names(split_data) <- names[[field]]
  split_data
}

childy <- head(fst_child, 20)
childy2 <- split(childy, 'bv1')

# child_3 <- head(fst_child, 50)
#
# child4 <- child_3 %>% dplyr::group_by(bv1)
# child5 <- dplyr::group_split(child4)
# names <- dplyr::group_keys(child4)
# names(child5) <- names[[field]]


# do we want to allow more that one column? https://stackoverflow.com/questions/57275613/how-to-name-a-list-of-a-group-split-in-dplyr-when-grouped-by-more-than-one-colum

for (name in names(childy2)) {
  print(name)
}


for (i in 1:length(childy2)) {
  print(i)
}

list_of_top_words <- list()
i <- 2
data <- childy2[[i]]
top_words <- fst_freq_table(data)
list_of_top_words <- append(list_of_top_words, list(top_words))
names(list_of_top_words) <- names[['bv1']]

for (i in 1:length(childy2)) {
  data <- childy2[[i]]
  top_words <- fst_freq_table(data)
  list_of_top_words <- list.append(top_words)
}

# split_and_get_top_words <- function(data, field) {
#   #SPLIT
#   group_data <- data %>% dplyr::group_by_at(field)
#   split_data <- dplyr::group_split(group_data)
#   names <- dplyr:: group_keys(group_data)
#   names(split_data) <- names[[field]]
#   # GET TOP WORDS
#   list_of_top_words <- list()
#   for (i in 1:length(split_data)) {
#     data <- split_data[[i]]
#     top_words <- fst_ngrams_table2(data)
#     list_of_top_words <- append(list_of_top_words, list(top_words))
#   }
#   names(list_of_top_words) <- names[[field]]
#   # FIND UNIQUE
# }

childy3 <- split_and_get_top_words(childy, 'bv1')
childy4 <- split_and_get_top_words(fst_child, 'bv1')
childy4


fst_get_unique_ngrams_VERSION2 <- function(list_of_top_words) {
  df <- data.table::rbindlist(list_of_top_words) #this is the row that is changed
  df <- df %>%
    dplyr::mutate(n = 1) %>%
    dplyr::group_by(words) %>%
    dplyr::summarise(n = sum(n)) %>%
    dplyr::mutate(n = ifelse(n == 1, "yes", "no")) %>%
    dplyr::rename(unique_word = n)
  df
}

split_and_get_top_words <- function(data,
                                    field,
                                    exclude_nulls = FALSE,
                                    rename_nulls = 'null_data') {
  if (exclude_nulls == TRUE) {
    data <- data %>% tidyr::drop_na(field)
  } else {
    data[is.na(data)] <- rename_nulls
  }
  #SPLIT
  group_data <- data %>% dplyr::group_by_at(field)
  split_data <- dplyr::group_split(group_data)
  names <- dplyr:: group_keys(group_data)
  names(split_data) <- names[[field]]
  # GET TOP WORDS
  list_of_top_words <- list()
  for (i in 1:length(split_data)) {
    data <- split_data[[i]]
    top_words <- fst_ngrams_table2(data)
    list_of_top_words <- append(list_of_top_words, list(top_words))
  }
  names(list_of_top_words) <- names[[field]]
  # FIND UNIQUE
  unique <- fst_get_unique_ngrams_VERSION2(list_of_top_words)
  list_of_top_words_tables <- list()
  for (i in 1:length(list_of_top_words)) {
    table <- list_of_top_words[[i]]
    table <- fst_join_unique(table, unique)
    list_of_top_words_tables <- append(list_of_top_words_tables, list(table))
  }
  names(list_of_top_words_tables) <- names[[field]]
  list_of_top_words_tables
}

abcd <- split_and_get_top_words(fst_child, 'bv1')
efgh <- split_and_get_top_words(fst_dev_coop, 'q1')
efgh

efgh <- split_and_get_top_words(fst_dev_coop, 'q1', exclude_nulls = TRUE)
efgh

efgh <- split_and_get_top_words(fst_dev_coop, 'q1', exclude_nulls = FALSE, rename_nulls = 'OOPS')
efgh

efgh <- split_and_get_top_words(fst_dev_coop, 'q1', exclude_nulls = FALSE, rename_nulls = 0)
efgh

split_and_get_top_words_AND_PLOT <- function(data,
                                    field,
                                    exclude_nulls = FALSE,
                                    rename_nulls = 'null_data') {
  if (exclude_nulls == TRUE) {
    data <- data %>% tidyr::drop_na(field)
  } else {
    data[is.na(data)] <- rename_nulls
  }
  #SPLIT
  group_data <- data %>% dplyr::group_by_at(field)
  split_data <- dplyr::group_split(group_data)
  names <- dplyr:: group_keys(group_data)
  names(split_data) <- names[[field]]
  # GET TOP WORDS
  list_of_top_words <- list()
  for (i in 1:length(split_data)) {
    data <- split_data[[i]]
    top_words <- fst_ngrams_table2(data)
    list_of_top_words <- append(list_of_top_words, list(top_words))
  }
  names(list_of_top_words) <- names[[field]]
  # FIND UNIQUE
  unique <- fst_get_unique_ngrams_VERSION2(list_of_top_words)
  list_of_top_words_tables <- list()
  for (i in 1:length(list_of_top_words)) {
    table <- list_of_top_words[[i]]
    table <- fst_join_unique(table, unique)
    list_of_top_words_tables <- append(list_of_top_words_tables, list(table))
  }
  names(list_of_top_words_tables) <- names[[field]]
  names2 <- names[[field]]
  list_of_plots <- list()
  for (i in 1:length(list_of_top_words_tables)) {
    table <- list_of_top_words_tables[[i]]
    plot <- fst_ngrams_compare_plot(table) #, name = c(field, ' = ', names2[i])) # FIX THIS, want it to say "BV1 = 1 Most Common Words"
    list_of_plots <- append(list_of_plots, plot)
  }
  list_of_plots
  # gridExtra::grid.arrange(grobs = list_of_plots, ncol = 2)
}

xyz <- split_and_get_top_words_AND_PLOT(fst_child, 'bv1')
xyz
gridExtra::grid.arrange(grobs = xyz, ncol = 2)


list_of_top_words_tables <- efgh
i <- 1
table <- list_of_top_words_tables[[i]]
plot <- fst_ngrams_compare_plot(table) # FIX THIS, want it to say "BV1 = 1 Most Common Words"
plot
list_of_plots <- list()
list_of_plots <- append(list_of_plots, plot)
list_of_plots
