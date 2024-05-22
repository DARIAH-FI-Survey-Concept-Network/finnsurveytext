group_data <- data %>% dplyr::group_by_at(field)
split_data <- dplyr::group_split(group_data)
names <- dplyr:: group_keys(group_data)
names(split_data) <- names[[field]]


#' Compare response lengths
#'
#' Compare length of text responses for between 2 and 4 sets of prepared data.
#'
#' @param data1 A dataframe of text in CoNLL-U format for the first group.
#' @param data2 A dataframe of text in CoNLL-U format for the second group.
#' @param data3 An optional dataframe of text in CoNLL-U format for the third
#'  group, default is `NULL`.
#' @param data4 An optional dataframe of text in CoNLL-U format for the fourth
#'  group, default is `NULL`.
#' @param name1 A string describing data1, default is `"Group 1"`.
#' @param name2 A string describing data2, default is `"Group 2"`.
#' @param name3 A string describing data3, default is `"Group 3"`.
#' @param name4 A string describing data4, default is `"Group 4"`.
#' @param incl_sentences Whether to include sentence data in table, default is
#'  `TRUE`.
#'
#' @return Dataframe summarising response lengths.
#' @export
#'
#' @examples
#' fst_length_compare(fst_child, 'bv1')
#' fst_length_compare(fst_dev_coop, 'q1')
fst_length_compare <- function(data, field, incl_sentences = TRUE) {
  group_data <- data %>% dplyr::group_by_at(field)
  split_data <- dplyr::group_split(group_data)
  names <- dplyr:: group_keys(group_data)
  names(split_data) <- names[[field]]
  names <- names [[field]]
  list_of_length_sum <- list()
  for (i in 1:length(split_data)) {
    data <- split_data[[i]]
    length_sum <- fst_length_summary(data, desc = names[i])
    list_of_length_sum <- append(list_of_length_sum, list(length_sum))
  }
  df <- data.table::rbindlist(list_of_length_sum)
  df
}
