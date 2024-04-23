# Looking into plotting arbitrarily many plots

#' Display comparison plots
#'
#' Display between 2 and 4 plots within the plots pane. If 2 or 3 plots, they
#' will be in a single row, if there are 4 plots, they will be in 2 rows of 2.
#'
#' @param plot1 First plot to display.
#' @param plot2 Second plot to display.
#' @param plot3 Optional third plot to display, defaul is `NULL`.
#' @param plot4 Optional fourth plot to display, defaul is `NULL`.
#' @param main_title An optional title for the set of plots. The default is
#'  `NULL` and no main title will be included.
#'
#' @return Up to 4 plots within the plots pane.
#' @export
#'
#' @examples
#' top_f <- fst_get_top_words(conllu_dev_q11_1_f_nltk)
#' top_m <- fst_get_top_words(conllu_dev_q11_1_m_nltk)
#' top_na <- fst_get_top_words(conllu_dev_q11_1_na_nltk)
#' topn_f <- fst_get_top_ngrams(conllu_dev_q11_1_f_nltk)
#' topn_m <- fst_get_top_ngrams(conllu_dev_q11_1_m_nltk)
#' topn_na <- fst_get_top_ngrams(conllu_dev_q11_1_na_nltk)
#' unique_words <- fst_get_unique_ngrams(top_f, top_m, top_na)
#' unique_ngrams <- fst_get_unique_ngrams(topn_f, topn_m, topn_na)
#' top_fu <- fst_join_unique(top_f, unique_words)
#' top_mu <- fst_join_unique(top_m, unique_words)
#' top_nau <- fst_join_unique(top_na, unique_words)
#' p1 <- fst_ngrams_compare_plot(top_fu, ngrams = 1, name = "Female")
#' p2 <- fst_ngrams_compare_plot(top_mu, ngrams = 1, name = "Male")
#' p3 <- fst_ngrams_compare_plot(top_nau, ngrams = 1, name = "Not Spec")
#' fst_plot_multiple(p1, p2, p3, main_title = "Comparison Plots")
#' fst_plot_multiple(p1, p1)
fst_plot_multiple <- function(plot1, plot2, plot3 = NULL, plot4 = NULL, main_title = NULL) {
  if (!is.null(plot3)) {
    if (!is.null(plot4)) {
      gridExtra::grid.arrange(plot1, plot2, plot3, plot4, top = ggpubr::text_grob(main_title, size = 15, face = "bold"))
    } else {
      gridExtra::grid.arrange(plot1, plot2, plot3, ncol = 3, top = ggpubr::text_grob(main_title, size = 15, face = "bold"))
    }
  } else {
    gridExtra::grid.arrange(plot1, plot2, ncol = 2, top = ggpubr::text_grob(main_title, size = 15, face = "bold"))
  }
}




# Ok, so grid arrange does it for us! Hooray.
gridExtra::grid.arrange(p1, p1, p2, p2, p3, p3, p3, top = ggpubr::text_grob('main_title', size = 15, face = "bold"))


# We just need to check the subtitles aren't hard coded in terms of size
## ANS: Doesn't look like they are on my end

# And we need to be able to send arbitrarily many plot names to the function
plots <- c('p1', 'p2', 'p2', 'p3')
#p1, p1, p2, p2, p3, p3, p3

gridExtra::grid.arrange(mget(plots), top = ggpubr::text_grob('main_title', size = 15, face = "bold"))


plist <- list(p1, p2, p3, p3)
do.call("grid.arrange", c(plist))
# but we can't get the title back in...
# > do.call("grid.arrange", c(plist, top = ggpubr::text_grob('main_title', size = 15, face = "bold")))
# Error in gList(...) : only 'grobs' allowed in "gList"


# DO we get rid of fst_plot_multiple.
# Then we would go from creating the plots, stra

