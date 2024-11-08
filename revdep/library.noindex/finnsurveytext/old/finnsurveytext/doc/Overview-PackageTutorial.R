## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(warning = FALSE, message = FALSE) 

## -----------------------------------------------------------------------------
library(finnsurveytext)
library(survey)

## ----echo = FALSE-------------------------------------------------------------
data(dev_coop)
knitr::kable(head(dev_coop, 5))

## ----eval = FALSE-------------------------------------------------------------
#  # FUNCTION DEFINITION
#  fst_prepare <- function(data,
#                          question,
#                          id,
#                          model = "ftb",
#                          stopword_list = "nltk",
#                          weights = NULL,
#                          add_cols = NULL,
#                          manual = FALSE,
#                          manual_list = "")

## -----------------------------------------------------------------------------
df <- fst_prepare(data = dev_coop,
                  question = 'q11_3', 
                  id = 'fsd_id', 
                  weights = 'paino',
                  add_cols = c('gender', 'region')
                  )

## ----echo = FALSE-------------------------------------------------------------
knitr::kable(head(df, 2))

## ----echo = FALSE-------------------------------------------------------------
svy_dev <- survey::svydesign(id = ~1, weights = ~paino, data =dev_coop)

## ----eval = FALSE-------------------------------------------------------------
#  # FUNCTION DEFINITION
#  fst_prepare_svydesign <- function(svydesign,
#                                    question,
#                                    id,
#                                    model = "ftb",
#                                    stopword_list = "nltk",
#                                    use_weights = TRUE,
#                                    add_cols = NULL,
#                                    manual = FALSE,
#                                    manual_list = "")

## -----------------------------------------------------------------------------
df2 <- fst_prepare_svydesign(svydesign = svy_dev,
                            question = 'q11_3', 
                            id = 'fsd_id', 
                            use_weights = TRUE,
                            add_cols = c('gender', 'region')
                            )

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(head(df2, 2))

## ----eval=FALSE---------------------------------------------------------------
#  # FUNCTION DEFINITIONS
#  fst_summarise <- function(data,
#                            desc = "All respondents")
#  
#  fst_pos <- function(data)
#  
#  fst_length_summary <- function(data,
#                                 desc = "All respondents",
#                                 incl_sentences = TRUE)

## -----------------------------------------------------------------------------
fst_summarise(df)
fst_pos(df)
fst_length_summary(df)

## ----eval = FALSE-------------------------------------------------------------
#  # FUNCTION DEFINITION
#  fst_wordcloud <- function(data,
#                            pos_filter = NULL,
#                            max = 100,
#                            use_svydesign_weights = FALSE,
#                            id = "",
#                            svydesign = NULL,
#                            use_column_weights = FALSE)

## -----------------------------------------------------------------------------
fst_wordcloud(df)


## -----------------------------------------------------------------------------
# We can only get weights from svydesign if they are NOT already in our formatted data. Hence we remove them for this demonstration!
df2$weight <- NULL
fst_wordcloud(df2, 
              pos_filter = c("NOUN", "VERB", "ADJ", "ADV"),
              max=100, 
              use_svydesign_weights = TRUE, 
              id = 'fsd_id', 
              svydesign = svy_dev)


## ----eval=FALSE---------------------------------------------------------------
#  # FUNCTION DEFINITIONS
#  fst_freq <- function(data,
#                       number = 10,
#                       norm = NULL,
#                       pos_filter = NULL,
#                       strict = TRUE,
#                       name = NULL,
#                       use_svydesign_weights = FALSE,
#                       id = "",
#                       svydesign = NULL,
#                       use_column_weights = FALSE)
#  
#  fst_ngrams <- function(data,
#                         number = 10,
#                         ngrams = 1,
#                         norm = NULL,
#                         pos_filter = NULL,
#                         strict = TRUE,
#                         name = NULL,
#                         use_svydesign_weights = FALSE,
#                         id = "",
#                         svydesign = NULL,
#                         use_column_weights = FALSE)

## -----------------------------------------------------------------------------
fst_freq(df)

fst_ngrams(df, 
           number = 9, 
           ngrams = 2, 
           strict = FALSE,
           use_column_weights = TRUE)

fst_freq(df,
         number = 5, 
         strict = FALSE,)

## -----------------------------------------------------------------------------
fst_freq_table(df, number = 15, strict = FALSE)

## ----eval=FALSE---------------------------------------------------------------
#  # FUNCTION DEFINITIONS
#  fst_concept_network <- function(data,
#                                  concepts,
#                                  threshold = NULL,
#                                  norm = NULL,
#                                  pos_filter = NULL,
#                                  title = NULL)

## -----------------------------------------------------------------------------
fst_concept_network(df, 
                    concepts = "köyhyys, nälänhätä, sota, ilmastonmuutos, puute", 
                    )

## ----eval=FALSE---------------------------------------------------------------
#  fst_pos_compare <- function(data,
#                              field,
#                              exclude_nulls = FALSE,
#                              rename_nulls = 'null_data')
#  
#  fst_summarise_compare <- function(data,
#                                    field,
#                                    exclude_nulls = FALSE,
#                                    rename_nulls = 'null_data')
#  
#  fst_length_compare <- function(data,
#                                 field,
#                                 incl_sentences = TRUE,
#                                 exclude_nulls = FALSE,
#                                 rename_nulls = 'null_data')
#  

## -----------------------------------------------------------------------------
knitr::kable(fst_pos_compare(df, 'region'))

knitr::kable(fst_summarise_compare(df, 'region'))

knitr::kable(fst_length_compare(df, 'region'))

## ----eval=FALSE---------------------------------------------------------------
#  # FUNCTION DEFINITIONS
#  fst_freq_compare <- function(data,
#                               field,
#                               number = 10,
#                               norm = NULL,
#                               pos_filter = NULL,
#                               strict = TRUE,
#                               use_svydesign_weights = FALSE,
#                               id = "",
#                               svydesign = NULL,
#                               use_column_weights = FALSE,
#                               exclude_nulls = FALSE,
#                               rename_nulls = 'null_data',
#                               unique_colour = "indianred",
#                               title_size = 20,
#                               subtitle_size = 15)
#  
#  
#  fst_ngrams_compare <- function(data,
#                                field,
#                                number = 10,
#                                ngrams = 1,
#                                norm = NULL,
#                                pos_filter = NULL,
#                                strict = TRUE,
#                                use_svydesign_weights = FALSE,
#                                id = "",
#                                svydesign = NULL,
#                                use_column_weights = FALSE,
#                                exclude_nulls = FALSE,
#                                rename_nulls = 'null_data',
#                                unique_colour = "indianred",
#                                title_size = 20,
#                                subtitle_size = 15)

## -----------------------------------------------------------------------------
fst_freq_compare(df, 
                 'gender', 
                 use_column_weights = TRUE,
                 exclude_nulls = TRUE)

fst_ngrams_compare(df, 
                   'gender', 
                   ngrams = 2, 
                   use_column_weights = TRUE, 
                   exclude_nulls = TRUE)

## ----eval=FALSE---------------------------------------------------------------
#  # FUNCTION DEFINITION
#  fst_comparison_cloud <- function(data,
#                                   field,
#                                   pos_filter = NULL,
#                                   norm = NULL,
#                                   max = 100,
#                                   use_svydesign_weights = FALSE,
#                                   id = "",
#                                   svydesign = NULL,
#                                   use_column_weights = FALSE,
#                                   exclude_nulls = FALSE,
#                                   rename_nulls = "null_data")

## ----out.width = '750px', dpi=200---------------------------------------------
fst_comparison_cloud(df, 'gender', max = 40, use_column_weights = TRUE, exclude_nulls = TRUE)

## ----eval=FALSE---------------------------------------------------------------
#  # FUNCTION DEFINITION
#  fst_concept_network_compare <- function(data,
#                                          concepts,
#                                          field,
#                                          norm = NULL,
#                                          threshold = NULL,
#                                          pos_filter = NULL,
#                                          exclude_nulls = FALSE,
#                                          rename_nulls = 'null_data',
#                                          title_size = 20,
#                                          subtitle_size = 15)

## -----------------------------------------------------------------------------
fst_concept_network_compare(df, 
                            concepts = "köyhyys, nälänhätä, sota, ilmastonmuutos, puute", 
                            'gender',
                            exclude_nulls = TRUE
                            )

## ----echo = FALSE-------------------------------------------------------------
unlink('finnish-ftb-ud-2.5-191206.udpipe')

