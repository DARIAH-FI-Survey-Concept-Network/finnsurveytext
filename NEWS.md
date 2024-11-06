# finnsurveytext 2.1.0

* New function `fst_print_available_models()` to print models available through `udpipe` package
* Bug fix in `fst_find_stopwords` - remove hardcoded language of `fi`.
* Bug fix in `fst_pos` to set NAs in count to 0
* New English sample data 

# finnsurveytext 2.0.0

## 01_prepare and 01b_prepare_svydesign

* `fst_format()` renamed from `fst_format_conllu()`.
* `fst_prepare()` renamed from `fst_prepare_conllu()`.
* In `fst_format()` and `fst_prepare()` new parameters weights = NULL and add_cols = NULL, and parameter field renamed question
* `fst_rm_stop_punct()` now enables user-input list of stopwords.
* `fst_format_svydesign()` and `fst_prepare_svydesign()` introduced. 

## 02_data_exploration

* `fst_freq_table()` renamed from `fst_get_top_words()` to reflect naming conventions.
* `fst_ngrams_table()` renamed from `fst_get_top_ngrams()` to reflect naming conventions.
* Within `fst_freq_table()`, `fst_ngrams_table()`, `fst_freq()`, `fst_ngrams()`, and `fst_wordcloud()` parameters to enable weights from svydesign object or from additional weights column in formatted data (use_svydesign_weights = FALSE, id = "", svydesign = NULL, use_column_weights = FALSE).
* pos_filter parameter now accepts strings (in addition to lists of strings). 

## 03_concept_network 

* (No major changes.)

## 04_comparison_functions

* Data is now split into groups from one dataset (data) via a parameter called ‘field’ which is the column in formatted data that splits into groups. Each different value in this column is used to create a group which is named after the value. 
* `fst_freq_compare()`, `fst_ngrams_compare()`, `fst_pos_compare()`, `fst_summarise_compare()`, `fst_length_compare()` and `fst_comparison_cloud()` now have parameters to enable inclusion/exclusion of null values when splitting data into groups (exclude_nulls = FALSE,
rename_nulls = 'null_data')
* Default for 'norm' parameter is now NULL to support use of weights. 
* `fst_comparison_cloud()` created. 
* `fst_freq_compare()`, `fst_ngrams_compare()` and `fst_comparison_cloud()` now have parameters to enable weights from svydesign object or from additional weights column in formatted data (use_svydesign_weights = FALSE, id = "", svydesign = NULL, use_column_weights = FALSE).
* `fst_ngrams_compare_plot()` has new parameter title_size = 20.
* `fst_freq_compare()`, and `fst_ngrams_compare()` have new parameters title_size = 20 and subtitle_size = 15. 

## 05_comparison_concept_network

* As above, data split by parameter 'field', parameters for inclusion/exclusion of null values in 'field', default norm = NULL, title_size and subtitle_size parameters. 
* `fst_cn_get_unique()` renamed `fst_cn_get_unique _separate()` and new function `fst_cn_get_unique()` created which finds unique ngrams in a list of ngrams tables. 

## Data

* New simplified sample data. 
* "child": Open-ended questions and background variables for Child Barometer survey. 
* "dev_coop*: Open-ended questions and background variables for Development Cooperation survey. 
* fst_child: Formatted data with b/g variables and weights.
* fst_child_2: Formatted data without b/g variables and weights.
* fst_dev_coop: Formatted data with b/g variables and weights.
* fst_dev_coop_2: Formatted data without b/g variables and weights.
* Removed data: child_barometer_data, conllu_cb_bullying, conllu_cb_bullying_iso, conllu_dev_q11_1_f_nltk, conllu_dev_q11_1_f, conllu_dev_q11_1_m_nltk, conllu_dev_q11_1_m, conllu_dev_q11_1_na_nltk, conllu_dev_q11_1_na, conllu_dev_q11_1_nltk, conllu_dev_q11_1, conllu_dev_q11_2_nltk, conllu_dev_q11_2, conllu_dev_q11_3_nltk, conllu_dev_q11_3, dev_data_f, dev_data_m, dev_data_na, dev_data.


# finnsurveytext 1.0.0

# finnsurveytext 0.1.0

* Added a `NEWS.md` file to track changes to the package.
