#' Bullying response data
#'
#' This data contains the responses to q7 "Kertoisitko, mitä sinun mielestäsi
#' kiusaaminen on? (Avokysymys)" in the FSD3134 Lapsibarometri 2016 dataset.
#'
#' @format ## `bullying_data`
#' A dataframe with 414 rows and 2 columns:
#' \describe{
#'   \item{fsd_id}{FSD case id}
#'   \item{q7}{response text}
#' }
#' @source <https://urn.fi/urn:nbn:fi:fsd:T-FSD3134>
"bullying_data"

#' Bullying response data in CoNLL-U format
#'
#' This data contains the responses to q7 "Kertoisitko, mitä sinun mielestäsi
#' kiusaaminen on? (Avokysymys)" in the FSD3134 Lapsibarometri 2016 dataset
#' in CoNLL-U format using `finnish-tdt` model from [udpipe] package.
#'
#' @format ## `conllu_bullying`
#' A dataframe with 2722 rows and 14 columns:
#' \describe{
#'   \item{doc_id} {the identifier of the document}
#'   \item{paragraph_id} {the identifier of the paragraph}
#'   \item{sentence_id} {the identifier of the sentence}
#'   \item{sentence} {the text of the sentence for which this token is part of}
#'   \item{token_id} {Word index, integer starting at 1 for each new sentence; may be a range for multi-word tokens; may be a decimal number for empty nodes.}
#'   \item{token} {Word form or punctuation symbol.}
#'   \item{lemma} {Lemma or stem of word form.}
#'   \item{upos} {Universal part-of-speech tag.}
#'   \item{xpos} {Language-specific part-of-speech tag; underscore if not available.}
#'   \item{feats} {List of morphological features from the universal feature inventory or from a defined language-specific extension; underscore if not available.}
#'   \item{head_token_id} {Head of the current word, which is either a value of token_id or zero (0).}
#'   \item{dep_rel} {Universal dependency relation to the HEAD (root iff HEAD = 0) or a defined language-specific subtype of one.}
#'   \item{deps} {Enhanced dependency graph in the form of a list of head-deprel pairs.}
#'   \item{misc} {Any other annotation.}
#' }
#' @source <https://urn.fi/urn:nbn:fi:fsd:T-FSD3134>
"conllu_bullying"

#' Bullying response data in CoNLL-U format with ISO stopwords removed
#'
#' This data contains the responses to q7 "Kertoisitko, mitä sinun mielestäsi
#' kiusaaminen on? (Avokysymys)" in the FSD3134 Lapsibarometri 2016 dataset
#' in CoNLL-U format with ISO stopwords and punctuation removed.
#'
#' @format ## `conllu_bullying_iso`
#' A dataframe with 1240 rows and 14 columns:
#' \describe{
#'   \item{doc_id} {the identifier of the document}
#'   \item{paragraph_id} {the identifier of the paragraph}
#'   \item{sentence_id} {the identifier of the sentence}
#'   \item{sentence} {the text of the sentence for which this token is part of}
#'   \item{token_id} {Word index, integer starting at 1 for each new sentence; may be a range for multi-word tokens; may be a decimal number for empty nodes.}
#'   \item{token} {Word form or punctuation symbol.}
#'   \item{lemma} {Lemma or stem of word form.}
#'   \item{upos} {Universal part-of-speech tag.}
#'   \item{xpos} {Language-specific part-of-speech tag; underscore if not available.}
#'   \item{feats} {List of morphological features from the universal feature inventory or from a defined language-specific extension; underscore if not available.}
#'   \item{head_token_id} {Head of the current word, which is either a value of token_id or zero (0).}
#'   \item{dep_rel} {Universal dependency relation to the HEAD (root iff HEAD = 0) or a defined language-specific subtype of one.}
#'   \item{deps} {Enhanced dependency graph in the form of a list of head-deprel pairs.}
#'   \item{misc} {Any other annotation.}
#' }
#' @source <https://urn.fi/urn:nbn:fi:fsd:T-FSD3134>
"conllu_bullying_iso"

#' Development Cooperation response data
#'
#' This data contains the responses to q11_1 'Jatka lausetta: Kehitysmaa on maa,
#' jossa... (Avokysymys)', q11_2 'Jatka lausetta: Kehitysyhteistyö on toimintaa,
#' jossa... (Avokysymys)', q11_3' Jatka lausetta: Maailman kolme suurinta
#' ongelmaa ovat... (Avokysymys)' in the FSD2821 Nuorten ajatuksia
#' kehitysyhteistyöstä 2012 dataset.
#'
#' @format ## `dev_data`
#' A dataframe with 925 rows and 4 columns:
#' \describe{
#'   \item{fsd_id}{FSD case id}
#'   \item{q11_1}{response text for q11_1}
#'   \item{q11_2}{response text for q11_2}
#'   \item{q11_3}{response text for q11_3}
#' }
#' @source <https://urn.fi/urn:nbn:fi:fsd:T-FSD2821>
"dev_data"

#' Development Cooperation q11_1 response data in CoNLL-U format
#'
#' This data contains the responses to q11_1 'Jatka lausetta: Kehitysmaa on maa,
#' jossa... (Avokysymys)' in CoNLL-U format using `finnish-ftb` model from
#' [udpipe] package.
#'
#' @format ## `conllu_dev_q11_1`
#' A dataframe with 6782 rows and 14 columns:
#' \describe{
#'   \item{doc_id} {the identifier of the document}
#'   \item{paragraph_id} {the identifier of the paragraph}
#'   \item{sentence_id} {the identifier of the sentence}
#'   \item{sentence} {the text of the sentence for which this token is part of}
#'   \item{token_id} {Word index, integer starting at 1 for each new sentence; may be a range for multi-word tokens; may be a decimal number for empty nodes.}
#'   \item{token} {Word form or punctuation symbol.}
#'   \item{lemma} {Lemma or stem of word form.}
#'   \item{upos} {Universal part-of-speech tag.}
#'   \item{xpos} {Language-specific part-of-speech tag; underscore if not available.}
#'   \item{feats} {List of morphological features from the universal feature inventory or from a defined language-specific extension; underscore if not available.}
#'   \item{head_token_id} {Head of the current word, which is either a value of token_id or zero (0).}
#'   \item{dep_rel} {Universal dependency relation to the HEAD (root iff HEAD = 0) or a defined language-specific subtype of one.}
#'   \item{deps} {Enhanced dependency graph in the form of a list of head-deprel pairs.}
#'   \item{misc} {Any other annotation.}
#' }
#' @source <https://urn.fi/urn:nbn:fi:fsd:T-FSD2821>
"conllu_dev_q11_1"

#' Development Cooperation q11_2 response data in CoNLL-U format
#'
#' This data contains the responses to q11_2 'Jatka lausetta: Kehitysyhteistyö
#' on toimintaa, jossa... (Avokysymys)' in CoNLL-U format using `finnish-ftb`
#' model from [udpipe] package.
#'
#' @format ## `conllu_dev_q11_2`
#' A dataframe with 5495 rows and 14 columns:
#' \describe{
#'   \item{doc_id} {the identifier of the document}
#'   \item{paragraph_id} {the identifier of the paragraph}
#'   \item{sentence_id} {the identifier of the sentence}
#'   \item{sentence} {the text of the sentence for which this token is part of}
#'   \item{token_id} {Word index, integer starting at 1 for each new sentence; may be a range for multi-word tokens; may be a decimal number for empty nodes.}
#'   \item{token} {Word form or punctuation symbol.}
#'   \item{lemma} {Lemma or stem of word form.}
#'   \item{upos} {Universal part-of-speech tag.}
#'   \item{xpos} {Language-specific part-of-speech tag; underscore if not available.}
#'   \item{feats} {List of morphological features from the universal feature inventory or from a defined language-specific extension; underscore if not available.}
#'   \item{head_token_id} {Head of the current word, which is either a value of token_id or zero (0).}
#'   \item{dep_rel} {Universal dependency relation to the HEAD (root iff HEAD = 0) or a defined language-specific subtype of one.}
#'   \item{deps} {Enhanced dependency graph in the form of a list of head-deprel pairs.}
#'   \item{misc} {Any other annotation.}
#' }
#' @source <https://urn.fi/urn:nbn:fi:fsd:T-FSD2821>
"conllu_dev_q11_2"

#' Development Cooperation q11_3 response data in CoNLL-U format
#'
#' This data contains the responses to , q11_3' Jatka lausetta: Maailman kolme
#' suurinta ongelmaa ovat... (Avokysymys)' in CoNLL-U format using `finnish-ftb`
#' model from [udpipe] package.
#'
#' @format ## `conllu_dev_q11_3`
#' A dataframe with 6610 rows and 14 columns:
#' \describe{
#'   \item{doc_id} {the identifier of the document}
#'   \item{paragraph_id} {the identifier of the paragraph}
#'   \item{sentence_id} {the identifier of the sentence}
#'   \item{sentence} {the text of the sentence for which this token is part of}
#'   \item{token_id} {Word index, integer starting at 1 for each new sentence; may be a range for multi-word tokens; may be a decimal number for empty nodes.}
#'   \item{token} {Word form or punctuation symbol.}
#'   \item{lemma} {Lemma or stem of word form.}
#'   \item{upos} {Universal part-of-speech tag.}
#'   \item{xpos} {Language-specific part-of-speech tag; underscore if not available.}
#'   \item{feats} {List of morphological features from the universal feature inventory or from a defined language-specific extension; underscore if not available.}
#'   \item{head_token_id} {Head of the current word, which is either a value of token_id or zero (0).}
#'   \item{dep_rel} {Universal dependency relation to the HEAD (root iff HEAD = 0) or a defined language-specific subtype of one.}
#'   \item{deps} {Enhanced dependency graph in the form of a list of head-deprel pairs.}
#'   \item{misc} {Any other annotation.}
#' }
#' @source <https://urn.fi/urn:nbn:fi:fsd:T-FSD2821>
"conllu_dev_q11_3"

#' Development Cooperation q11_1 response data in CoNLL-U format with NTLK stopwords removed
#'
#' This data contains the responses to Development Cooperation q11_1 dataset
#' in CoNLL-U format with ISO stopwords and punctuation removed.
#'
#' @format ## `conllu_dev_q11_1_nltk`
#' A dataframe with 4257 rows and 14 columns:
#' \describe{
#'   \item{doc_id} {the identifier of the document}
#'   \item{paragraph_id} {the identifier of the paragraph}
#'   \item{sentence_id} {the identifier of the sentence}
#'   \item{sentence} {the text of the sentence for which this token is part of}
#'   \item{token_id} {Word index, integer starting at 1 for each new sentence; may be a range for multi-word tokens; may be a decimal number for empty nodes.}
#'   \item{token} {Word form or punctuation symbol.}
#'   \item{lemma} {Lemma or stem of word form.}
#'   \item{upos} {Universal part-of-speech tag.}
#'   \item{xpos} {Language-specific part-of-speech tag; underscore if not available.}
#'   \item{feats} {List of morphological features from the universal feature inventory or from a defined language-specific extension; underscore if not available.}
#'   \item{head_token_id} {Head of the current word, which is either a value of token_id or zero (0).}
#'   \item{dep_rel} {Universal dependency relation to the HEAD (root iff HEAD = 0) or a defined language-specific subtype of one.}
#'   \item{deps} {Enhanced dependency graph in the form of a list of head-deprel pairs.}
#'   \item{misc} {Any other annotation.}
#' }
#' @source <https://urn.fi/urn:nbn:fi:fsd:T-FSD2821>
"conllu_dev_q11_1_nltk"

#' Development Cooperation q11_2 response data in CoNLL-U format with NTLK stopwords removed
#'
#' This data contains the responses to Development Cooperation q11_2 dataset
#' in CoNLL-U format with ISO stopwords and punctuation removed.
#'
#' @format ## `conllu_dev_q11_2_nltk`
#' A dataframe with 4407 rows and 14 columns:
#' \describe{
#'   \item{doc_id} {the identifier of the document}
#'   \item{paragraph_id} {the identifier of the paragraph}
#'   \item{sentence_id} {the identifier of the sentence}
#'   \item{sentence} {the text of the sentence for which this token is part of}
#'   \item{token_id} {Word index, integer starting at 1 for each new sentence; may be a range for multi-word tokens; may be a decimal number for empty nodes.}
#'   \item{token} {Word form or punctuation symbol.}
#'   \item{lemma} {Lemma or stem of word form.}
#'   \item{upos} {Universal part-of-speech tag.}
#'   \item{xpos} {Language-specific part-of-speech tag; underscore if not available.}
#'   \item{feats} {List of morphological features from the universal feature inventory or from a defined language-specific extension; underscore if not available.}
#'   \item{head_token_id} {Head of the current word, which is either a value of token_id or zero (0).}
#'   \item{dep_rel} {Universal dependency relation to the HEAD (root iff HEAD = 0) or a defined language-specific subtype of one.}
#'   \item{deps} {Enhanced dependency graph in the form of a list of head-deprel pairs.}
#'   \item{misc} {Any other annotation.}
#' }
#' @source <https://urn.fi/urn:nbn:fi:fsd:T-FSD2821>
"conllu_dev_q11_2_nltk"

#' Development Cooperation q11_3 response data in CoNLL-U format with NTLK stopwords removed
#'
#' This data contains the responses to Development Cooperation q11_3 dataset
#' in CoNLL-U format with ISO stopwords and punctuation removed.
#'
#' @format ## `conllu_dev_q11_3_nltk`
#' A dataframe with 4192 rows and 14 columns:
#' \describe{
#'   \item{doc_id} {the identifier of the document}
#'   \item{paragraph_id} {the identifier of the paragraph}
#'   \item{sentence_id} {the identifier of the sentence}
#'   \item{sentence} {the text of the sentence for which this token is part of}
#'   \item{token_id} {Word index, integer starting at 1 for each new sentence; may be a range for multi-word tokens; may be a decimal number for empty nodes.}
#'   \item{token} {Word form or punctuation symbol.}
#'   \item{lemma} {Lemma or stem of word form.}
#'   \item{upos} {Universal part-of-speech tag.}
#'   \item{xpos} {Language-specific part-of-speech tag; underscore if not available.}
#'   \item{feats} {List of morphological features from the universal feature inventory or from a defined language-specific extension; underscore if not available.}
#'   \item{head_token_id} {Head of the current word, which is either a value of token_id or zero (0).}
#'   \item{dep_rel} {Universal dependency relation to the HEAD (root iff HEAD = 0) or a defined language-specific subtype of one.}
#'   \item{deps} {Enhanced dependency graph in the form of a list of head-deprel pairs.}
#'   \item{misc} {Any other annotation.}
#' }
#' @source <https://urn.fi/urn:nbn:fi:fsd:T-FSD2821>
"conllu_dev_q11_3_nltk"

#' Development Cooperation q11_1 response data in CoNLL-U format with snowball stopwords removed
#'
#' This data contains the responses to Development Cooperation q11_1 dataset
#' in CoNLL-U format with ISO stopwords and punctuation removed.
#'
#' @format ## `conllu_dev_q11_1_snow`
#' A dataframe with 4259 rows and 14 columns:
#' \describe{
#'   \item{doc_id} {the identifier of the document}
#'   \item{paragraph_id} {the identifier of the paragraph}
#'   \item{sentence_id} {the identifier of the sentence}
#'   \item{sentence} {the text of the sentence for which this token is part of}
#'   \item{token_id} {Word index, integer starting at 1 for each new sentence; may be a range for multi-word tokens; may be a decimal number for empty nodes.}
#'   \item{token} {Word form or punctuation symbol.}
#'   \item{lemma} {Lemma or stem of word form.}
#'   \item{upos} {Universal part-of-speech tag.}
#'   \item{xpos} {Language-specific part-of-speech tag; underscore if not available.}
#'   \item{feats} {List of morphological features from the universal feature inventory or from a defined language-specific extension; underscore if not available.}
#'   \item{head_token_id} {Head of the current word, which is either a value of token_id or zero (0).}
#'   \item{dep_rel} {Universal dependency relation to the HEAD (root iff HEAD = 0) or a defined language-specific subtype of one.}
#'   \item{deps} {Enhanced dependency graph in the form of a list of head-deprel pairs.}
#'   \item{misc} {Any other annotation.}
#' }
#' @source <https://urn.fi/urn:nbn:fi:fsd:T-FSD2821>
"conllu_dev_q11_1_snow"
