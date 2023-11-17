#' Child Barometer 2016 response data
#'
#' This data contains the responses to q7 "Kertoisitko, mitä sinun mielestäsi
#' kiusaaminen on? (Avokysymys)" in the FSD3134 Lapsibarometri 2016 dataset.
#'
#' @format ## `child_barometer_data`
#' A dataframe with 414 rows and 2 columns:
#' \describe{
#'   \item{fsd_id}{FSD case id}
#'   \item{q7}{response text}
#' }
#' @source <https://urn.fi/urn:nbn:fi:fsd:T-FSD3134>
"child_barometer_data"

#' Child Barometer 2016 Bullying response data in CoNLL-U format
#'
#' This data contains the responses to q7 "Kertoisitko, mitä sinun mielestäsi
#' kiusaaminen on? (Avokysymys)" in the FSD3134 Lapsibarometri 2016 dataset
#' in CoNLL-U format using `finnish-tdt` model from [udpipe] package.
#'
#' @format ## `conllu_cb_bullying`
#' A dataframe with 2722 rows and 14 columns:
#' \describe{
#'   \item{doc_id}{the identifier of the document}
#'   \item{paragraph_id}{the identifier of the paragraph}
#'   \item{sentence_id}{the identifier of the sentence}
#'   \item{sentence}{the text of the sentence for which this token is part of}
#'   \item{token_id}{Word index, integer starting at 1 for each new sentence; may be a range for multi-word tokens; may be a decimal number for empty nodes.}
#'   \item{token}{Word form or punctuation symbol.}
#'   \item{lemma}{Lemma or stem of word form.}
#'   \item{upos}{Universal part-of-speech tag.}
#'   \item{xpos}{Language-specific part-of-speech tag; underscore if not available.}
#'   \item{feats}{List of morphological features from the universal feature inventory or from a defined language-specific extension; underscore if not available.}
#'   \item{head_token_id}{Head of the current word, which is either a value of token_id or zero (0).}
#'   \item{dep_rel}{Universal dependency relation to the HEAD (root iff HEAD = 0) or a defined language-specific subtype of one.}
#'   \item{deps}{Enhanced dependency graph in the form of a list of head-deprel pairs.}
#'   \item{misc}{Any other annotation.}
#' }
#' @source <https://urn.fi/urn:nbn:fi:fsd:T-FSD3134>
"conllu_cb_bullying"

#' Child Barometer 2016 Bullying response data in CoNLL-U format with ISO
#' stopwords removed
#'
#' This data contains the responses to q7 "Kertoisitko, mitä sinun mielestäsi
#' kiusaaminen on? (Avokysymys)" in the FSD3134 Lapsibarometri 2016 dataset
#' in CoNLL-U format with ISO stopwords and punctuation removed.
#'
#' @format ## `conllu_cb_bullying_iso`
#' A dataframe with 1240 rows and 14 columns:
#' \describe{
#'   \item{doc_id}{the identifier of the document}
#'   \item{paragraph_id}{the identifier of the paragraph}
#'   \item{sentence_id}{the identifier of the sentence}
#'   \item{sentence}{the text of the sentence for which this token is part of}
#'   \item{token_id}{Word index, integer starting at 1 for each new sentence; may be a range for multi-word tokens; may be a decimal number for empty nodes.}
#'   \item{token}{Word form or punctuation symbol.}
#'   \item{lemma}{Lemma or stem of word form.}
#'   \item{upos}{Universal part-of-speech tag.}
#'   \item{xpos}{Language-specific part-of-speech tag; underscore if not available.}
#'   \item{feats}{List of morphological features from the universal feature inventory or from a defined language-specific extension; underscore if not available.}
#'   \item{head_token_id}{Head of the current word, which is either a value of token_id or zero (0).}
#'   \item{dep_rel}{Universal dependency relation to the HEAD (root iff HEAD = 0) or a defined language-specific subtype of one.}
#'   \item{deps}{Enhanced dependency graph in the form of a list of head-deprel pairs.}
#'   \item{misc}{Any other annotation.}
#' }
#' @source <https://urn.fi/urn:nbn:fi:fsd:T-FSD3134>
"conllu_cb_bullying_iso"

#' Young People's Views on Development Cooperation 2012 response data
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

#' Young People's Views on Development Cooperation 2012 q11_1 response data in
#' CoNLL-U format
#'
#' This data contains the responses to q11_1 'Jatka lausetta: Kehitysmaa on maa,
#' jossa... (Avokysymys)' in CoNLL-U format using `finnish-ftb` model from
#' [udpipe] package.
#'
#' @format ## `conllu_dev_q11_1`
#' A dataframe with 6782 rows and 14 columns:
#' \describe{
#'   \item{doc_id}{the identifier of the document}
#'   \item{paragraph_id}{the identifier of the paragraph}
#'   \item{sentence_id}{the identifier of the sentence}
#'   \item{sentence}{the text of the sentence for which this token is part of}
#'   \item{token_id}{Word index, integer starting at 1 for each new sentence; may be a range for multi-word tokens; may be a decimal number for empty nodes.}
#'   \item{token}{Word form or punctuation symbol.}
#'   \item{lemma}{Lemma or stem of word form.}
#'   \item{upos}{Universal part-of-speech tag.}
#'   \item{xpos}{Language-specific part-of-speech tag; underscore if not available.}
#'   \item{feats}{List of morphological features from the universal feature inventory or from a defined language-specific extension; underscore if not available.}
#'   \item{head_token_id}{Head of the current word, which is either a value of token_id or zero (0).}
#'   \item{dep_rel}{Universal dependency relation to the HEAD (root iff HEAD = 0) or a defined language-specific subtype of one.}
#'   \item{deps}{Enhanced dependency graph in the form of a list of head-deprel pairs.}
#'   \item{misc}{Any other annotation.}
#' }
#' @source <https://urn.fi/urn:nbn:fi:fsd:T-FSD2821>
"conllu_dev_q11_1"

#' Young People's Views on Development Cooperation 2012 q11_2 response data in
#' CoNLL-U format
#'
#' This data contains the responses to q11_2 'Jatka lausetta: Kehitysyhteistyö
#' on toimintaa, jossa... (Avokysymys)' in CoNLL-U format using `finnish-ftb`
#' model from [udpipe] package.
#'
#' @format ## `conllu_dev_q11_2`
#' A dataframe with 5495 rows and 14 columns:
#' \describe{
#'   \item{doc_id}{the identifier of the document}
#'   \item{paragraph_id}{the identifier of the paragraph}
#'   \item{sentence_id}{the identifier of the sentence}
#'   \item{sentence}{the text of the sentence for which this token is part of}
#'   \item{token_id}{Word index, integer starting at 1 for each new sentence; may be a range for multi-word tokens; may be a decimal number for empty nodes.}
#'   \item{token}{Word form or punctuation symbol.}
#'   \item{lemma}{Lemma or stem of word form.}
#'   \item{upos}{Universal part-of-speech tag.}
#'   \item{xpos}{Language-specific part-of-speech tag; underscore if not available.}
#'   \item{feats}{List of morphological features from the universal feature inventory or from a defined language-specific extension; underscore if not available.}
#'   \item{head_token_id}{Head of the current word, which is either a value of token_id or zero (0).}
#'   \item{dep_rel}{Universal dependency relation to the HEAD (root iff HEAD = 0) or a defined language-specific subtype of one.}
#'   \item{deps}{Enhanced dependency graph in the form of a list of head-deprel pairs.}
#'   \item{misc}{Any other annotation.}
#' }
#' @source <https://urn.fi/urn:nbn:fi:fsd:T-FSD2821>
"conllu_dev_q11_2"

#' Young People's Views on Development Cooperation 2012 q11_3 response data in
#' CoNLL-U format
#'
#' This data contains the responses to , q11_3' Jatka lausetta: Maailman kolme
#' suurinta ongelmaa ovat... (Avokysymys)' in CoNLL-U format using `finnish-ftb`
#' model from [udpipe] package.
#'
#' @format ## `conllu_dev_q11_3`
#' A dataframe with 6610 rows and 14 columns:
#' \describe{
#'   \item{doc_id}{the identifier of the document}
#'   \item{paragraph_id}{the identifier of the paragraph}
#'   \item{sentence_id}{the identifier of the sentence}
#'   \item{sentence}{the text of the sentence for which this token is part of}
#'   \item{token_id}{Word index, integer starting at 1 for each new sentence; may be a range for multi-word tokens; may be a decimal number for empty nodes.}
#'   \item{token}{Word form or punctuation symbol.}
#'   \item{lemma}{Lemma or stem of word form.}
#'   \item{upos}{Universal part-of-speech tag.}
#'   \item{xpos}{Language-specific part-of-speech tag; underscore if not available.}
#'   \item{feats}{List of morphological features from the universal feature inventory or from a defined language-specific extension; underscore if not available.}
#'   \item{head_token_id}{Head of the current word, which is either a value of token_id or zero (0).}
#'   \item{dep_rel}{Universal dependency relation to the HEAD (root iff HEAD = 0) or a defined language-specific subtype of one.}
#'   \item{deps}{Enhanced dependency graph in the form of a list of head-deprel pairs.}
#'   \item{misc}{Any other annotation.}
#' }
#' @source <https://urn.fi/urn:nbn:fi:fsd:T-FSD2821>
"conllu_dev_q11_3"

#' Young People's Views on Development Cooperation 2012 q11_1 response data in
#' CoNLL-U format with NTLK stopwords removed
#'
#' This data contains the responses to Development Cooperation q11_1 dataset
#' in CoNLL-U format with ISO stopwords and punctuation removed.
#'
#' @format ## `conllu_dev_q11_1_nltk`
#' A dataframe with 4257 rows and 14 columns:
#' \describe{
#'   \item{doc_id}{the identifier of the document}
#'   \item{paragraph_id}{the identifier of the paragraph}
#'   \item{sentence_id}{the identifier of the sentence}
#'   \item{sentence}{the text of the sentence for which this token is part of}
#'   \item{token_id}{Word index, integer starting at 1 for each new sentence; may be a range for multi-word tokens; may be a decimal number for empty nodes.}
#'   \item{token}{Word form or punctuation symbol.}
#'   \item{lemma}{Lemma or stem of word form.}
#'   \item{upos}{Universal part-of-speech tag.}
#'   \item{xpos}{Language-specific part-of-speech tag; underscore if not available.}
#'   \item{feats}{List of morphological features from the universal feature inventory or from a defined language-specific extension; underscore if not available.}
#'   \item{head_token_id}{Head of the current word, which is either a value of token_id or zero (0).}
#'   \item{dep_rel}{Universal dependency relation to the HEAD (root iff HEAD = 0) or a defined language-specific subtype of one.}
#'   \item{deps}{Enhanced dependency graph in the form of a list of head-deprel pairs.}
#'   \item{misc}{Any other annotation.}
#' }
#' @source <https://urn.fi/urn:nbn:fi:fsd:T-FSD2821>
"conllu_dev_q11_1_nltk"

#' Young People's Views on Development Cooperation 2012 q11_2 response data in
#' CoNLL-U format with NTLK stopwords removed
#'
#' This data contains the responses to Development Cooperation q11_2 dataset
#' in CoNLL-U format with ISO stopwords and punctuation removed.
#'
#' @format ## `conllu_dev_q11_2_nltk`
#' A dataframe with 4407 rows and 14 columns:
#' \describe{
#'   \item{doc_id}{the identifier of the document}
#'   \item{paragraph_id}{the identifier of the paragraph}
#'   \item{sentence_id}{the identifier of the sentence}
#'   \item{sentence}{the text of the sentence for which this token is part of}
#'   \item{token_id}{Word index, integer starting at 1 for each new sentence; may be a range for multi-word tokens; may be a decimal number for empty nodes.}
#'   \item{token}{Word form or punctuation symbol.}
#'   \item{lemma}{Lemma or stem of word form.}
#'   \item{upos}{Universal part-of-speech tag.}
#'   \item{xpos}{Language-specific part-of-speech tag; underscore if not available.}
#'   \item{feats}{List of morphological features from the universal feature inventory or from a defined language-specific extension; underscore if not available.}
#'   \item{head_token_id}{Head of the current word, which is either a value of token_id or zero (0).}
#'   \item{dep_rel}{Universal dependency relation to the HEAD (root iff HEAD = 0) or a defined language-specific subtype of one.}
#'   \item{deps}{Enhanced dependency graph in the form of a list of head-deprel pairs.}
#'   \item{misc}{Any other annotation.}
#' }
#' @source <https://urn.fi/urn:nbn:fi:fsd:T-FSD2821>
"conllu_dev_q11_2_nltk"

#' Young People's Views on Development Cooperation 2012 q11_3 response data in
#' CoNLL-U format with NTLK stopwords removed
#'
#' This data contains the responses to Development Cooperation q11_3 dataset
#' in CoNLL-U format with ISO stopwords and punctuation removed.
#'
#' @format ## `conllu_dev_q11_3_nltk`
#' A dataframe with 4192 rows and 14 columns:
#' \describe{
#'   \item{doc_id}{the identifier of the document}
#'   \item{paragraph_id}{the identifier of the paragraph}
#'   \item{sentence_id}{the identifier of the sentence}
#'   \item{sentence}{the text of the sentence for which this token is part of}
#'   \item{token_id}{Word index, integer starting at 1 for each new sentence; may be a range for multi-word tokens; may be a decimal number for empty nodes.}
#'   \item{token}{Word form or punctuation symbol.}
#'   \item{lemma}{Lemma or stem of word form.}
#'   \item{upos}{Universal part-of-speech tag.}
#'   \item{xpos}{Language-specific part-of-speech tag; underscore if not available.}
#'   \item{feats}{List of morphological features from the universal feature inventory or from a defined language-specific extension; underscore if not available.}
#'   \item{head_token_id}{Head of the current word, which is either a value of token_id or zero (0).}
#'   \item{dep_rel}{Universal dependency relation to the HEAD (root iff HEAD = 0) or a defined language-specific subtype of one.}
#'   \item{deps}{Enhanced dependency graph in the form of a list of head-deprel pairs.}
#'   \item{misc}{Any other annotation.}
#' }
#' @source <https://urn.fi/urn:nbn:fi:fsd:T-FSD2821>
"conllu_dev_q11_3_nltk"

#' Young People's Views on Development Cooperation 2012 q11_1 response data in
#' CoNLL-U format with snowball stopwords removed
#'
#' This data contains the responses to Development Cooperation q11_1 dataset
#' in CoNLL-U format with ISO stopwords and punctuation removed.
#'
#' @format ## `conllu_dev_q11_1_snow`
#' A dataframe with 4259 rows and 14 columns:
#' \describe{
#'   \item{doc_id}{the identifier of the document}
#'   \item{paragraph_id}{the identifier of the paragraph}
#'   \item{sentence_id}{the identifier of the sentence}
#'   \item{sentence}{the text of the sentence for which this token is part of}
#'   \item{token_id}{Word index, integer starting at 1 for each new sentence; may be a range for multi-word tokens; may be a decimal number for empty nodes.}
#'   \item{token}{Word form or punctuation symbol.}
#'   \item{lemma}{Lemma or stem of word form.}
#'   \item{upos}{Universal part-of-speech tag.}
#'   \item{xpos}{Language-specific part-of-speech tag; underscore if not available.}
#'   \item{feats}{List of morphological features from the universal feature inventory or from a defined language-specific extension; underscore if not available.}
#'   \item{head_token_id}{Head of the current word, which is either a value of token_id or zero (0).}
#'   \item{dep_rel}{Universal dependency relation to the HEAD (root iff HEAD = 0) or a defined language-specific subtype of one.}
#'   \item{deps}{Enhanced dependency graph in the form of a list of head-deprel pairs.}
#'   \item{misc}{Any other annotation.}
#' }
#' @source <https://urn.fi/urn:nbn:fi:fsd:T-FSD2821>
"conllu_dev_q11_1_snow"

#' Young People's Views on Development Cooperation 2012 Female response data
#'
#' This data contains the female responses to q11_1 'Jatka lausetta: Kehitysmaa
#' on maa, jossa... (Avokysymys)', q11_2 'Jatka lausetta: Kehitysyhteistyö on
#' toimintaa, jossa... (Avokysymys)', q11_3' Jatka lausetta: Maailman kolme
#' suurinta ongelmaa ovat... (Avokysymys)' in the FSD2821 Nuorten ajatuksia
#' kehitysyhteistyöstä 2012 dataset.
#'
#' @format ## `dev_data_f`
#' A dataframe with 673 rows and 4 columns:
#' \describe{
#'   \item{fsd_id}{FSD case id}
#'   \item{q11_1}{response text for q11_1}
#'   \item{q11_2}{response text for q11_2}
#'   \item{q11_3}{response text for q11_3}
#' }
#' @source <https://urn.fi/urn:nbn:fi:fsd:T-FSD2821>
"dev_data_f"

#' Young People's Views on Development Cooperation 2012 Female q11_1 response
#' data in CoNLL-U format
#'
#' This data contains the female responses to q11_1 'Jatka lausetta: Kehitysmaa
#' on maa, jossa... (Avokysymys)' in CoNLL-U format using `finnish-ftb` model
#' from [udpipe] package.
#'
#' @format ## `conllu_dev_q11_1_f`
#' A dataframe with 5251 rows and 14 columns:
#' \describe{
#'   \item{doc_id}{the identifier of the document}
#'   \item{paragraph_id}{the identifier of the paragraph}
#'   \item{sentence_id}{the identifier of the sentence}
#'   \item{sentence}{the text of the sentence for which this token is part of}
#'   \item{token_id}{Word index, integer starting at 1 for each new sentence; may be a range for multi-word tokens; may be a decimal number for empty nodes.}
#'   \item{token}{Word form or punctuation symbol.}
#'   \item{lemma}{Lemma or stem of word form.}
#'   \item{upos}{Universal part-of-speech tag.}
#'   \item{xpos}{Language-specific part-of-speech tag; underscore if not available.}
#'   \item{feats}{List of morphological features from the universal feature inventory or from a defined language-specific extension; underscore if not available.}
#'   \item{head_token_id}{Head of the current word, which is either a value of token_id or zero (0).}
#'   \item{dep_rel}{Universal dependency relation to the HEAD (root iff HEAD = 0) or a defined language-specific subtype of one.}
#'   \item{deps}{Enhanced dependency graph in the form of a list of head-deprel pairs.}
#'   \item{misc}{Any other annotation.}
#' }
#' @source <https://urn.fi/urn:nbn:fi:fsd:T-FSD2821>
"conllu_dev_q11_1_f"

#' Young People's Views on Development Cooperation 2012 Female q11_1 response
#' data in CoNLL-U format with NTLK stopwords removed
#'
#' This data contains the female responses to Development Cooperation q11_1
#' dataset in CoNLL-U format with ISO stopwords and punctuation removed.
#'
#' @format ## `conllu_dev_q11_1_f_nltk`
#' A dataframe with 3268 rows and 14 columns:
#' \describe{
#'   \item{doc_id}{the identifier of the document}
#'   \item{paragraph_id}{the identifier of the paragraph}
#'   \item{sentence_id}{the identifier of the sentence}
#'   \item{sentence}{the text of the sentence for which this token is part of}
#'   \item{token_id}{Word index, integer starting at 1 for each new sentence; may be a range for multi-word tokens; may be a decimal number for empty nodes.}
#'   \item{token}{Word form or punctuation symbol.}
#'   \item{lemma}{Lemma or stem of word form.}
#'   \item{upos}{Universal part-of-speech tag.}
#'   \item{xpos}{Language-specific part-of-speech tag; underscore if not available.}
#'   \item{feats}{List of morphological features from the universal feature inventory or from a defined language-specific extension; underscore if not available.}
#'   \item{head_token_id}{Head of the current word, which is either a value of token_id or zero (0).}
#'   \item{dep_rel}{Universal dependency relation to the HEAD (root iff HEAD = 0) or a defined language-specific subtype of one.}
#'   \item{deps}{Enhanced dependency graph in the form of a list of head-deprel pairs.}
#'   \item{misc}{Any other annotation.}
#' }
#' @source <https://urn.fi/urn:nbn:fi:fsd:T-FSD2821>
"conllu_dev_q11_1_f_nltk"

#' Young People's Views on Development Cooperation 2012 Male response data
#'
#' This data contains the male responses to q11_1 'Jatka lausetta: Kehitysmaa on
#' maa, jossa... (Avokysymys)', q11_2 'Jatka lausetta: Kehitysyhteistyö on
#' toimintaa, jossa... (Avokysymys)', q11_3' Jatka lausetta: Maailman kolme
#' suurinta ongelmaa ovat... (Avokysymys)' in the FSD2821 Nuorten ajatuksia
#' kehitysyhteistyöstä 2012 dataset.
#'
#' @format ## `dev_data_m`
#' A dataframe with 183 rows and 4 columns:
#' \describe{
#'   \item{fsd_id}{FSD case id}
#'   \item{q11_1}{response text for q11_1}
#'   \item{q11_2}{response text for q11_2}
#'   \item{q11_3}{response text for q11_3}
#' }
#' @source <https://urn.fi/urn:nbn:fi:fsd:T-FSD2821>
"dev_data_m"

#' Young People's Views on Development Cooperation 2012 Male q11_1 response data
#' in CoNLL-U format
#'
#' This data contains the male responses to q11_1 'Jatka lausetta: Kehitysmaa on
#' maa, jossa... (Avokysymys)' in CoNLL-U format using `finnish-ftb` model from
#' [udpipe] package.
#'
#' @format ## `conllu_dev_q11_1_m`
#' A dataframe with 1006 rows and 14 columns:
#' \describe{
#'   \item{doc_id}{the identifier of the document}
#'   \item{paragraph_id}{the identifier of the paragraph}
#'   \item{sentence_id}{the identifier of the sentence}
#'   \item{sentence}{the text of the sentence for which this token is part of}
#'   \item{token_id}{Word index, integer starting at 1 for each new sentence; may be a range for multi-word tokens; may be a decimal number for empty nodes.}
#'   \item{token}{Word form or punctuation symbol.}
#'   \item{lemma}{Lemma or stem of word form.}
#'   \item{upos}{Universal part-of-speech tag.}
#'   \item{xpos}{Language-specific part-of-speech tag; underscore if not available.}
#'   \item{feats}{List of morphological features from the universal feature inventory or from a defined language-specific extension; underscore if not available.}
#'   \item{head_token_id}{Head of the current word, which is either a value of token_id or zero (0).}
#'   \item{dep_rel}{Universal dependency relation to the HEAD (root iff HEAD = 0) or a defined language-specific subtype of one.}
#'   \item{deps}{Enhanced dependency graph in the form of a list of head-deprel pairs.}
#'   \item{misc}{Any other annotation.}
#' }
#' @source <https://urn.fi/urn:nbn:fi:fsd:T-FSD2821>
"conllu_dev_q11_1_m"

#' Young People's Views on Development Cooperation 2012 Male q11_1 response
#' data in CoNLL-U format with NTLK stopwords removed
#'
#' This data contains the male responses to Development Cooperation q11_1
#' dataset in CoNLL-U format with ISO stopwords and punctuation removed.
#'
#' @format ## `conllu_dev_q11_1_m_nltk`
#' A dataframe with 651 rows and 14 columns:
#' \describe{
#'   \item{doc_id}{the identifier of the document}
#'   \item{paragraph_id}{the identifier of the paragraph}
#'   \item{sentence_id}{the identifier of the sentence}
#'   \item{sentence}{the text of the sentence for which this token is part of}
#'   \item{token_id}{Word index, integer starting at 1 for each new sentence; may be a range for multi-word tokens; may be a decimal number for empty nodes.}
#'   \item{token}{Word form or punctuation symbol.}
#'   \item{lemma}{Lemma or stem of word form.}
#'   \item{upos}{Universal part-of-speech tag.}
#'   \item{xpos}{Language-specific part-of-speech tag; underscore if not available.}
#'   \item{feats}{List of morphological features from the universal feature inventory or from a defined language-specific extension; underscore if not available.}
#'   \item{head_token_id}{Head of the current word, which is either a value of token_id or zero (0).}
#'   \item{dep_rel}{Universal dependency relation to the HEAD (root iff HEAD = 0) or a defined language-specific subtype of one.}
#'   \item{deps}{Enhanced dependency graph in the form of a list of head-deprel pairs.}
#'   \item{misc}{Any other annotation.}
#' }
#' @source <https://urn.fi/urn:nbn:fi:fsd:T-FSD2821>
"conllu_dev_q11_1_m_nltk"

#' Young People's Views on Development Cooperation 2012 Gender Not Specified
#' response data
#'
#' This data contains the gender not specified responses to q11_1 'Jatka
#' lausetta: Kehitysmaa on maa, jossa... (Avokysymys)', q11_2 'Jatka lausetta:
#' Kehitysyhteistyö on toimintaa, jossa... (Avokysymys)', q11_3' Jatka lausetta:
#' Maailman kolme suurinta ongelmaa ovat... (Avokysymys)' in the FSD2821 Nuorten
#' ajatuksia kehitysyhteistyöstä 2012 dataset.
#'
#' @format ## `dev_data_na`
#' A dataframe with 89 rows and 4 columns:
#' \describe{
#'   \item{fsd_id}{FSD case id}
#'   \item{q11_1}{response text for q11_1}
#'   \item{q11_2}{response text for q11_2}
#'   \item{q11_3}{response text for q11_3}
#' }
#' @source <https://urn.fi/urn:nbn:fi:fsd:T-FSD2821>
"dev_data_na"

#' Young People's Views on Development Cooperation 2012 Gender Not Specified
#' q11_1 response data in CoNLL-U format
#'
#' This data contains the gender not specified responses to q11_1 'Jatka
#' lausetta: Kehitysmaa on maa, jossa... (Avokysymys)' in CoNLL-U format using
#' `finnish-ftb` model from [udpipe] package.
#'
#' @format ## `conllu_dev_q11_1_na`
#' A dataframe with 525 rows and 14 columns:
#' \describe{
#'   \item{doc_id}{the identifier of the document}
#'   \item{paragraph_id}{the identifier of the paragraph}
#'   \item{sentence_id}{the identifier of the sentence}
#'   \item{sentence}{the text of the sentence for which this token is part of}
#'   \item{token_id}{Word index, integer starting at 1 for each new sentence; may be a range for multi-word tokens; may be a decimal number for empty nodes.}
#'   \item{token}{Word form or punctuation symbol.}
#'   \item{lemma}{Lemma or stem of word form.}
#'   \item{upos}{Universal part-of-speech tag.}
#'   \item{xpos}{Language-specific part-of-speech tag; underscore if not available.}
#'   \item{feats}{List of morphological features from the universal feature inventory or from a defined language-specific extension; underscore if not available.}
#'   \item{head_token_id}{Head of the current word, which is either a value of token_id or zero (0).}
#'   \item{dep_rel}{Universal dependency relation to the HEAD (root iff HEAD = 0) or a defined language-specific subtype of one.}
#'   \item{deps}{Enhanced dependency graph in the form of a list of head-deprel pairs.}
#'   \item{misc}{Any other annotation.}
#' }
#' @source <https://urn.fi/urn:nbn:fi:fsd:T-FSD2821>
"conllu_dev_q11_1_na"

#' Young People's Views on Development Cooperation 2012 Gender Not Specified
#' q11_1 response data in CoNLL-U format with NTLK stopwords removed
#'
#' This data contains the gender not specified responses to Development
#' Cooperation q11_1 dataset in CoNLL-U format with ISO stopwords and
#' punctuation removed.
#'
#' @format ## `conllu_dev_q11_1_na_nltk`
#' A dataframe with 338 rows and 14 columns:
#' \describe{
#'   \item{doc_id}{the identifier of the document}
#'   \item{paragraph_id}{the identifier of the paragraph}
#'   \item{sentence_id}{the identifier of the sentence}
#'   \item{sentence}{the text of the sentence for which this token is part of}
#'   \item{token_id}{Word index, integer starting at 1 for each new sentence; may be a range for multi-word tokens; may be a decimal number for empty nodes.}
#'   \item{token}{Word form or punctuation symbol.}
#'   \item{lemma}{Lemma or stem of word form.}
#'   \item{upos}{Universal part-of-speech tag.}
#'   \item{xpos}{Language-specific part-of-speech tag; underscore if not available.}
#'   \item{feats}{List of morphological features from the universal feature inventory or from a defined language-specific extension; underscore if not available.}
#'   \item{head_token_id}{Head of the current word, which is either a value of token_id or zero (0).}
#'   \item{dep_rel}{Universal dependency relation to the HEAD (root iff HEAD = 0) or a defined language-specific subtype of one.}
#'   \item{deps}{Enhanced dependency graph in the form of a list of head-deprel pairs.}
#'   \item{misc}{Any other annotation.}
#' }
#' @source <https://urn.fi/urn:nbn:fi:fsd:T-FSD2821>
"conllu_dev_q11_1_na_nltk"
