#' Young People's Views on Development Cooperation 2012 response data
#'
#' This data contains background variables and the responses to q11_1 'Jatka
#' lausetta: Kehitysmaa on maa, jossa... (Avokysymys)', q11_2 'Jatka lausetta:
#' Kehitysyhteistyö on toimintaa, jossa... (Avokysymys)', q11_3' Jatka lausetta:
#' Maailman kolme suurinta ongelmaa ovat... (Avokysymys)' in the FSD2821 Nuorten
#' ajatuksia kehitysyhteistyöstä 2012 dataset.
#'
#' @format ## `dev_coop`
#' A dataframe with 925 rows and 9 columns:
#' \describe{
#'   \item{fsd_id}{FSD case id}
#'   \item{q1}{Gender: 1 (Male), 2 (Female)}
#'   \item{q2}{Year of Birth}
#'   \item{q3}{Region of Residence (NUTS2): 1 (Helsinki-Uusima), 2 (Muu Etelä-
#'    Suomi), 3 (Länsi-Suomi), 4 (Pohjois- ja Itä-Suomi)}
#'   \item{q6}{Education level: 1 (Primary education), 2 (Matriculation
#'    examination), 3 (Upper secondary vocational qualification)}
#'   \item{q11_1}{response text for q11_1}
#'   \item{q11_2}{response text for q11_2}
#'   \item{q11_3}{response text for q11_3}
#'   \item{paino}{Weight}
#' }
#' @source <https://urn.fi/urn:nbn:fi:fsd:T-FSD2821>
#"dev_coop"

#' Young People's Views on Development Cooperation 2012 q11_3 response data in
#' CoNLL-U format with NTLK stopwords removed and background variables.
#'
#' This data contains the responses to Development Cooperation q11_3 dataset
#' in CoNLL-U format with NLTK stopwords and punctuation removed plus weights
#' and background variables.
#'
#' @format ## `fst_dev_coop`
#' A dataframe with 4192 rows and 19 columns:
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
#'   \item{weight}{Weight}
#'   \item{q1}{Gender: 1 (Male), 2 (Female)}
#'   \item{q2}{Year of Birth}
#'   \item{q3}{Region of Residence (NUTS2): 1 (Helsinki-Uusima), 2 (Muu Etelä-
#'    Suomi), 3 (Länsi-Suomi), 4 (Pohjois- ja Itä-Suomi)}
#' }
#' @source <https://urn.fi/urn:nbn:fi:fsd:T-FSD2821>
"fst_dev_coop"

#' Young People's Views on Development Cooperation 2012 q11_3 response data in
#' CoNLL-U format with NTLK stopwords removed
#'
#' This data contains the responses to Development Cooperation q11_3 dataset
#' in CoNLL-U format with NLTK stopwords and punctuation removed.
#'
#' @format ## `fst_dev_coop_2`
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
"fst_dev_coop_2"

#' Child Barometer 2016 response data
#'
#' This data contains background variables and the responses to q3 "Missä
#' asioissa olet hyvä? (Avokysymys)", q7 "Kertoisitko, mitä sinun mielestäsi
#' kiusaaminen on? (Avokysymys)", and q11 "Mikä tekee sinut iloiseksi?
#' (Avokysymys)" in the FSD3134 Lapsibarometri 2016 dataset.
#'
#' @format ## `child_barometer_data`
#' A dataframe with 414 rows and 8 columns:
#' \describe{
#'   \item{fsd_id}{FSD case id}
#'   \item{bv1}{Gender: 1 (Boy), 2 (Girl)}
#'   \item{bv3}{Major region (NUTS2): 1 (Helsinki-Uusima), 2 (Etelä-Suomi), 3
#'    (Länsi-Suomi), 4 (Pohjois- ja Itä-Suomi)}
#'   \item{bv9}{Daycare before pre-school: 1 (Yes), 2 (No)}
#'   \item{q3}{'Which things are you good at?' response text}
#'   \item{q7}{'What do you think bullying is?' response text}
#'   \item{q11}{'What makes you happy?' response text}
#'   \item{paino}{Weight}
#' }
#' @source <https://urn.fi/urn:nbn:fi:fsd:T-FSD3134>
"child"

#' Child Barometer 2016 Bullying response data in CoNLL-U format with NLTK
#' stopwords removed and background variables
#'
#' This data contains the responses to q7 "Kertoisitko, mitä sinun mielestäsi
#' kiusaaminen on? (Avokysymys)" in the FSD3134 Lapsibarometri 2016 dataset
#' in CoNLL-U format with NLTK stopwords and punctuation removed plus weights
#' and background variables.
#'
#' @format ## `fst_child`
#' A dataframe with 1240 rows and 17 columns:
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
#'   \item{weight}{Weight}
#'   \item{bv1}{Gender: 1 (Boy), 2 (Girl)}
#'   \item{bv3}{Major region (NUTS2): 1 (Helsinki-Uusima), 2 (Etelä-Suomi), 3
#'    (Länsi-Suomi), 4 (Pohjois- ja Itä-Suomi)}
#' }
#' @source <https://urn.fi/urn:nbn:fi:fsd:T-FSD3134>
"fst_child"

#' Child Barometer 2016 Bullying response data in CoNLL-U format with NLTK
#' stopwords removed
#'
#' This data contains the responses to q7 "Kertoisitko, mitä sinun mielestäsi
#' kiusaaminen on? (Avokysymys)" in the FSD3134 Lapsibarometri 2016 dataset
#' in CoNLL-U format with NLTK stopwords and punctuation removed.
#'
#' @format ## `fst_child_2`
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
"fst_child_2"



