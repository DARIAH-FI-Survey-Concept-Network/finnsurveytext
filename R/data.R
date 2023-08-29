#' Bullying response data
#'
#' This data contains the responses to q7 "Kertoisitko, mitä sinun mielestäsi
#' kiusaaminen on? (Avokysymys)" in the FSD3134 Lapsibarometri 2016 dataset.
#'
#' @format ## `bullying_data`
#' A list of length 414:
#' \describe{
#'   \item{bullying_data}{response text}
#' }
#' @source <https://urn.fi/urn:nbn:fi:fsd:T-FSD3134>
"bullying_data"

#' Bullying response data in CONN-LU format
#'
#' This data contains the responses to q7 "Kertoisitko, mitä sinun mielestäsi
#' kiusaaminen on? (Avokysymys)" in the FSD3134 Lapsibarometri 2016 dataset
#' in CoNNL-U format.
#'
#' @format ## `connlu_bullying`
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
"connlu_bullying"

#' Bullying response data in CoNNL-U format with ISO stopwords remvoved
#'
#' This data contains the responses to q7 "Kertoisitko, mitä sinun mielestäsi
#' kiusaaminen on? (Avokysymys)" in the FSD3134 Lapsibarometri 2016 dataset
#' in CONN-LU format with ISO stopwords removed.
#'
#' @format ## `connlu_bullying_iso`
#' A dataframe with 1747 rows and 14 columns:
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
"connlu_bullying_iso"

#' Loneliness response data
#'
#' This data contains the responses to q9 "Miltä yksinäisyys tuntuu?
#' Yksinäisyyteen #' liittyy usein voimakkaita tunteita, jotka ovat erilaisia
#' eri #' elämäntilanteissa. Jos haluat, voit kertoa yksinäisyyskokemuksiasi
#' tässä. #' (Avokysymys)" in the FSD3360 Helsingin Sanomat Loneliness Survey
#' 2014.
#'
#' @format ## `lonely_data`
#' A list of length 27851:
#' \describe{
#'   \item{lonely_data}{response text}
#' }
#' @source <https://urn.fi/urn:nbn:fi:fsd:T-FSD3360>
"lonely_data"

#' Loneliness response data in CONN-LU format
#'
#' This data contains the responses to q9 "Miltä yksinäisyys tuntuu?
#' Yksinäisyyteen liittyy usein voimakkaita tunteita, jotka ovat erilaisia eri
#' elämäntilanteissa. Jos haluat, voit kertoa yksinäisyyskokemuksiasi tässä.
#' (Avokysymys)" in the FSD3360 Helsingin Sanomat Loneliness Survey
#' 2014 in CoNNL-U format.
#'
#' @format ## `connlu_lonely`
#' A dataframe with 128946 rows and 14 columns:
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
#' @source <https://urn.fi/urn:nbn:fi:fsd:T-FSD3360>
"connlu_lonely"

#' Loneliness response data in CONN-LU format with NLTK stopwords remvoved
#'
#' This data contains the responses to q9 "Miltä yksinäisyys tuntuu?
#' Yksinäisyyteen liittyy usein voimakkaita tunteita, jotka ovat erilaisia eri
#' elämäntilanteissa. Jos haluat, voit kertoa yksinäisyyskokemuksiasi tässä.
#' (Avokysymys)" in the FSD3360 Helsingin Sanomat Loneliness Survey
#' 2014 in CoNNL-U format with NLTK stopwords removed.
#'
#' @format ## `connlu_lonely_nltk`
#' A dataframe with 101833 rows and 14 columns:
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
#' @source <https://urn.fi/urn:nbn:fi:fsd:T-FSD3360>
"connlu_lonely_nltk"

