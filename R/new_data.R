# Getting new data ready on work pc

#' Young People's Views on Development Cooperation 2012 response data
#'
#' This data contains background variables and the responses to q11_1 'Jatka
#' lausetta: Kehitysmaa on maa, jossa... (Avokysymys)', q11_2 'Jatka lausetta:
#' Kehitysyhteistyö on toimintaa, jossa... (Avokysymys)', q11_3' Jatka lausetta:
#' Maailman kolme suurinta ongelmaa ovat... (Avokysymys)' in the FSD2821 Nuorten
#' ajatuksia kehitysyhteistyöstä 2012 dataset.
#'
#' @format ## `dev_data`
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

dev_coop <- read.csv('//ad.helsinki.fi/home/a/adeclark/Documents/2024/surveyrep/data/daF2821.csv', sep = ';')
dev_coop <- select(dev_coop, 'fsd_id', 'q1', 'q2', 'q3', 'q6', 'q11_1', 'q11_2', 'q11_3', 'paino')
usethis::use_data(dev_coop)

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
#'   \item{bv1}{FSD case id}
#'   \item{bv2}{FSD case id}
#'   \item{bv9}{FSD case id}
#'   \item{q3}{'Which things are you good at?' response text}
#'   \item{q7}{'What do you think bullying is?' response text}
#'   \item{q11}{'What makes you happy?' response text}
#'   \item{paino}{Weight}
#' }
#' @source <https://urn.fi/urn:nbn:fi:fsd:T-FSD3134>
"child"

child <- read.csv('//ad.helsinki.fi/home/a/adeclark/Documents/2024/surveyrep/data/childbaro2016.csv', sep = ';')
child <- select(child, 'fsd_id', 'bv1', 'bv2', 'bv9', 'q3', 'q7', 'q11', 'paino')

usethis::use_data(child, overwrite = TRUE)
