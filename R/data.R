#' Energy-related greenhouse emission data from China Statistics Bureau
#'
#' GHG emission data from 2000-2015
#'
#' @format  data.frame variable containing the emission data from total emission, coal related source, oil related and gas related:
#' \describe{
#'     \item{year}{year, from 2000 to 2015}
#'     \item{Total}{total emission}
#'     \item{from Coal}{Coal related source emission}
#'     \item{from Oil}{Oil related source emission}
#'     \item{from Gas}{Gas related source emission}
#' }
#' @source  \url{http://www.stats.gov.cn/tjsj/ndsj/2018/indexch.htm}
#'
"ghg"

#' Energy-related greenhouse emission data from China Statistcs Bureau
#'
#' the adjusted data of GHG emission data from 2000-2015,checked after ghg dataset.
#'
#' @format  data.frame variable containing the emission data from total emission, coal related source, oil related and gas related:
#' \describe{
#'     \item{year}{year, from 2000 to 2015}
#'     \item{Total}{total emission}
#'     \item{from Coal}{Coal related source emission}
#'     \item{from Oil}{Oil related source emission}
#'     \item{from Gas}{Gas related source emission}
#' }
#' @source  \url{http://www.stats.gov.cn/tjsj/ndsj/2018/indexch.htm}
#'
"ghg2"

#'
#' A dataset containing the emissions from China
#'
#' @format  An atom vector from 2000-2014:
#' \describe{
#'     \item{year}{year, from 2000 to 2014}
#' }
#' @source  \url{http://www.diamonds.info/}
"y"

#'
#' Declining sequential test data
#' Test data generated from y by rev() function
#'
#' @format  An atom vector from 2000-2014:
#' \describe{
#'     \item{year}{year, from 2000 to 2014}
#' }
#' @source  \url{http://www.diamonds.info/}
"yr"

#' Quantity of annual vehicle ownership
#'
#' A dataset of Chinese vehicle owner
#'
#' @format An vector of 2006-2010
#' \describe{
#'     \item{p}{price, in dollars}
#' }
#' @source \url{https://data.stats.gov.cn/}
"owners"

#' Statistic data of annual accident death of China land traffic
#'
#' A dataset of Chinese vehicle owner
#'
#' @format An vector of 1990-2003
#' \describe{
#'     \item{ys}{death number, 10000 persons}
#' }
#' @source \url{https://data.stats.gov.cn/}
"ys"

#' Annual cost of Chinese food cold logistics demand
#'
#' A dataset of Chinese food cold logistics cost from 2014 to 2019, published by Cold Chain Logistics Commitee of CFLP
#'
#' @format An vector of 2014-2019
#' \describe{
#'     \item{logiscost}{total logistics, trillion yuan}
#' }
#' @source \url{China Cold Chain Logisitics Development Report(2020)}
"logiscost"

#' Annual quantity of Chinese food cold logistics demand
#'
#' A dataset of Chinese cold logistics cost from 2014 to 2019, published by Cold Chain Logistics Commitee of CFLP
#'
#' @format An vector of 2014-2019
#' \describe{
#'     \item{logisquantity}{total quantity of cold logisticsdemand, trillion yuan}
#' }
#' @source \url{China Cold Chain Logisitics Development Report(2020)}
"logisquantity"
