#' WoSdata
#'
#' The list of publications and associated metadata for a scholarly institution
#' in India queried from the Web of Science (WoS) database. All publications, totalling
#' 2,355 distinct publications, within a time frame of 10 years, spanning from 2011
#' to 2020 were extracted. For these publications, additional information/metadata,
#' namely the 'UT (Unique WoS ID)', 'Keywords Plus', 'WoS Categories', and 'Times
#' Cited, WoS Core' fields, were also extracted.
#'
#' @format
#' A data frame with 2,355 rows and 4 columns. Each row represents a unique
#' publication:
#' \describe{
#'  \item{UT.Unique.WOS.ID}{Unique publication identifier.}
#'  \item{Keywords.Plus}{Indexed keywords separated by ';'s.}
#'  \item{WoS.Categories}{Indexed categories separated by ';'s.}
#'  \item{Times.Cited.WoS.Core}{Total citations as recorded in the WoS database.}
#' }
#'
#' @source
#' * [WoS](https://clarivate.com/academia-government/scientific-and-academic-research/research-discovery-and-referencing/web-of-science/)
"WoSdata"
