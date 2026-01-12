#' @title xc_index - Category-adjusted Expertise (x-) Index
#'
#' @description
#' Calculate the xc-index for an institution using bibliometric data from an
#' edge list, with an optional plot visualisation. The function is suitable
#' for including inside loops when plotting parameter is set to "FALSE" or "F".
#'
#' @param df Data frame object containing bibliometric data. This data frame
#'  must have at least three columns: one for keywords, one for unique IDs,
#'  and one for citation counts. Each row in the data frame should represent
#'  a document or publication.
#' @param kw Character string specifying the name of the column in "df" that
#'  contains keywords. Each cell in this column may contain no keywords (missing),
#'  a single keyword or multiple keywords separated by a specified delimiter.
#' @param cat Character string specifying the name of the column in "df" that
#'  contains categories. Each cell in this column may contain no categories
#'  (missing), a single category or multiple categories separated by a specified
#'  delimiter.
#' @param id Character string specifying the name of the column in "df" that
#'  contains unique identifiers for each document. Each cell in this column must
#'  contain a single ID (unless missing) and not multiple IDs.
#' @param cit Character string specifying the name of the column in "df" that
#'  contains the number of citations each document has received. Citations must
#'  be represented as integers. Each cell in this column should contain a single
#'  integer value (unless missing) representing the citation count for the
#'  corresponding document.
#' @param type "h" (default) for Hirsch's h-type index or "g" for Egghe's g-type index.
#'  Default set to "h".
#' @param dlm Character vector specifying the delimiter used in the "kw" and
#'  "cat" columns to separate multiple keywords and categories within a single cell.
#'  The delimiter should be consistent across the entirety of the two columns. The
#'  first element in the vector is used as the delimiter for keywords, while the second element
#'  is used for categories. Common delimiters include ";", "/", ":", and ",", and example usage
#'  should be similar to 'c(";", ";")' (default), 'c(";", ",")', or 'c(",", ":")'.
#' @param plot Logical value indicating whether to generate and display a plot
#' of the xc-index calculation. Set to "TRUE" or "T" to generate the plot, and
#' "FALSE" (default) or "F" to skip plot generation.
#'
#' @return xc-index magnitude, core category specific keywords and optional plot.
#'
#' @examples
#' # Load example data
#' data(WoSdata)
#'
#' # Calculate xc-index with plot
#' xc_index(df = WoSdata,
#'          id = "UT.Unique.WOS.ID",
#'          kw = "Keywords.Plus",
#'          cat = "WoS.Categories",
#'          cit = "Times.Cited.WoS.Core",
#'          plot = TRUE)
#'
#' @export
#'
#' @importFrom tidyr separate_rows
#' @importFrom dplyr %>% arrange desc filter mutate row_number select
#' @importFrom Matrix colSums sparseMatrix
#' @importFrom agop index.h index.g
#' @importFrom stats na.omit
#' @importFrom ggplot2 aes element_text geom_segment geom_point ggplot theme ggtitle xlab ylab

xc_index <- function(df,
                     kw,
                     cat,
                     id,
                     cit,
                     type = "h",
                     dlm = c(";", ";"),
                     plot = FALSE) {

  # Load required libraries
  if (!requireNamespace("Matrix", quietly = TRUE)) {
    stop("Package 'Matrix' is required but not installed.")
  }
  if (!requireNamespace("agop", quietly = TRUE)) {
    stop("Package 'agop' is required but not installed.")
  }
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("Package 'tidyr' is required but not installed.")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required but not installed.")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required but not installed.")
  }

  # declare global variables
  kc <- NULL

  # Working data frame
  dat <- df %>%
    dplyr::select(kw = {{kw}},
                  cat = {{cat}},
                  id = {{id}},
                  cit = {{cit}}) %>%
    dplyr::mutate(kw = as.character(kw), cat = as.character(cat), id = as.character(id), cit = as.numeric(cit)) %>%
    stats::na.omit()

  # Clean dataset
  dat <- dat %>%
    tidyr::separate_rows(kw, sep = as.character(dlm[1])) %>%
    tidyr::separate_rows(cat, sep = as.character(dlm[2])) %>%
    dplyr::mutate(kw = trimws(kw), cat = trimws(cat)) %>%
    dplyr::filter(kw != "") %>%
    dplyr::mutate(kc = paste0(kw, " (", cat, ")")) %>%
    stats::na.omit()

  # Sum citations per category specific keyword
  dat <- dat %>%
    dplyr::mutate(kc = trimws(kc)) %>%
    dplyr::group_by(kc) %>%
    dplyr::summarise(total_cit = sum(cit), .groups = "drop")

  # Sum citations for each keyword
  col_sum_citation_matrix <- dat$total_cit
  names(col_sum_citation_matrix) <- dat$kc

  # Calculate xc-index
  if (type == "h") {
    xc_index <- agop::index.h(unname(col_sum_citation_matrix))
  } else if (type == "g") {
    xc_index <- agop::index.g(unname(col_sum_citation_matrix))
  }

  # get keywords whose citation counts meet the index threshold
  cit_sorted <- sort(col_sum_citation_matrix, decreasing = TRUE)
  core_keywords <- names(cit_sorted)[seq_len(xc_index)]

  if (plot) {
    # Prepare data for plotting
    df_plot <- data.frame(kc = names(col_sum_citation_matrix), cit = col_sum_citation_matrix) %>%
      dplyr::arrange(desc(cit)) %>%
      dplyr::mutate(kc = factor(kc, levels = kc))

    # Create and print ggplot for xc-index
    print(ggplot2::ggplot(df_plot) +
            ggplot2::geom_point(ggplot2::aes(x = kc, y = cit), shape = 16) +
            ggplot2::geom_segment(x = xc_index,
                                  xend = xc_index,
                                  y = -Inf,
                                  yend = Inf,
                                  color = "#ff0000",
                                  linetype = 2) +
            ggplot2::xlab("Categorical Keywords") +
            ggplot2::ylab("Total Citations") +
            ggplot2::ggtitle(label = "xc-index") +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5)))
  }

  # Return value
  return(list(xc.index = xc_index,
              xc.keywords = core_keywords))
}
