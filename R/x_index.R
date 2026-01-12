#' @title x_index - Expertise (x-) Index
#'
#' @description
#' Calculate x-index for an institution using bibliometric data from an edge
#' list, with an optional visualisation of ranked citation scores. The function
#' is suitable for including inside loops when plotting parameter is set to
#' "FALSE" or "F".
#'
#' @param df Data frame object containing bibliometric data. This data frame
#' must have at least three columns: one for keywords, one for unique IDs, and
#' one for citation counts. Each row in the data frame should represent a
#' document or publication.
#' @param kw Character string specifying the name of the column in "df" that
#' contains keywords. Each cell in this column may contain no keywords
#' (missing), a single keyword or multiple keywords separated by a specified
#' delimiter.
#' @param id Character string specifying the name of the column in "df" that
#' contains unique identifiers for each document. Each cell in this column must
#' contain a single ID (unless missing) and not multiple IDs.
#' @param cit Character string specifying the name of the column in "df" that
#' contains the number of citations each document has received. Citations must
#' be represented as integers. Each cell in this column should contain a single
#' integer value (unless missing) representing the citation count for the
#' corresponding document.
#' @param type "h" (default) for Hirsch's h-type index or "g" for Egghe's g-type index.
#' @param dlm Character string specifying the delimiter used in the "kw"
#' column to separate multiple keywords within a single cell. The delimiter
#' should be consistent across the entire "kw" column. Common delimiters
#' include ";" (default), "/", ":", and ",".
#' @param plot Logical value indicating whether to generate and display a
#' plot of the x-index calculation. Set to "TRUE" or "T" to generate the plot,
#' and "FALSE" (default) or "F" to skip plot generation.
#'
#' @return x-index magnitude, core keywords, and optional plot.
#'
#' @examples
#' # Load example data
#' data(WoSdata)
#'
#' # Calculate x-index with plot
#' x_index(df = WoSdata,
#'         id = "UT.Unique.WOS.ID",
#'         kw = "Keywords.Plus",
#'         cit = "Times.Cited.WoS.Core",
#'         plot = TRUE)
#'
#' @export x_index
#' @importFrom tidyr separate_rows
#' @importFrom dplyr %>% arrange desc filter mutate row_number select
#' @importFrom Matrix colSums sparseMatrix
#' @importFrom agop index.h index.g
#' @importFrom stats na.omit
#' @importFrom ggplot2 aes element_text geom_segment geom_point ggplot ggtitle theme xlab ylab

# Function to calculate x-index
x_index <- function(df,
                    kw,
                    id,
                    cit,
                    type ="h",
                    dlm = ";",
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
  if (!requireNamespace("stats", quietly = TRUE)) {
    stop("Package 'stats' is required but not installed.")
  }

  # Working data frame
  dat <- df %>%
    dplyr::select(kw = {{kw}}, id = {{id}}, cit = {{cit}}) %>%
    dplyr::mutate(kw = as.character(kw), id = as.character(id), cit = as.numeric(cit)) %>%
    stats::na.omit()

  # Clean dataset
  dat <- dat %>%
    tidyr::separate_rows(kw, sep = dlm) %>%
    dplyr::filter(kw != "") %>%
    stats::na.omit()

  # Sum citations per keyword
  dat <- dat %>%
    dplyr::mutate(kw = trimws(kw)) %>%
    dplyr::group_by(kw) %>%
    dplyr::summarise(total_cit = sum(cit), .groups = "drop")

  # Sum citations for each keyword
  col_sum_citation_matrix <- dat$total_cit
  names(col_sum_citation_matrix) <- dat$kw

  # Calculate x-index
  if (type == "h") {
    x_index <- agop::index.h(unname(col_sum_citation_matrix))
  }
  if (type == "g") {
    x_index <- agop::index.g(unname(col_sum_citation_matrix))
  }

  # get keywords whose citation counts meet the index threshold
  cit_sorted <- sort(col_sum_citation_matrix, decreasing = TRUE)
  core_keywords <- names(cit_sorted)[seq_len(x_index)]

  if (plot) {
    # Prepare data for plotting
    df_plot <- data.frame(kw = names(col_sum_citation_matrix), cit = col_sum_citation_matrix) %>%
      dplyr::arrange(dplyr::desc(cit)) %>%
      dplyr::mutate(kw = factor(kw, levels = kw))

    # Create and print ggplot for x-index
    print(ggplot2::ggplot(df_plot) +
            ggplot2::geom_point(ggplot2::aes(x = kw, y = cit), shape = 16) +
            ggplot2::geom_segment(x = x_index,
                                  xend = x_index,
                                  y = -Inf,
                                  yend = Inf,
                                  color = "#ff0000",
                                  linetype = 2) +
            ggplot2::xlab("Keywords") +
            ggplot2::ylab("Total Citations") +
            ggplot2::ggtitle(label = "x-index") +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5)))
  }

  # Return value
  return(list(x.index = x_index,
              x.keywords = core_keywords))
}
