#' @title g_index - Egghe's g-index
#'
#' @description
#' Calculate g-index for an institution using bibliometric data from an edge list,
#' with an optional visualisation of ranked citation scores.
#'
#' @param df Data frame object containing bibliometric data. This data frame must
#'  have at least two columns: one for keywords and one for citation counts. An optional
#'  column for unique identifiers can be included. Each row in the data frame should
#'  represent a document or publication.
#' @param id Character string specifying the name of the column in "df" that contains
#'  unique identifiers for each document. Each cell in this column must contain a single
#'  ID (unless missing) and not multiple IDs. Must be included when 'plot' parameter is set
#'  to "TRUE". Default set to NULL.
#' @param cit Character string specifying the name of the column in "df" that contains
#'  the number of citations each document has received. Citations must be represented
#'  as integers. Each cell in this column should contain a single integer value
#'  (unless missing) representing the citation count for the corresponding document.
#' @param plot Logical value indicating whether to generate and display a plot of
#'  the g-index calculation. Set to "TRUE" or "T" to generate the plot, and "FALSE" (default)
#'  or "F" to skip plot generation.
#'
#' @return g-index magnitude and optional plot.
#'
#' @examples
#' # Load example data
#' data(WoSdata)
#'
#' # Calculate g-index with plot
#' g_index(df = WoSdata,
#'         id = "UT.Unique.WOS.ID",
#'         cit = "Times.Cited.WoS.Core",
#'         plot = TRUE)
#'
#' @export
#' @importFrom dplyr %>% arrange desc mutate row_number select
#' @importFrom agop index.g
#' @importFrom stats na.omit
#' @importFrom ggplot2 aes element_text geom_hline geom_point ggplot theme ggtitle xlab ylab

# Function to calculate g-index
g_index <- function(df,
                    id = NULL,
                    cit,
                    plot = FALSE) {

  # Load required libraries
  if (!requireNamespace("agop", quietly = TRUE)) {
    stop("Package 'agop' is required but not installed.")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required but not installed.")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required but not installed.")
  }
  if (!requireNamespace("stats", quietly = TRUE)) {
    stop("Package 'stats' is required but not installed.")
  }

  ### MAIN FUNCTION
  if (is.null(id)) {
    # Working data frame
    dat <- df %>%
      dplyr::select(cit = {{cit}}) %>%
      dplyr::mutate(cit = as.numeric(cit)) %>%
      stats::na.omit()

    # Calculate g-index
    g_index_value <- agop::index.g(dat$cit)
  } else {
    # Working data frame
    dat <- df %>%
      dplyr::select(id = {{id}}, cit = {{cit}}) %>%
      dplyr::mutate(id = as.character(id), cit = as.numeric(cit)) %>%
      stats::na.omit()

    # Calculate g-index
    g_index_value <- agop::index.g(dat$cit)

    # Optional plot
    if (plot) {
      df_plot <- dat %>%
        dplyr::arrange(dplyr::desc(cit)) %>%
        dplyr::mutate(id = dplyr::row_number(),
                      cit = cumsum(cit) / id)

      print(ggplot2::ggplot(df_plot) +
              ggplot2::geom_point(ggplot2::aes(x = id, y = cit), shape = 16) +
              ggplot2::geom_vline(xintercept = g_index_value,
                                  color = "#ff0000",
                                  linetype = 2) +
              ggplot2::xlab("Number of Articles") +
              ggplot2::ylab("Average Citations") +
              ggplot2::ggtitle("g-index") +
              ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5)))
    }
  }

  # Return value
  return(g_index_value)
}
