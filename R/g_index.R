#' @title g_index
#'
#' @description Calculate g-index for an institution using bibliometric data from an edge list, with an optional visualisation of ranked citation scores.
#'
#' @param df Data frame object containing bibliometric data. This data frame must have at least two columns: one for keywords and one for citation counts. An optional column for unique identifiers can be included. Each row in the data frame should represent a document or publication.
#' @param id Character string specifying the name of the column in "df" that contains unique identifiers for each document. Each cell in this column must contain a single ID (unless missing) and not multiple IDs. Only required when 'plot' parameter is set to "TRUE". Default set to NULL.
#' @param cit Character string specifying the name of the column in "df" that contains the number of citations each document has received. Citations must be represented as integers. Each cell in this column should contain a single integer value (unless missing) representing the citation count for the corresponding document.
#' @param plot Logical value indicating whether to generate and display a plot of the g-index calculation. Set to "TRUE" or "T" to generate the plot, and "FALSE" or "F" to skip plot generation. The default is "FALSE".
#'
#' @return g-index value and plot.
#'
#' @examples
#' # Create an example data frame
#' dat1 <- data.frame(citations = c(0, 1, 1, 2, 3, 5, 8),
#'                    keywords = c("a; b; c", "b; d", "c", "d", "e; g", "f", "g"),
#'                    id = c("abc123", "bcd234", "def345", "efg456", "fgh567", "ghi678", "hij789"),
#'                    categories = c("a; d; e", "b", "c", "d; g", "e", "f", "g"))
#' # Calculate g-index
#' g_index(df = dat1, cit = "citations")
#'
#' # Create another example data frame
#' dat2 <- data.frame(citations = c(0, 1, 1, 2, 3, 5, 8),
#'                    keywords = c("a/ b/ c", "b/ d", "c", "d", "e/ g", "f", "g"),
#'                    id = c("123", "234", "345", "456", "567", "678", "789"),
#'                    categories = c("a/ d/ e", "b", "c", "d/ g", "e", "f", "g"))
#'
#' # Calculate g-index
#' g_index(df = dat2, id = "id", cit = "citations", plot = FALSE)
#'
#' # Create another example data frame
#' dat3 <- data.frame(citations = c(0, 1, 1, 2, 3, 5, 8),
#'                    keywords = c("a, b, c", "b, d", "c", "d", "e, g", "f", "g"),
#'                    id = c(123, 234, 345, 456, 567, 678, 789),
#'                    categories = c("a: d: e", "b", "c", "d: g", "e", "f", "g"))
#'
#' # Calculate g-index and produce plot
#' g_index(df = dat3, id = "id", cit = "citations", plot = TRUE)
#'
#' @export g_index
#' @importFrom dplyr %>% arrange desc mutate row_number select
#' @importFrom agop index.g
#' @importFrom stats na.omit
#' @importFrom ggplot2 aes element_text geom_hline geom_point ggplot theme ggtitle xlab ylab

# Function to calculate g-index
g_index <- function(df, id = NULL, cit, plot = FALSE) {

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

    if (plot) {
      # Prepare data for plotting
      df_plot <- dat %>%
        dplyr::arrange(dplyr::desc(cit)) %>%
        dplyr::mutate(id = row_number(),
                      cit = cumsum(cit)/id)

      # Create and print ggplot for g-index
      print(ggplot2::ggplot(df_plot) +
              ggplot2::geom_point(ggplot2::aes(x = id, y = cit), shape = 16) +
              ggplot2::geom_hline(yintercept = g_index_value, color = "#ff0000", linetype = 2) +
              ggplot2::xlab("Number of Articles") +
              ggplot2::ylab("Average Citations") +
              ggplot2::ggtitle("g-index") +
              ggplot2::theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)))
    }
  }

  # Return value
  return(g_index_value)
}
