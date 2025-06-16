#' @title x_index
#'
#' @description Calculate x-index for an institution using bibliometric data from an edge list, with an optional visualisation of ranked citation scores. The function is suitable for including inside loops when plotting parameter is set to "FALSE" or "F".
#'
#' @param df Data frame object containing bibliometric data. This data frame must have at least three columns: one for keywords, one for unique IDs, and one for citation counts. Each row in the data frame should represent a document or publication.
#' @param kw Character string specifying the name of the column in "df" that contains keywords. Each cell in this column may contain no keywords (missing), a single keyword or multiple keywords separated by a specified delimiter.
#' @param id Character string specifying the name of the column in "df" that contains unique identifiers for each document. Each cell in this column must contain a single ID (unless missing) and not multiple IDs.
#' @param cit Character string specifying the name of the column in "df" that contains the number of citations each document has received. Citations must be represented as integers. Each cell in this column should contain a single integer value (unless missing) representing the citation count for the corresponding document.
#' @param type "h" for Hirsch's h-type index or "g" for Egghe's g-type index. Default set to "h".
#' @param dlm Character string specifying the delimiter used in the "kw" column to separate multiple keywords within a single cell. The delimiter should be consistent across the entire "kw" column. Common delimiters include ";", "/", ":", and ",". The default delimiter is set to ";".
#' @param plot Logical value indicating whether to generate and display a plot of the x-index calculation. Set to "TRUE" or "T" to generate the plot, and "FALSE" or "F" to skip plot generation. The default is "FALSE".
#'
#' @return x-index value and plot.
#'
#' @examples
#' # Create an example data frame
#' dat1 <- data.frame(citations = c(0, 1, 1, 2, 3, 5, 8),
#'                    keywords = c("a; b; c", "b; d", "c", "d", "e; g", "f", "g"),
#'                    id = c("abc123", "bcd234", "def345", "efg456", "fgh567", "ghi678", "hij789"),
#'                    categories = c("a; d; e", "b", "c", "d; g", "e", "f", "g"))
#'
#' # Calculate h-type x-index
#' x_index(df = dat1, kw = "keywords", id = "id", cit = "citations")
#'
#' # Create another example data frame
#' dat2 <- data.frame(citations = c(0, 1, 1, 2, 3, 5, 8),
#'                    keywords = c("a/ b/ c", "b/ d", "c", "d", "e/ g", "f", "g"),
#'                    id = c("123", "234", "345", "456", "567", "678", "789"),
#'                    categories = c("a/ d/ e", "b", "c", "d/ g", "e", "f", "g"))
#'
#' # Calculate g-type x-index
#' x_index(df = dat2, kw = "keywords", id = "id", cit = "citations", type = "g", dlm = "/")
#'
#' # Create another example data frame
#' dat3 <- data.frame(citations = c(0, 1, 1, 2, 3, 5, 8),
#'                    keywords = c("a, b, c", "b, d", "c", "d", "e, g", "f", "g"),
#'                    id = c(123, 234, 345, 456, 567, 678, 789),
#'                    categories = c("a: d: e", "b", "c", "d: g", "e", "f", "g"))
#'
#' # Calculate h-type x-index and produce plot
#' x_index(df = dat3, kw = "keywords", id = "id", cit = "citations", dlm = ",", plot = TRUE)
#'
#' @export x_index
#' @importFrom tidyr separate_rows
#' @importFrom dplyr %>% arrange desc filter mutate row_number select
#' @importFrom Matrix colSums sparseMatrix
#' @importFrom agop index.h index.g
#' @importFrom stats na.omit
#' @importFrom ggplot2 aes element_text geom_hline geom_point ggplot ggtitle theme xlab ylab

# Function to calculate x-index
x_index <- function(df, kw, id, cit, type ="h", dlm = ";", plot = FALSE) {

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

  # Create unique keywords and IDs
  unique_keywords <- unique(trimws(dat$kw))
  unique_ids <- unique(dat$id)

  # Create a sparse matrix
  citation_matrix <- Matrix::sparseMatrix(i = match(dat$id, unique_ids),
                                          j = match(trimws(dat$kw), unique_keywords),
                                          x = dat$cit,
                                          dims = c(length(unique_ids), length(unique_keywords)),
                                          dimnames = list(unique_ids, unique_keywords)
                                          )

  # Sum citations for each keyword
  col_sum_citation_matrix <- Matrix::colSums(citation_matrix)

  # Calculate x-index
  if (type == "h") {
    x_index <- agop::index.h(unname(col_sum_citation_matrix))
  }
  if (type == "g") {
    x_index <- agop::index.g(unname(col_sum_citation_matrix))
  }

  if (plot) {
    # Prepare data for plotting
    df_plot <- data.frame(kw = names(col_sum_citation_matrix), cit = col_sum_citation_matrix) %>%
      dplyr::arrange(dplyr::desc(cit)) %>%
      dplyr::mutate(kw = factor(kw, levels = kw))

    # Create and print ggplot for x-index
    print(ggplot2::ggplot(df_plot) +
            ggplot2::geom_point(ggplot2::aes(x = kw, y = cit), shape = 16) +
            ggplot2::geom_hline(yintercept = x_index, color = "#ff0000", linetype = 2) +
            ggplot2::xlab("Keywords") +
            ggplot2::ylab("Total Citations") +
            ggplot2::ggtitle(label = "x-index") +
            ggplot2::theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)))
  }

  # Return value
  return(x_index)
}
