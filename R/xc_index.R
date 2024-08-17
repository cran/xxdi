#' @title xc_index
#'
#' @description This is a standalone function that specifically calculates the xc-index for an institution using bibliometric data from an edge list, with an optional plot visualisation. The function is suitable for including inside loops when plotting parameter is set to "FALSE" or "F".
#'
#' @param df Data frame object containing bibliometric data. This data frame must have at least three columns: one for keywords, one for unique IDs, and one for citation counts. Each row in the data frame should represent a document or publication.
#' @param kw Character string specifying the name of the column in "df" that contains keywords. Each cell in this column may contain no keywords (missing), a single keyword or multiple keywords separated by a specified delimiter.
#' @param cat Character string specifying the name of the column in "df" that contains categories. Each cell in this column may contain no categories (missing), a single category or multiple categories separated by a specified delimiter.
#' @param id Character string specifying the name of the column in "df" that contains unique identifiers for each document. Each cell in this column must contain a single ID (unless missing) and not multiple IDs.
#' @param cit Character string specifying the name of the column in "df" that contains the number of citations each document has received. Citations must be represented as integers. Each cell in this column should contain a single integer value (unless missing) representing the citation count for the corresponding document.
#' @param type "h" for Hirsch's h-type index or "g" for Egghe's g-type index. Default set to "h".
#' @param kdlm Character string specifying the delimiter used in the "kw" column to separate multiple keywords within a single cell. The delimiter should be consistent across the entire "kw" column. Common delimiters include ";", "/", ":", and ",". The default delimiter is set to ";".
#' @param cdlm Character string specifying the delimiter used in the "cat" column to separate multiple categories within a single cell. The delimiter should be consistent across the entire "cat" column. Common delimiters include ";", "/", ":", and ",". The default delimiter is set to ";".
#' @param plot Logical value indicating whether to generate and display a plot of the xc-index calculation. Set to "TRUE" or "T" to generate the plot, and "FALSE" or "F" to skip plot generation. The default is "FALSE".
#'
#' @return xc-index value and plot for institution.
#'
#' @examples
#' # Create an example data frame
#' dat <- data.frame(citations = c(0, 1, 1, 2, 3, 5, 8),
#'                    keywords = c("a; b; c", "b; d", "c", "d", "e; g", "f", "g"),
#'                    id = c("abc123", "bcd234", "def345", "efg456", "fgh567", "ghi678", "hij789"),
#'                    categories = c("a; d; e", "b", "c", "d; g", "e", "f", "g"))
#' # Calculate g-type xc-index
#' xc_index(df = dat, kw = "keywords", cat = "categories", id = "id", cit = "citations", type = "g")
#' # Calculate h-type xc-index and produce plot
#' xc_index(df = dat, kw = "keywords", cat = "categories", id = "id", cit = "citations", plot = TRUE)
#' @export xc_index
#' @importFrom tidyr separate_rows
#' @importFrom dplyr %>%
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr row_number
#' @importFrom dplyr select
#' @importFrom Matrix colSums
#' @importFrom Matrix sparseMatrix
#' @importFrom agop index.h
#' @importFrom agop index.g
#' @importFrom stats na.omit
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab

xc_index <- function(df, kw, cat, id, cit, type = "h", kdlm = ";", cdlm = ";", plot = FALSE) {

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

  # Working data frame
  dat <- df %>%
    select(kw = {{kw}}, cat = {{cat}}, id = {{id}}, cit = {{cit}}) %>%
    mutate(kw = as.character(kw), cat = as.character(cat), id = as.character(id), cit = as.numeric(cit)) %>%
    na.omit()

  # Clean dataset
  dat <- dat %>%
    separate_rows(kw, sep = kdlm) %>%
    separate_rows(cat, sep = cdlm) %>%
    mutate(kw = trimws(kw), cat = trimws(cat), kw = paste0(kw, " (", cat, ")")) %>%
    filter(kw != "") %>%
    na.omit()

  # Create unique keywords and IDs
  unique_keywords <- unique(dat$kw)
  unique_ids <- unique(dat$id)

  # Create a sparse matrix
  citation_matrix <- sparseMatrix(
    i = match(dat$id, unique_ids),
    j = match(dat$kw, unique_keywords),
    x = dat$cit,
    dims = c(length(unique_ids), length(unique_keywords)),
    dimnames = list(unique_ids, unique_keywords)
  )

  # Sum citations for each categorical keyword
  col_sum_citation_matrix <- colSums(citation_matrix)

  # Calculate xc-index
  if (type == "h") {
    xc_index <- index.h(unname(col_sum_citation_matrix))
  }
  if (type == "g") {
    xc_index <- index.g(unname(col_sum_citation_matrix))
  }

  if (plot) {
    # Prepare data for plotting
    df_plot <- data.frame(kw = names(col_sum_citation_matrix), cit = col_sum_citation_matrix) %>%
      arrange(desc(cit)) %>%
      mutate(kw = factor(kw, levels = kw))

    # Create and print ggplot for xc-index
    print(ggplot(df_plot) +
            geom_point(aes(x = kw, y = cit), shape = 16) +
            geom_hline(yintercept = xc_index, color = "#ff0000", linetype = 2) +
            xlab("Categorical Keywords") +
            ylab("Total Citations") +
            ggtitle(label = "xc-index") +
            theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)))
  }

  # Return value
  return(xc_index)
}

