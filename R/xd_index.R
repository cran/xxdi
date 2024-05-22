#' @title xd_index
#'
#' @description This function calculates the xd-index for an institution using bibliometric data from an edge list, with an optional plot visualisation.
#'
#' @param df Data frame object containing bibliometric data. This data frame must have at least three columns: one for categories, one for unique IDs, and one for citation counts. Each row in the data frame should represent a document or publication.
#' @param cat Character string specifying the name of the column in "df" that contains categories. Each cell in this column may contain no categories (missing), a single category or multiple categories separated by a specified delimiter.
#' @param id Character string specifying the name of the column in "df" that contains unique identifiers for each document. Each cell in this column must contain a single ID (unless missing) and not multiple IDs.
#' @param cit Character string specifying the name of the column in "df" that contains the number of citations each document has received. Citations must be represented as integers. Each cell in this column should contain a single integer value (unless missing) representing the citation count for the corresponding document.
#' @param dlm Character string specifying the delimiter used in the "cat" column to separate multiple categories within a single cell. The delimiter should be consistent across the entire "cat" column. Common delimiters include ";", "/", ":", and ",". The default delimiter is ";".
#' @param plot Logical value indicating whether to generate and display a plot of the xd-index calculation. Set to TRUE or T to generate the plot, and FALSE or F to skip plot generation. The default is FALSE.
#'
#' @return xd-index value and plot for institution.
#'
#' @examples
#' dat1 <- data.frame(citations = c(0, 1, 1, 2, 3, 5, 8),
#'                    keywords = c("a; b; c", "b; d", "c", "d", "e; g", "f", "g"),
#'                    id = c("abc123", "bcd234", "def345", "efg456", "fgh567", "ghi678", "hij789"),
#'                    categories = c("a; d; e", "b", "c", "d; g", "e", "f", "g"))
#' xd_index(df = dat1, cat = "categories", id = "id", cit = "citations")
#'
#' dat2 <- data.frame(citations = c(0, 1, 1, 2, 3, 5, 8),
#'                   keywords = c("a/ b/ c", "b/ d", "c", "d", "e/ g", "f", "g"),
#'                   id = c("123", "234", "345", "456", "567", "678", "789"),
#'                   categories = c("a/ d/ e", "b", "c", "d/ g", "e", "f", "g"))
#' xd_index(df = dat2, cat = "categories", id = "id", cit = "citations", dlm = "/", plot = FALSE)
#'
#' dat3 <- data.frame(citations = c(0, 1, 1, 2, 3, 5, 8),
#'                   keywords = c("a, b, c", "b, d", "c", "d", "e, g", "f", "g"),
#'                   id = c(123, 234, 345, 456, 567, 678, 789),
#'                   categories = c("a: d: e", "b", "c", "d: g", "e", "f", "g"))
#' xd_index(df = dat3, cat = "categories", id = "id", cit = "citations", dlm = ":", plot = TRUE)
#' @export xd_index
#' @importFrom tidyr separate_rows
#' @importFrom Matrix colSums
#' @importFrom agop index.h
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

# Function to calculate xd-index
xd_index <- function(df, cat, id, cit, dlm = ";", plot = FALSE) {

  # Load dependent libraries
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

  dat <- data.frame(cat = df[[cat]], idc = df[[id]], cit = df[[cit]])

  #Change structure
  dat$kwc <- as.character(dat$cat)
  dat$idc <- as.character(dat$idc)
  dat$cit <- as.numeric(dat$cit)

  # Clean dataset
  df_separated <- separate_rows(dat, cat, sep = dlm)

  df_separated <- data.frame(lapply(df_separated, function(x) ifelse(x == "", NA, x)))

  df_sep <- na.omit(df_separated)

  # Filter out unique categories and unique WOS IDs
  unique_categories <- unique(trimws(df_sep$cat))
  unique_ids <- unique(df_sep$idc)

  # Create an empty matrix with rows for unique IDs and columns for unique categories
  citation_matrix <- matrix(0, nrow = length(unique_ids), ncol = length(unique_categories),
                            dimnames = list(unique_ids, unique_categories))

  # Fill the matrix with citation numbers
  for (i in 1:nrow(df_sep)) {
    col_name <- trimws(df_sep$cat[i])
    row_name <- df_sep$idc[i]
    citation_matrix[row_name, col_name] <- df_sep$cit[df_sep$idc == row_name & trimws(df_sep$cat) == col_name]
  }

  # Form a vector of the column sums without the names of the columns
  col_sum_citation_matrix <- unname(colSums(citation_matrix))

  # Calculate xd-index
  xd_index <- index.h(col_sum_citation_matrix)

  if (plot) {
    # Prepare data for plotting
    df <- data.frame(cat = colnames(citation_matrix), cit = col_sum_citation_matrix)
    df <- df[order(df$cit, decreasing = TRUE), ]
    df$cat <- factor(df$cat, levels = df$cat)

    # Create and print ggplot for xd-index
    print(
      ggplot(df) +
        geom_point(aes(x = cat, y = cit), shape = 16) +
        geom_hline(yintercept = xd_index, color = "#ff0000", linetype = 2) +
        xlab("Categories") +
        ylab("Total Citations") +
        ggtitle(label = "xd-index") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
    )
  }

  # Return value
  return(xd_index)
}
