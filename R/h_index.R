#' @title h_index
#'
#' @description This function calculates the h-index for an institution using bibliometric data from an edge list, with an optional plot visualisation.
#'
#' @param df Data frame object containing bibliometric data. This data frame must have at least three columns: one for keywords, one for unique IDs, and one for citation counts. Each row in the data frame should represent a document or publication.
#' @param id Character string specifying the name of the column in "df" that contains unique identifiers for each document. Each cell in this column must contain a single ID (unless missing) and not multiple IDs. Only required when 'plot' parameter is set to "TRUE". Default set to NULL.
#' @param cit Character string specifying the name of the column in "df" that contains the number of citations each document has received. Citations must be represented as integers. Each cell in this column should contain a single integer value (unless missing) representing the citation count for the corresponding document.
#' @param plot Logical value indicating whether to generate and display a plot of the h-index calculation. Set to "TRUE" or "T" to generate the plot, and "FALSE" or "F" to skip plot generation. The default is "FALSE".
#'
#' @return h-index value and plot for institution.
#'
#' @examples
#' # Create an example data frame
#' dat1 <- data.frame(citations = c(0, 1, 1, 2, 3, 5, 8),
#'                    keywords = c("a; b; c", "b; d", "c", "d", "e; g", "f", "g"),
#'                    id = c("abc123", "bcd234", "def345", "efg456", "fgh567", "ghi678", "hij789"),
#'                    categories = c("a; d; e", "b", "c", "d; g", "e", "f", "g"))
#' # Calculate h-index
#' h_index(df = dat1, cit = "citations")
#'
#' # Create another example data frame
#' dat2 <- data.frame(citations = c(0, 1, 1, 2, 3, 5, 8),
#'                   keywords = c("a/ b/ c", "b/ d", "c", "d", "e/ g", "f", "g"),
#'                   id = c("123", "234", "345", "456", "567", "678", "789"),
#'                   categories = c("a/ d/ e", "b", "c", "d/ g", "e", "f", "g"))
#' # Calculate h-index
#' h_index(df = dat2, id = "id", cit = "citations", plot = FALSE)
#'
#' # Create another example data frame
#' dat3 <- data.frame(citations = c(0, 1, 1, 2, 3, 5, 8),
#'                   keywords = c("a, b, c", "b, d", "c", "d", "e, g", "f", "g"),
#'                   id = c(123, 234, 345, 456, 567, 678, 789),
#'                   categories = c("a: d: e", "b", "c", "d: g", "e", "f", "g"))
#' # Calculate h-index and produce plot
#' h_index(df = dat3, id = "id", cit = "citations", plot = TRUE)
#' @export h_index
#' @importFrom dplyr %>%
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom dplyr mutate
#' @importFrom dplyr row_number
#' @importFrom dplyr select
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

# Function to calculate h-index
h_index <- function(df, id = NULL, cit, plot = FALSE) {

  # Load required libraries
  if (!requireNamespace("agop", quietly = TRUE)) {
    stop("Package 'agop' is required but not installed.")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required but not installed.")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required but not installed.")
  }

  if (is.null(id)) {
    # Working data frame
    dat <- df %>%
      select(cit = {{cit}}) %>%
      mutate(cit = as.numeric(cit)) %>%
      na.omit()

    # Calculate h-index
    h_index_value <- index.h(dat$cit)
  } else {
    # Working data frame
    dat <- df %>%
      select(id = {{id}}, cit = {{cit}}) %>%
      mutate(id = as.character(id), cit = as.numeric(cit)) %>%
      na.omit()

    # Calculate h-index
    h_index_value <- index.h(dat$cit)

    if (plot) {
      # Prepare data for plotting
      df_plot <- dat %>%
        arrange(desc(cit)) %>%
        mutate(id = factor(id, levels = id))

      # Create and print ggplot for h-index
      print(ggplot(df_plot) +
              geom_point(aes(x = id, y = cit), shape = 16) +
              geom_hline(yintercept = h_index_value, color = "#ff0000", linetype = 2) +
              xlab("Articles") +
              ylab("Citations") +
              ggtitle("h-index") +
              theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)))
    }
  }

  # Return value
  return(h_index_value)
}
