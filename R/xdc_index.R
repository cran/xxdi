#' @title xdc_index
#'
#' @description This is a general function that calculates the x-index, xd-index, and xc-index for an institution using bibliometric data from an edge list. Returns the x-index or the xd-index depending on the input vector in "p1". Returns x-index when "p1" is the dataframe column containing keywords and xd-index when "p1" the dataframe column containing categories. Returns a summary table listing x-index, xd-index, and xc-index when both "p1" and "p2" are supplied. In this case, "p2" must be the higher level, i.e., categories and "p1" must be the lower level, i.e., keywords. The function is suitable for including inside loops when only one input vector is provided. However, for looping xc-index, a separate 'xc_index' function is provided which also includes an option to produce plots. Similarly, 'x_index' and 'xd_index' are standalone functions for calculating x-index and xd-index respectively as well as producing plots.
#'
#' @param df Data frame object containing bibliometric data. This data frame must have at least three columns: one for keywords/categories, one for unique IDs, and one for citation counts. Each row in the data frame should represent a document or publication.
#' @param p1 Character string specifying the name of the column in "df" that contains the first parameter of interest. Generally, this will be a column for keywords/categories. Each cell in this column may contain no keywords/categories (missing), a single keyword/category or multiple keywords/categories separated by a specified delimiter. Input the column for keywords to calculate the x-index, and the column for categories to calculate the xd-index.
#' @param p2 Character string specifying the name of the column in "df" that contains the second parameter of interest. This is an optional parameter only required when calculating xc-index. When an input is provided, it is expected that "p1" is the lower level, generally keywords, and "p2" is the higher level, generally categories. Each cell in this column may contain no categories (missing), a single category or multiple categories separated by a specified delimiter. The default is set to "NULL".
#' @param id Character string specifying the name of the column in "df" that contains unique identifiers for each document. Each cell in this column must contain a single ID (unless missing) and not multiple IDs.
#' @param cit Character string specifying the name of the column in "df" that contains the number of citations each document has received. Citations must be represented as integers. Each cell in this column should contain a single integer value (unless missing) representing the citation count for the corresponding document.
#' @param dlm1 Character string specifying the delimiter used in the "p1" column to separate multiple keywords/categories within a single cell. The delimiter should be consistent across the entire "p1" column. Common delimiters include ";", "/", ":", and ",". The default delimiter is set to ";".
#' @param dlm2 Character string specifying the delimiter used in the "p2" column to separate multiple categories within a single cell. The delimiter should be consistent across the entire "p2" column. Common delimiters include ";", "/", ":", and ",". The default delimiter is set to ";".
#'
#' @return x-index value, and/or xd-index value, and/or xc-index value for institution.
#'
#' @examples
#' # Create an example data frame
#' dat1 <- data.frame(citations = c(0, 1, 1, 2, 3, 5, 8),
#'                    keywords = c("a; b; c", "b; d", "c", "d", "e; g", "f", "g"),
#'                    id = c("abc123", "bcd234", "def345", "efg456", "fgh567", "ghi678", "hij789"),
#'                    categories = c("a; d; e", "b", "c", "d; g", "e", "f", "g"))
#' # Calculate x-index
#' xdc_index(df = dat1, p1 = "keywords", id = "id", cit = "citations")
#' # Calculate xd-index
#' xdc_index(df = dat1, p1 = "categories", id = "id", cit = "citations")
#' # Calculate x-index, xd-index, and xc-index together
#' xdc_index(df = dat1, p1 = "keywords", p2 = "categories", id = "id", cit = "citations")
#' @export xdc_index
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
#' @importFrom stats na.omit

# Function to calculate x-index
xdc_index <- function(df, p1, p2 = NULL, id, cit, dlm1 = ";", dlm2 = ";") {

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
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required but not installed.")
  }

  if (!is.null(p2)) {
    ### Calculate x-index
    # Working data frame
    dat1 <- df %>%
      select(p1 = {{p1}}, id = {{id}}, cit = {{cit}}) %>%
      mutate(p1 = as.character(p1), id = as.character(id), cit = as.numeric(cit)) %>%
      na.omit()

    # Clean dataset
    dat1 <- dat1 %>%
      separate_rows(p1, sep = dlm1) %>%
      filter(p1 != "") %>%
      na.omit()

    # Create unique keywords and IDs
    unique_keywords <- unique(trimws(dat1$p1))
    unique_ids <- unique(dat1$id)

    # Create a sparse matrix
    citation_matrix <- sparseMatrix(
      i = match(dat1$id, unique_ids),
      j = match(trimws(dat1$p1), unique_keywords),
      x = dat1$cit,
      dims = c(length(unique_ids), length(unique_keywords)),
      dimnames = list(unique_ids, unique_keywords)
    )

    # Sum citations for each keyword
    col_sum_citation_matrix <- colSums(citation_matrix)

    # Calculate x-index
    x_index <- index.h(unname(col_sum_citation_matrix))

    ### Calculate xd-index
    # Working data frame
    dat2 <- df %>%
      select(p2 = {{p2}}, id = {{id}}, cit = {{cit}}) %>%
      mutate(p2 = as.character(p2), id = as.character(id), cit = as.numeric(cit)) %>%
      na.omit()

    # Clean dataset
    dat2 <- dat2 %>%
      separate_rows(p2, sep = dlm2) %>%
      filter(p2 != "") %>%
      na.omit()

    # Create unique keywords and IDs
    unique_keywords <- unique(trimws(dat2$p2))
    unique_ids <- unique(dat2$id)

    # Create a sparse matrix
    citation_matrix <- sparseMatrix(
      i = match(dat2$id, unique_ids),
      j = match(trimws(dat2$p2), unique_keywords),
      x = dat2$cit,
      dims = c(length(unique_ids), length(unique_keywords)),
      dimnames = list(unique_ids, unique_keywords)
    )

    # Sum citations for each keyword
    col_sum_citation_matrix <- colSums(citation_matrix)

    # Calculate xd-index
    xd_index <- index.h(unname(col_sum_citation_matrix))

    ### Calculate xc-index
    # Working data frame
    dat3 <- df %>%
      select(p1 = {{p1}}, p2 = {{p2}}, id = {{id}}, cit = {{cit}}) %>%
      mutate(p1 = as.character(p1), p2 = as.character(p2), id = as.character(id), cit = as.numeric(cit)) %>%
      na.omit()

    # Clean dataset
    dat3 <- dat3 %>%
      separate_rows(p1, sep = dlm1) %>%
      separate_rows(p2, sep = dlm2) %>%
      mutate(p1 = trimws(p1), p2 = trimws(p2), p1 = paste0(p1, " (", p2, ")")) %>%
      filter(p1 != "") %>%
      na.omit()

    # Create unique keywords and IDs
    unique_keywords <- unique(dat3$p1)
    unique_ids <- unique(dat3$id)

    # Create a sparse matrix
    citation_matrix <- sparseMatrix(
      i = match(dat3$id, unique_ids),
      j = match(dat3$p1, unique_keywords),
      x = dat3$cit,
      dims = c(length(unique_ids), length(unique_keywords)),
      dimnames = list(unique_ids, unique_keywords)
    )

    # Sum citations for each categorical keyword
    col_sum_citation_matrix <- colSums(citation_matrix)

    # Calculate xc-index
    xc_index <- index.h(unname(col_sum_citation_matrix))

    # Create output df
    df_values <- c(x_index, xd_index, xc_index) # Vector of index values
    matrix_data <- matrix(df_values, nrow = 1, byrow = TRUE) # Transform into matrix
    df_out <- data.frame(matrix_data) # Convert matrix into data frame object
    colnames(df_out) <- c("x-index", "xd-index", "xc-index") # Assign column names

    # Return value
    return(df_out)
  }
  else {
    # Working data frame
    dat <- df %>%
      select(p1 = {{p1}}, id = {{id}}, cit = {{cit}}) %>%
      mutate(p1 = as.character(p1), id = as.character(id), cit = as.numeric(cit)) %>%
      na.omit()

    # Clean dataset
    dat <- dat %>%
      separate_rows(p1, sep = dlm1) %>%
      filter(p1 != "") %>%
      na.omit()

    # Create unique keywords and IDs
    unique_keywords <- unique(trimws(dat$p1))
    unique_ids <- unique(dat$id)

    # Create a sparse matrix
    citation_matrix <- sparseMatrix(
      i = match(dat$id, unique_ids),
      j = match(trimws(dat$p1), unique_keywords),
      x = dat$cit,
      dims = c(length(unique_ids), length(unique_keywords)),
      dimnames = list(unique_ids, unique_keywords)
    )

    # Sum citations for each keyword
    col_sum_citation_matrix <- colSums(citation_matrix)

    # Calculate xdc-index
    xdc_index <- index.h(unname(col_sum_citation_matrix))

    # Return value
    return(xdc_index)
  }
}
