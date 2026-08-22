#' @title xo_index - Overall Expertise Index
#'
#' @description
#' Calculate the xo-index for an institution using bibliometric data from an
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
#' @param dlm Character vector specifying the delimiter used in the "kw" and
#'  "cat" columns to separate multiple keywords and categories within a single cell.
#'  The delimiter should be consistent across the entirety of the two columns. The
#'  first element in the vector is used as the delimiter for keywords, while the second element
#'  is used for categories. Common delimiters include ";", "/", ":", and ",", and example usage
#'  should be similar to 'c(";", ";")' (default), 'c(";", ",")', or 'c(",", ":")'.
#' @param plot Logical value indicating whether to generate and display a plot
#' of the xo-index calculation. Set to "TRUE" or "T" to generate the plot, and
#' "FALSE" (default) or "F" to skip plot generation.
#'
#' @return xo-index magnitude, core categories and optional plot.
#'
#' @examples
#' # Load example data
#' data(WoSdata)
#'
#' # Calculate xo-index with plot
#' xo_index(df = WoSdata,
#'          id = "UT.Unique.WOS.ID",
#'          kw = "Keywords.Plus",
#'          cat = "WoS.Categories",
#'          cit = "Times.Cited.WoS.Core",
#'          plot = TRUE)
#'
#' @export
#' @importFrom dplyr %>%

xo_index <- function(df,
                     kw,
                     cat,
                     id,
                     cit,
                     type = "h",
                     dlm = c(";", ";"),
                     plot = FALSE) {

  # check inputs
  checkmate::assert_data_frame(df, min.rows = 2, min.cols = 4, col.names = "named")
  checkmate::assert_string(kw)
  checkmate::assert_string(cat)
  checkmate::assert_string(id)
  checkmate::assert_string(cit)
  checkmate::assert_names(colnames(df), must.include = c(kw, cat, id, cit))
  checkmate::assert_choice(type, choices = c("h", "g"))
  checkmate::assert_character(dlm, len = 2)
  checkmate::assert_flag(plot)

  # set classes of some inputs
  # Convert 'cit' column safely if it isn't already an integer vector
  if (!checkmate::test_integer(df[[cit]])) {
    df[[cit]] <- as.integer(df[[cit]])
  }
  checkmate::assert_integer(df[[cit]], len = nrow(df), lower = 0)
  # Convert 'kw' column safely if it isn't already a character vector
  if (!checkmate::test_character(df[[kw]], any.missing = TRUE)) {
    # Keep track of where real missing values are
    na_mask <- is.na(df[[kw]])
    df[[kw]] <- as.character(df[[kw]])
    # Restore true NA values so they don't become the string "NA"
    df[[kw]][na_mask] <- NA_character_
  }
  # Convert 'cat' column safely if it isn't already a character vector
  if (!checkmate::test_character(df[[cat]], any.missing = TRUE)) {
    # Keep track of where real missing values are
    na_mask <- is.na(df[[cat]])
    df[[cat]] <- as.character(df[[cat]])
    # Restore true NA values so they don't become the string "NA"
    df[[cat]][na_mask] <- NA_character_
  }
  # Convert 'id' column safely if it is provided and not already character
  if (!checkmate::test_character(df[[id]], any.missing = TRUE)) {
    na_mask <- is.na(df[[id]])
    df[[id]] <- as.character(df[[id]])
    df[[id]][na_mask] <- NA_character_
  }

  x <- NULL
  total_cit <- NULL

  # Working data frame
  dat <- df %>%
    dplyr::select(kw = {{kw}}, cat = {{cat}}, id = {{id}}, cit = {{cit}}) %>%
    stats::na.omit()

  # clean working data frame
  dat <- dat %>%
    tidyr::separate_rows(kw, sep = dlm[1]) %>%
    tidyr::separate_rows(cat, sep = dlm[2]) %>%
    dplyr::mutate(kw = trimws(kw), cat = trimws(cat)) %>%
    dplyr::filter(cat != "") %>%
    dplyr::filter(kw != "") %>%
    stats::na.omit() %>%
    dplyr::group_by(cat, kw) %>%
    dplyr::summarise(total_cit = sum(cit), .groups = "drop")

  # Calculate h-type xo-index
  if (type == "h") {
    dat <- dat %>%
      dplyr::group_by(cat) %>%
      dplyr::summarise(x = agop::index.h(total_cit), .groups = "drop")

    # Sum citations for each keyword
    col_sum_citation_matrix <- dat$x
    names(col_sum_citation_matrix) <- dat$cat

    # xo-index
    xo_index <- agop::index.h(unname(col_sum_citation_matrix))
  }

  # Calculate g-type xo-index
  if (type == "g") {
    dat <- dat %>%
      dplyr::group_by(cat) %>%
      dplyr::summarise(x = agop::index.g(total_cit), .groups = "drop")

    # Sum citations for each keyword
    col_sum_citation_matrix <- dat$x
    names(col_sum_citation_matrix) <- dat$cat

    # xo-index
    xo_index <- agop::index.g(unname(col_sum_citation_matrix))
  }

  # get keywords whose citation counts meet the index threshold
  cit_sorted <- sort(col_sum_citation_matrix, decreasing = TRUE)
  core_keywords <- names(cit_sorted)[seq_len(xo_index)]

  if (plot) {
    # Prepare data for plotting
    df_plot <- data.frame(kw = names(col_sum_citation_matrix), cit = col_sum_citation_matrix) %>%
      dplyr::arrange(dplyr::desc(cit)) %>%
      dplyr::mutate(kw = factor(kw, levels = kw))

    # Create and print ggplot for x-index
    print(ggplot2::ggplot(df_plot) +
            ggplot2::geom_point(ggplot2::aes(x = kw, y = cit), shape = 16) +
            ggplot2::geom_segment(x = xo_index,
                                  xend = xo_index,
                                  y = -Inf,
                                  yend = Inf,
                                  color = "#ff0000",
                                  linetype = 2) +
            ggplot2::xlab("Category") +
            ggplot2::ylab("Number of Core Keywords") +
            ggplot2::ggtitle(label = "xo-index") +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5)))
  }

  # Return value
  return(list(xo.index = xo_index,
              xo.keywords = core_keywords))
}
