#' @title ivw_xd_index - Inverse Variance Weighted (IVW) Expertise Diversity (xd-) Index
#'
#' @description
#' Calculate IVW adjusted xd-index for an institution using bibliometric data from
#' an edge list, with an optional visualisation of ranked citation scores. The function
#' is suitable for including inside loops when plotting parameter is set to "FALSE" or "F".
#'
#' @param df Data frame object containing bibliometric data. This data frame must
#'  have at least three columns: one for categories, one for unique IDs, and one
#'  for citation counts. Each row in the data frame should represent a document
#'  or publication.
#' @param cat Character string specifying the name of the column in "df" that
#'  contains categories. Each cell in this column may contain no categories (missing),
#'  a single category or multiple categories separated by a specified delimiter.
#' @param id Character string specifying the name of the column in "df" that contains
#'  unique identifiers for each document. Each cell in this column must contain a single
#'  ID (unless missing) and not multiple IDs.
#' @param cit Character string specifying the name of the column in "df" that
#'  contains the number of citations each document has received. Citations must
#'  be represented as integers. Each cell in this column should contain a single
#'  integer value (unless missing) representing the citation count for the
#'  corresponding document.
#' @param vfc Data frame with columns 'cat' and 'var_cit'. Optionally required
#'  for using population variances.
#' @param type "h" (default) for Hirsch's h-type index or "g" for Egghe's g-type index.
#' @param dlm Character string specifying the delimiter used in the "cat" column
#'  to separate multiple categories within a single cell. The delimiter should be
#'  consistent across the entire "cat" column. Common delimiters include ";" (default), "/",
#'  ":", and ",".
#' @param plot Logical value indicating whether to generate and display a plot of
#'  the xd-index calculation. Set to "TRUE" or "T" to generate the plot, and "FALSE" (default)
#'  or "F" to skip plot generation.
#'
#' @return IVW xd-index magnitude, core categories, and optional plot.
#'
#' @examples
#' # Load example data
#' data(WoSdata)
#'
#' # Calculate ivw xd-index with plot
#' ivw_xd_index(df = WoSdata,
#'         id = "UT.Unique.WOS.ID",
#'         cat = "WoS.Categories",
#'         cit = "Times.Cited.WoS.Core",
#'         plot = TRUE)
#'
#' @export
#' @importFrom tidyr separate_rows
#' @importFrom dplyr %>% arrange desc filter mutate row_number select
#' @importFrom Matrix colSums sparseMatrix
#' @importFrom agop index.h index.g
#' @importFrom stats na.omit var
#' @importFrom ggplot2 aes element_text geom_segment geom_point ggplot ggtitle theme xlab ylab

ivw_xd_index <- function(df,
                         cat,
                         id,
                         cit,
                         vfc = NULL,
                         type = "h",
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

  # declare global variable
  var_cit <- NULL

  # Working data frame
  dat <- df %>%
    dplyr::select(cat = {{cat}}, id = {{id}}, cit = {{cit}}) %>%
    dplyr::mutate(cat = as.character(cat), id = as.character(id), cit = as.numeric(cit)) %>%
    stats::na.omit()

  # Clean dataset
  dat <- dat %>%
    tidyr::separate_rows(cat, sep = dlm) %>%
    dplyr::mutate(cat = trimws(cat)) %>%
    dplyr::filter(cat != "") %>%
    stats::na.omit()

  # check vfc
  if (is.null(vfc)) {
    message("'vfc' not provided. Computing category variances from provided data.")

    dat <- dat %>%
      dplyr::group_by(cat) %>%
      dplyr::mutate(var_cit = stats::var(cit, na.rm = TRUE))
  } else {
    dat <- dat %>%
      dplyr::group_by(cat)

    dat <- dplyr::left_join(dat, vfc, by = "cat")
  }

  # remove na variances
  var_cit_na <- length(unique(dat$cat[is.na(dat$var_cit)]))

  if (var_cit_na > 0) {
    message(paste0("Variance cannot be computed for ", var_cit_na, " category(s)."))
    message("Categories occurring only once are likely to result in NA variances.")
    message(paste0("Excluding ", var_cit_na, " category(s)."))

    dat <- dat[!is.na(dat$var_cit), ]
  }

  # Replace zero-variance categories (cannot be weighted)
  var_cit_zero <- length(unique(dat$cat[dat$var_cit == 0]))

  if (var_cit_zero > 0) {

    # print out zero variance categories
    zero_var_idx <- dat$var_cit == 0
    zero_var_cats <- unique(dat$cat[zero_var_idx])
    message(paste0("Found ", var_cit_zero, " category(s) with zero variance(s): ",
                   paste(zero_var_cats, collapse = "; ")))

    message("Replacing with '0.01' to allow index calculation.")
    message("It is recommended to check why category(s) produced zero variances.")

    dat$var_cit[dat$var_cit == 0] <- 0.01
  }

  # ivw citations
  dat <- dat %>%
    dplyr::mutate(cit = cit / var_cit)

  # Sum citations per category specific keyword
  dat <- dat %>%
    dplyr::summarise(total_cit = sum(cit), .groups = "drop")

  # Sum citations for each keyword
  col_sum_citation_matrix <- dat$total_cit
  names(col_sum_citation_matrix) <- dat$cat

  # Calculate xd-index
  if (type == "h") {
    ivw_xd_index <- agop::index.h(unname(col_sum_citation_matrix))
  }
  if (type == "g") {
    ivw_xd_index <- agop::index.g(unname(col_sum_citation_matrix))
  }

  # get keywords whose citation counts meet the index threshold
  cit_sorted <- sort(col_sum_citation_matrix, decreasing = TRUE)
  core_keywords <- names(cit_sorted)[seq_len(ivw_xd_index)]

  if (plot) {
    # Prepare data for plotting
    df_plot <- data.frame(cat = names(col_sum_citation_matrix),
                          cit = col_sum_citation_matrix) %>%
      dplyr::arrange(dplyr::desc(cit)) %>%
      dplyr::mutate(cat = factor(cat, levels = cat))

    # Create and print ggplot for xd-index
    print(ggplot2::ggplot(df_plot) +
            ggplot2::geom_point(ggplot2::aes(x = cat, y = cit), shape = 16) +
            ggplot2::geom_segment(x = ivw_xd_index,
                                  xend = ivw_xd_index,
                                  y = -Inf,
                                  yend = Inf,
                                  color = "#ff0000",
                                  linetype = 2) +
            ggplot2::xlab("Categories") +
            ggplot2::ylab("Total Citations") +
            ggplot2::ggtitle(label = "IVW xd-index") +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5)))
  }

  # Return value
  return(list(ivw.xd.index = ivw_xd_index,
              ivw.xd.categories = core_keywords))
}
