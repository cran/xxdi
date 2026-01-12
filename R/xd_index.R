#' @title xd_index - Expertise Diversity (xd-) Index
#'
#' @description
#' Calculate the xd-index (and its variants, field-normalized and fractional)
#' for an institution using bibliometric data from an edge list, with an optional
#' visualisation of ranked citation scores.
#'
#' @param df Data frame object containing bibliometric data. Must have at least
#'  three columns: one for categories, one for unique IDs, and one for citation
#'  counts.
#' @param cat Character string specifying the name of the column in "df" that contains categories.
#'  Categories can be multiple separated by a delimiter.
#' @param id Character string specifying the name of the column in "df" that
#'  contains unique identifiers for each document. Each cell in this column must
#'  contain a single ID (unless missing) and not multiple IDs.
#' @param cit Character string specifying the name of the column in "df" that
#'  contains the number of citations each document has received. Citations must
#'  be represented as integers. Each cell in this column should contain a single
#'  integer value (unless missing) representing the citation count for the
#'  corresponding document.
#' @param mfc Data frame with columns 'cat' and 'mean_cit'. Optionally required to
#'  utilise population means when variant set to "f".
#' @param type "h" (default) for Hirsch's h-type index or "g" for Egghe's g-type index.
#' @param dlm Character string specifying the delimiter used in the "cat" column
#'  to separate multiple categories within a single cell. The delimiter should be
#'  consistent across the entire "cat" column. Common delimiters include ";" (default), "/",
#'  ":", and ",".
#' @param variant One of "full" (default), "f", or "FN".
#' \itemize{
#'   \item \code{"full"} — Computes the unconditional xd-index.
#'   \item \code{"f"} — Computes the fractional xd-index. If set to 'f', input data
#'    frame 'df' must include an 'inst_count' column which gives the number of institutions
#'    per publication.
#'   \item \code{"FN"} — Computes the field-normalised xd-index. If set to 'FN', input
#'    may optionally include an 'mfc' data frame  which gives the population
#'    level mean citations for different fields. If not provided, sample mean field citations will be used.
#' }
#' @param plot Logical value indicating whether to generate and display a plot of
#'  the xd-index calculation. Set to "TRUE" or "T" to generate the plot, and "FALSE" (default)
#'  or "F" to skip plot generation.
#'
#' @return xd-index magnitude, core categories, and optional plot.
#'
#' @examples
#' # Load example data
#' data(WoSdata)
#'
#' # Calculate xd-index with plot
#' xd_index(df = WoSdata,
#'          id = "UT.Unique.WOS.ID",
#'          cat = "WoS.Categories",
#'          cit = "Times.Cited.WoS.Core",
#'          plot = TRUE)
#'
#' # Calculate field-normalised xd-index with plot
#' xd_index(df = WoSdata,
#'          id = "UT.Unique.WOS.ID",
#'          cat = "WoS.Categories",
#'          cit = "Times.Cited.WoS.Core",
#'          variant = "FN",
#'          plot = TRUE)
#'
#' @export
#'
#' @importFrom tidyr separate_rows
#' @importFrom dplyr %>% arrange desc filter mutate row_number select left_join
#' @importFrom Matrix colSums sparseMatrix
#' @importFrom agop index.h index.g
#' @importFrom stats na.omit
#' @importFrom ggplot2 aes element_text geom_segment geom_point ggplot theme ggtitle xlab ylab

#### Main function ---
xd_index <- function(df,
                     cat,
                     id,
                     cit,
                     mfc = NULL,
                     type = "h",
                     dlm = ";",
                     variant = "full",
                     plot = FALSE) {

  # --- Package checks ---
  for (pkg in c("Matrix","agop","tidyr","ggplot2","dplyr","stats")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' is required but not installed.")
    }
  }

  # declare global variable
  mean_cit <- NULL

  # --- Helper: fractional variant ---
  xd_index_fractional <- function(dat) {

    dat$cit <- dat$cit / dat$inst_count

    dat <- dat %>%
      dplyr::group_by(cat)

    return(dat)
  }


  # --- Helper: field-normalized variant ---
  xd_index_normalised <- function(dat, mfc) {

    if (is.null(mfc)) {
      message("'mfc' not provided. Computing category means from provided data.")

      dat <- dat %>%
        dplyr::group_by(cat) %>%
        dplyr::mutate(mean_cit = mean(cit, na.rm = TRUE))
    } else {
      dat <- dat %>%
        dplyr::group_by(cat)

      dat <- dplyr::left_join(dat, mfc, by = "cat")
    }

    # check for missing mean citations
    mean_cit_na <- length(unique(dat$cat[is.na(dat$mean_cit)]))

    if (mean_cit_na > 0) {
      message(paste0("Found missing mean citations for ", mean_cit_na, " category(s). Excluding publication(s)."))
      dat <- dat[!is.na(dat$mean_cit), ]
    }

    # check for zero means
    mean_cit_zero <- length(unique(dat$cat[dat$mean_cit == 0]))

    if (mean_cit_zero > 0) {
      # print out zero variance categories
      zero_mean_idx <- dat$mean_cit == 0
      zero_mean_cats <- unique(dat$cat[zero_mean_idx])
      message(paste0("Found ", mean_cit_zero, " category(s) with zero variance(s): ",
                     paste(zero_mean_cats, collapse = "; ")))

      message("Replacing with 0.01 to allow index calculations.")
      message("It is recommended to check why category(s) produced zero means.")
      dat$mean_cit[dat$mean_cit == 0] <- 0.01
    }

    dat <- dat %>% dplyr::mutate(cit = cit / mean_cit)

    return(dat)
  }

  # --- Working data frame ---
  dat <- df %>%
    dplyr::select(cat = {{cat}},
                  id = {{id}},
                  cit = {{cit}},
                  dplyr::everything()) %>%
    dplyr::mutate(cat = as.character(cat),
                  id = as.character(id),
                  cit = as.numeric(cit)) %>%
    stats::na.omit()

  # check for institution count column in df
  if (variant == "f") {
    # If inst_count not provided, build it
    if (!"inst_count" %in% colnames(dat)) {
      stop("Institution counts per 'inst_count' not provided.")
    }
  }

  # Split multiple categories
  dat <- dat %>%
    tidyr::separate_rows(cat, sep = dlm) %>%
    dplyr::mutate(cat = trimws(cat)) %>%
    dplyr::filter(cat != "") %>%
    stats::na.omit()

  # --- Handle variants via helpers ---
  if (variant == "FN") {
    dat <- xd_index_normalised(dat, mfc)
  } else if (variant == "f") {
    dat <- xd_index_fractional(dat)
  } else {
    dat <- dat %>%
      dplyr::group_by(cat)
  }

  # Sum citations per category specific keyword
  dat <- dat %>%
    dplyr::summarise(total_cit = sum(cit), .groups = "drop")

  # Sum citations for each keyword
  col_sum_citation_matrix <- dat$total_cit
  names(col_sum_citation_matrix) <- dat$cat

  # --- Compute xd-index ---
  if (type == "h") {
    xd_val <- agop::index.h(unname(col_sum_citation_matrix))
  } else if (type == "g") {
    xd_val <- agop::index.g(unname(col_sum_citation_matrix))
  } else {
    stop("Invalid type. Use 'h' or 'g'.")
  }

  # get keywords whose citation counts meet the index threshold
  cit_sorted <- sort(col_sum_citation_matrix, decreasing = TRUE)
  core_keywords <- names(cit_sorted)[seq_len(xd_val)]

  # --- Plot (optional) ---
  if (plot) {
    df_plot <- data.frame(cat = names(col_sum_citation_matrix),
                          cit = col_sum_citation_matrix) %>%
      dplyr::arrange(dplyr::desc(cit)) %>%
      dplyr::mutate(cat = factor(cat, levels = cat))

    print(
      ggplot2::ggplot(df_plot) +
        ggplot2::geom_point(ggplot2::aes(x = cat, y = cit), shape = 16) +
        ggplot2::geom_segment(x = xd_val,
                              xend = xd_val,
                              y = -Inf,
                              yend = Inf,
                              color = "#ff0000",
                              linetype = 2) +
        ggplot2::xlab("Categories") +
        ggplot2::ylab(ifelse(variant == "f",
                             "Total Fractional Citations",
                             ifelse(variant == "FN", "Total Field-Normalised Citations", "Total Citations"))) +
        ggplot2::ggtitle(label = paste("xd-index (variant:", variant, ")")) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5))
    )
  }

  return(list(xd.index = xd_val,
              xd.categories = core_keywords))
}
