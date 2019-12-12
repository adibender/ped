#' Extract information of the sample contained in a data set
#'
#' Given a data set and grouping variables, this function returns mean values
#' for numeric variables and modus for characters and factors. Usually
#' this function should not be called directly but will rather be called
#' as part of a call to \code{make_newdata}.
#'
#' @rdname sample_info
#' @param x A data frame (or object that inherits from \code{data.frame}).
#' @importFrom stats median
#' @return A data frame containing sample information (for each group).
#' If applied to an object of class \code{ped}, the sample means of the
#' original data is returned.
#' Note: When applied to a \code{ped} object, that doesn't contain covariates
#' (only interval information), returns data frame with 0 columns.
#'
#' @export
#' @keywords internal
sample_info <- function(x) {
  UseMethod("sample_info", x)
}

#' @import checkmate dplyr
#' @importFrom purrr compose
#' @export
#' @rdname sample_info
sample_info.data.frame <- function(x) {

  cn  <- colnames(x)
  num <- summarize_if (x, .predicate = is.numeric, ~mean(., na.rm = TRUE))
  fac <- summarize_if (x, .predicate = compose("!", is.numeric), modus)

  nnames <- intersect(names(num), names(fac))

  if (length(nnames) != 0) {
    suppressMessages(
      x <- left_join(num, fac) %>% grouped_df(vars = lapply(nnames, as.name))
    )
  } else {
    x <- bind_cols(num, fac)
  }

  return(select(x, one_of(cn)))

}

#' @rdname sample_info
#' @import checkmate dplyr
#' @importFrom rlang sym
#' @export
sample_info.ped <- function(x) {
  # is.grouped_df
  # remove "noise" information on interval variables
  grps <- group_vars(x)
  iv <- attr(x, "intvars")
  id_var <- attr(x, "id_var")
  x <- x %>%
    group_by(!!sym(id_var)) %>%
    slice(1) %>%
    ungroup() %>%
    grouped_df(grps) %>%
    select(-one_of(iv))
  if (test_data_frame(x, min.rows = 1, min.cols = 1)) {
    sample_info.data.frame(x)
  } else {
    NULL
  }

}

#' @rdname sample_info
#' @export
sample_info.fped <- function(x) {

  x %>% select_if(~!is.matrix(.x)) %>% sample_info.ped()

}
