#' Format p-values for APA style
#'
#' This function takes a numeric vector of p-values and formats them for
#' reporting in APA style. It handles "< .001", rounding, and removal of the
#' leading zero. It is vectorized and can be customized for different contexts
#' (e.g., in-text vs. tables).
#'
#' @param p A numeric vector of p-values.
#' @param digits The number of decimal places to round to. Defaults to 3.
#' @param prefix A character string to prepend to the formatted value (e.g., "= ").
#'   Set to `""` for use in tables.
#' @param na_char The character string to use for `NA` values.
#'
#' @return A character vector of formatted p-values.
#'
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' format_p(0.023)
#' format_p(c(0.045, 0.00012, NA), prefix = "")
format_p <- function(p, digits = 3, prefix = "= ", na_char = "NA") {

  p_is_na <- is.na(p)

  # Use a dynamic threshold based on digits
  threshold <- 1 / (10^digits)

  # Round p-values and format them
  # Use pmin to avoid rounding up to 1.000 from 0.999...
  rounded_p <- formatC(pmin(p, 1 - threshold), digits = digits, format = "f") %>%
    sub("^0\\.", ".", .)

  # Create the final output using vectorized logic
  result <- ifelse(p < threshold,
                   paste0("< .", paste0(rep("0", digits - 1), collapse = ""), "1"),
                   paste0(prefix, rounded_p))

  # Restore NA values
  result[p_is_na] <- na_char

  return(result)
}
