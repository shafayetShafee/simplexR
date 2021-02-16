#' Prepares the Z row
#'
#' Eliminates Basic Variables from the Z row of simplex tableau.
#'
#' @param mat A matrix representing a tableau.
#' @param col An integer representing column which contains non-zero basic variable
#'     coefficients in Z-row.
#' @param row An integer representing row by which you will eliminate the Z-row's
#'     non-zero basic varible's coefficients.
#'
#' @return A matrix representing the tableau for next step.
#' @export
#'
#' @examples
#' # please see the package vignette with \code{vignette("simplex", package = "simplexR")}
z_elim <- function(mat, col, row) {
  dt <- mat
  z_row <- dt[1, ]
  dt[1, ] <- z_row - z_row[col] * dt[row, ]
  print(round(dt, 3))
  return(dt)
}
