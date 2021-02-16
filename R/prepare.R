#' Prepares The 2nd phase Table
#'
#' prepare the 2nd phase tablue in two-phase simplex method by replacing Z-row
#'
#' @param mat A matrix representing a tableau
#' @param z_row A vetor representing Z row (1st row) of 2nd phase
#'
#'     Note that this function does replace the Z row
#'     at first and then drop the columns of artificial
#'     variable. So Z_row vector length should be same
#'     as the previous Z row of previous tableau matrix.
#' @param drop A vector giving the column indices of artificial variable to remove.
#'
#' @return A matrix representing the tableau for next step.
#' @export
#'
#' @examples
#' # please see the package vignette with \code{vignette("simplex", package = "simplexR")}
prepare <- function(mat, z_row, drop) {
  dt <- mat
  dt[1, ] <- z_row
  dt <- dt[, -drop]
  print(round(dt, 3))
  return(dt)
}
