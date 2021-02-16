#' Calculates simplex tableau
#'
#' Calculates simplex tableau given a matrix and pivot column and pivot row of that matrix
#'
#' @param mat A matrix representing a tableau
#' @param col An integer representing pivot column of current tableau
#' @param row An integer representing pivot row of current tableau
#'
#' @return A matrix representing the tableau for next step
#' @export
#'
#' @examples
#' # Maximize  Z = 4x1 + 3x2 + 6x3 Subject to, 3x1 + x2 + 3x3 <= 30
#' # 2x1 + 2x2 + 3x3 <= 40 where x1 >= 0, x2 >= 0, x3 >= 0.
#'
#' # Define the linear programming problem by a augmented matrix form.
#' prob_mat <- matrix(c(-4, -3, -6, 0, 0, 0, 3, 1, 3, 1,
#'                       0, 30, 2, 2, 3, 0, 1, 40),
#'                      byrow = TRUE, ncol = 6)
#'
#' # check how that matrix looks like and find pivot row and pivot column.
#' prob_mat
#'
#' # pivot column is 3rd one and pivot row is 2nd one.
#' prob_mat <- tableau(prob_mat, col = 3, row = 2)
#'
#' # now pivot column is 2nd one and pivot row is 3rd one.
#' prob_mat <- tableau(prob_mat, col = 2, row = 3)
tableau <- function(mat, col, row) {
  m <- mat
  pivot_row <- m[row, ]
  pivot_col <- m[, col]

  new_row <- pivot_row / pivot_col[row]

  for (i in 1:nrow(m)) {
    m[i, ] <- row_update(new_row, mat[i, ], pivot_col[i])
  }

  m[row, ] <- new_row
  print(round(m, 3))
  return(m)
}




# Helper ------------------------------------------------------------------


row_update <- function(new_row, current_row, pivot) {
  if (pivot < 0) {
    return((abs(pivot) * new_row) + current_row)
  } else {
    return(current_row - (pivot * new_row))
  }
}



