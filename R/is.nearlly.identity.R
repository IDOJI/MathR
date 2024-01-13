is.nearly.identity <- function(mat, tolerance = 1e-8) {
  # Check dimension of mat
  if (!is.matrix(mat)) stop("The input is not a matrix.")
  if (nrow(mat) != ncol(mat)) return(FALSE)
  
  # check if the diagonal elements are 1
  if (any(abs(diag(mat) - 1) > tolerance)) return(FALSE)
  
  # Check if the non-diagonal elements are near 0
  off.diagonal <- mat - diag(diag(mat))
  if (any(abs(off.diagonal) > tolerance)) return(FALSE)
  
  return(TRUE)
}

# 예제 :
# mat <- diag(3)  # 3x3 단위행렬 생성
# mat[1, 2] <- 1e-9  # 0에 매우 가까운 비대각선 원소 추가
# is.nearly.identity(mat)  # TRUE가 반환
