#' Algorithm for finding the greatest common divisor, Euclidean algorithm
#'@param a integer
#'@param b integer
#'@return an integer which is the gcd(a,b)
#'@references \url{https://en.wikipedia.org/wiki/Euclidean_algorithm}
#'@export
euclidean <- function(a,b) {
  stopifnot(is.numeric(a) & is.numeric(b) &
              a %% 1 == 0 & b %% 1 == 0)
  t <- 0
  while(b != 0) {
    t <- b
    b <- a %% b
    a <- t
  }
  return (a)
}
