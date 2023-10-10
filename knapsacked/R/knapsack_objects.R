#'
#'Example data for the knapsack problem
#'
#'
#'@format ## `knapsack_objects`
#'A data frame with 2000 rows and 2 columns
#'\describe{
#'\item{w}{weight}
#'\item{v}{value}
#'...}
#'
#'@source \url{https://www.ida.liu.se/~732A94/}
#'
'knapsack_objects'

RNGversion(min(as.character(getRversion()),"3.5.3"))
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )
rm(n)

