# Acknowledgement: Henrik Karlsson
name <- "Simon Jonsson"
liuid <- "simjo241"

#library(markmyassignment)
#lab_path <- "https://raw.githubusercontent.com/stimaliu/advrcourse/master/labs/tests/lab3.yml"
#set_assignment(lab_path)

# 1.1 Write the R code
# 1.1.1 euclidean()
euclidean <- function(a,b) {
  t <- 0
  while(b != 0) {
    t <- b
    b <- a %% b
    a <- t
  }
  return (a)
}

# 1.1.2 * dijkstra()
dijkstra <- function(df,init_node) {
  stopifnot(is.numeric(init_node) & is.atomic(init_node))

  Q <- c()
  dist <- c()
  prev <- c()

  for (v in unique(c(df[["v1"]],df[["v2"]]))) {
    dist[v] <- 10000
    prev[v] <- (NA)
    Q <- c(Q,v)
  }

  dist[init_node] <- 0
  iter <- 0
  while (length(Q) != 0) {
    u <- Q[which.min(dist[Q])]
    Q <- Q[Q != u]

    for (v in df[["v2"]][df[["v1"]] == u]) {
      weight <- df[["w"]][df[["v1"]] == v & df[["v2"]] == u]
      alt <- dist[u] + weight

      if (alt < dist[v]) {
        dist[v] <- alt
        prev[v] <- u
      }
    }
  }
  return (dist)
}

wiki_graph <-
  data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
             v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
             w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
