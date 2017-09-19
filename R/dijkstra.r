
#' A simple implementation of the Dijkstra algorithm
#'@param graph a data.frame with three columns v1,v2,w
#'@param init_node the initial node, an integer
#'@return a vector of distances
#'@references \url{https://en.wikipedia.org/wiki/Dijkstra\%27s_algorithm}
#'@export
dijkstra <- function(graph,init_node) {
  stopifnot(is.numeric(init_node) &
              is.atomic(init_node) &
              is.data.frame(graph) &
              (length(graph[[1]]) == length(graph[[2]])) &
              (length(graph[[2]]) == length(graph[[3]])) &
              all(colnames(graph) == c("v1", "v2", "w")) &
              length(colnames(graph)) == 3 &
              (init_node %in% graph[[1]] || init_node %in% graph[[2]])
  )

  Q <- c()
  dist <- c()
  prev <- c()

  for (v in unique(c(graph[["v1"]],graph[["v2"]]))) {
    dist[v] <- Inf
    prev[v] <- (NA)
    Q <- c(Q,v)
  }

  dist[init_node] <- 0
  while (length(Q) != 0) {
    u <- Q[which.min(dist[Q])]
    Q <- Q[Q != u]

    for (v in graph[["v2"]][graph[["v1"]] == u]) {
      weight <- graph[["w"]][graph[["v1"]] == v & graph[["v2"]] == u]
      alt <- dist[u] + weight

      if (alt < dist[v]) {
        dist[v] <- alt
        prev[v] <- u
      }
    }
  }
  return (dist)
}
#' A data set of a bipartite graph with weights
#'@docType data
#'@name wiki_graph
#'@title wiki_graph
#'@usage data(wiki_graph)
#'@format A data frame with vectors v1, v2, w
#'@export
NULL
