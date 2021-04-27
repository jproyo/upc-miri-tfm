library(sna)
library(igraph)
library("data.table") 
library("DirectedClustering")

setwd("~/Projects/upc/upc-miri-tfm/connected-comp/misc")

run_metrics <- function(file_graph){
  graph <- read_graph(file_graph, format=c("edgelist"))
  E = length(E(graph))
  N = length(V(graph))
  k = 2 * E / N
  delta = 2 * E / (N * (N - 1))
  DIAM = diameter(graph)
  MGD = 0
  # MGD = average.path.length(graph)
  # closeness_cent <- estimate_closeness(graph)
  # mean_cc <- transitivity(graph, type = "average")
  
  closeness_cent <- 0
  
  mean_cc <- 0 

  return(c(N, E, round(k, 2), round(delta, 4),round(MGD, 4), DIAM, round(closeness_cent,4), round(mean_cc,4)))
}

get_graph_name <- function(file){
  return(tools::file_path_sans_ext(basename(file)))
}

process_graphs <- function(path_graph, label){
  print(paste("Running", label, "......."))
  table <- NULL
  rows <- NULL
  files <- list.files(path = path_graph, pattern = "*.txt", full.names = TRUE)
  for (f in files) {
    table <- rbind(table, run_metrics(f))
  }
  df <- data.frame(table)
  colnames(df) = c("N", "E", "K", "Delta", "MGD", "Diameter", "Mean Closeness Cent", "Mean Clustering Coef")
  rownames(df) = lapply(files, get_graph_name)
  print.data.frame(df, right=FALSE)
  summary(df)
}


main <- function(){
  process_graphs("../data", "Graphs for Analysis")
}

cc <- function(){
  start_time <- Sys.time()
  g_enron <- read_graph("../data/email-Enron.txt", format=c("edgelist"))
  length(decompose(g_enron, mode = c("weak")))
  end_time <- Sys.time()
  end_time - start_time
  
  start_time <- Sys.time()
  g_ca <- read_graph("../data/ca-AstroPh.txt", format=c("edgelist"))
  length(decompose(g_ca, mode = c("weak")))
  end_time <- Sys.time()
  end_time - start_time
  
  start_time <- Sys.time()
  g_google <- read_graph("../data/web-Google.txt", format=c("edgelist"))
  length(decompose(g_google, mode = c("weak")))
  end_time <- Sys.time()
  end_time - start_time
}

main()
