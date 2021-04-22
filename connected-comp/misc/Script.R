library(igraph)
library("data.table") 

setwd("~/Projects/upc/upc-miri-tfm/connected-comp/misc")

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

