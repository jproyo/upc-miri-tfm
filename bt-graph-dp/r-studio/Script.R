library(igraph)
library("data.table") 
library(ggplot2)

setwd("~/Projects/upc/upc-miri-tfm/bt-graph-dp/r-studio")

exp_time <- matrix(c(8756,3,31,9,596,3,64,8,577,2,23,8,86414,3,1660,21,142232,3,11,8,1591,2,22,8,13609,2,129,9,3828,2,12,11,21583,2,92,11),ncol=4,byrow=TRUE)
rownames(exp_time) <- c("Edge-High","Edge-Low","Edge-Middle","Vertex-Lower-High","Vertex-Lower-Low","Vertex-Lower-Medium","Vertex-Upper-High","Vertex-Upper-Low","Vertex-Upper-Medium")
colnames(exp_time) <- c("dbpedia","moreno_crime","opsahl-ucforum","wang-amazon")

exp_type <- c(rep("E-H" , 4), rep("E-L", 4), rep("E-M", 4), rep("VL-H", 4),rep("VL-L", 4),rep("VL-M", 4),rep("VU-H", 4),rep("VU-L", 4),rep("VU-M", 4))
data_set <- rep(c("dbpedia","moreno_crime","opsahl-ucforum","wang-amazon") , 9)
value <- c(8756,3,31,9,596,3,64,8,577,2,23,8,86414,3,1660,21,142232,3,11,8,1591,2,22,8,13609,2,129,9,3828,2,12,11,21583,2,92,11)
exp_data <- data.frame(exp_type,data_set,value)

abs(rnorm(12 , 0 , 15))
value

plot <- ggplot(exp_data, aes(fill=data_set, y=value, x=exp_type)) + geom_bar(position="dodge", stat="identity") + scale_y_continuous(trans='log10', labels = scales::comma) + scale_fill_discrete(name="Data Set")
plot + labs (x = "Experiments", y = "Time Secs (Log10 Scale)")

# levels(exp_time) <- c("Edge-High","Edge-Low","Edge-Middle","Vertex-Lower-High","Vertex-Lower-Low","Vertex-Lower-Medium","Vertex-Upper-High","Vertex-Upper-Low","Vertex-Upper-Medium")

# barplot(exp_time, main = "Execution Time in seconds", log="y", col = rainbow(4))
