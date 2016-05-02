library(DataComputing)
library(igraph)

nodes <- read.csv("/Users/kenchen/MDST/visual_reddit/extra_files/Data/Dataset1-Media-Example-NODES.csv", header=T, as.is=T)
links <- read.csv("/Users/kenchen/MDST/visual_reddit/extra_files/Data/Dataset1-Media-Example-EDGES.csv", header=T, as.is=T)

head(nodes)
head(links)
nrow(nodes); length(unique(nodes$id))
nrow(links); nrow(unique(links[,c("from", "to")]))

net <- graph.data.frame(links, nodes, directed=T)
net
net <- simplify(net, remove.multiple = F, remove.loops = T) 
plot(net, edge.arrow.size=.4,vertex.label=NA)