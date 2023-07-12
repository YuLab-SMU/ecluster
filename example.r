library(SummarizedExperiment)
data(airway, package="airway")

library(eclust)
ee <- eclust(airway)

tree1 <- as.treedata(ee, longer = FALSE)
tree2 <- as.treedata(ee, longer = TRUE)

ggtree(tree1) + geom_tiplab(aes(color=dex)) + hexpand(.2)

