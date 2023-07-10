library(SummarizedExperiment)
data(airway, package="airway")

library(eclust)
ee <- eclust(airway)
tree <- as.treedata(ee)

library(ggtree)
ggtree(tree) + geom_tiplab(aes(color=dex)) + hexpand(.2)

