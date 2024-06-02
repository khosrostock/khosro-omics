################# CloudPlot ###############
This plot superimpose the Featureplot of different genes

data: the Seurat object datasets
gene.list: list of your desired genes
alpha: transparency of labels, by default alpha = 0.03
size: size of labels, by default size = 2
col: colour of labels, by default col = "blue"


#```{r, CloudPlot}
# ----------------------------------

CloudPlot <- function(data, gene.list, alpha = 0.03, size = 2, col = "blue") {
  subset.matrix <- data[gene.list,]
  subset.matrix2 <- subset.matrix[, subset.matrix$nCount_RNA > 1] 
  
  Idents(subset.matrix2) <- rownames(subset.matrix2)
  #DimPlot(subset.matrix2)
  
  # extract data of dimplot
  umap.genes <- DimPlot(subset.matrix2)
  
  # extract umap data
  umap.genes <- umap.genes$data
  
  print(table(umap.genes$ident))
  
  
  # stroke=NA remove border around points
  # alpha control transparency of the points
  ggplot(umap.genes, aes(fill=ident, y=UMAP_2, x=UMAP_1)) + #, color=ident
    geom_point(color=col, alpha=alpha, size=size, stroke=NA) +
    scale_fill_discrete(name = "Genes") 
  
  
  
}

# asc.manon <- c("Gpx3", "Abca8a", "Abca8b", "Mgp", "Cilp", "Cd55", "Pi1")

# CloudPlot(data = data.4m, gene.list = asc.manon)
#```
