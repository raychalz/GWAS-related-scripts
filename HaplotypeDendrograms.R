#haplotype dendrograms - try to use heatmap to show haplotypes across 96 accessions within a gene

# setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Haplotype Dendrograms")
# data <- read.csv("JulinsAcc3_MAF20_RFF.csv")
# 
# library(tidyr)
# data2 <- separate(data, SNP, into = c("chr", "pos"))
# write.csv(data2, "JulinsAcc3_MAF20_RFF_sep.csv") #this is separated into chr and pos
# 


library(gplots)
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Haplotype Dendrograms/genes")
snp <- read.csv("SNPs.at4g17010.csv")
require(graphics); require(grDevices)
x <- as.matrix(data.frame(lapply(snp[,3:97], as.numeric), stringsAsFactors=FALSE))
rownames(x) <- as.character( snp[,2])

heatmap.2(x,
          scale="none",
          distfun = function(d) dist(d, method = "euclidean"),
          hclustfun = function(d) hclust(d, method =  "ward"),
          col = cm.colors(2),
          Rowv = NA, 
          dendrogram = "col",
          margins=c(4,4.5),
          cexRow=0.5,
          cexCol=0.35,
          trace=c("none"),
          density.info=c("none"),
          main = "Haplotypes AT4G17010",
          xlab = "A. thaliana Accession",
          ylab= "SNP position in Chrom IV")
