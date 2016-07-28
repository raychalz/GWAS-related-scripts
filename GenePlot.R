
library(ggplot2)

plot <- ggplot(data = trait_data, x = chromosome_coordinate, y = effect_size, aes(color = trait)) + geom_point() + geom_segment(data = gene_coordinates, y = 1, x = chromosome_coordinate)

plot <- ggplot(data = trait_data, x = 9575849, y = .0000101, aes(color = trait)) + geom_point() + geom_segment(data = gene_coordinates, y = 1, x = chromosome_coordinate)
#----------====================-------------------====================---------------------
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/GWASnew/Jason's data new/comparisons")
plot.data <- read.csv("plot.data.csv")
colors()[grep("olive",colors())]
plot.data.cesa2 <- subset(plot.data, gene ==  "CESA2-1")

b05.10 <- data.frame(bp = plot.data.cesa2$bp, Ylw.Lesion.Proportion_Apple517 = plot.data.cesa2$Ylw.Lesion.Proportion_B05.10)
supersteak <- data.frame(bp = plot.data.cesa2$bp, Ylw.Lesion.Proportion_Apple517 = plot.data.cesa2$Ylw.Lesion.Proportion_Supersteak)
ukrazz <- data.frame(bp = plot.data.cesa2$bp, Ylw.Lesion.Proportion_Apple517 = plot.data.cesa2$Ylw.Lesion.Proportion_UKRazz)
b <- ggplot(plot.data.cesa2, aes(bp, Ylw.Lesion.Proportion_Apple517, fill = Ylw.Lesion.Proportion_Apple517))  +
    geom_point( colour = "olivedrab") +
    geom_point(data = b05.10, colour = "goldenrod1") +
    geom_point(data = supersteak, colour = "darkred") +
    geom_point(data = ukrazz, colour = "cornflowerblue") +
    labs(y = "Effect Size", x = "Base Position") +
 
#---------------------   cesa2 ---------with gene regions

        gene.cesa2 <- read.csv("gene.cesa2.csv")
        library(tidyr)
        gene.cesa2 <- gene.cesa2 %>% 
            separate( pos, c('front', 'back'), '-')
        gene.cesa2$front <- 18296796 + as.numeric(gene.cesa2$front)
        gene.cesa2$back <- 18296796 + as.numeric(gene.cesa2$back)
        write.csv(gene.cesa2, "gene.cesa2.csv")
        #paste this into plot.data.csv - repeating to length of SNP list
        
plot.data <- read.csv("yellow.plot.data.csv")
plot.data.gene <- subset(plot.data, gene ==  "CESA2-1")

b <- ggplot(plot.data.gene, aes(bp, Effect.Size, color = Isolate))  +
    geom_point()+
    labs(y = "Effect Size", x = "Base Position in Chrom IV") +
    ggtitle("Effect Size of SNPs in CESA2-1 on Yellowness of Lesion") +
    theme(plot.title = element_text(size = 10)) +
    theme(axis.title = element_text(size = 10)) +
    geom_segment( aes(x = front[1:48], xend = back[1:48], y = 0, yend = 0), col = "blue", size=7, alpha = 0.05) +
    annotate("text", x = 18300000, y = -.000001, label = "coding region", color="blue", alpha = 0.35, size = 3) + #label the coding region
    geom_hline(yintercept = -.00000691996 , color = "coral") + #add threshold horizontal line
    annotate("text", x = 18301000, y = -.0000073, label = "95th percentile threshold", color="coral", size = 3) + #label the line
    #xlim(18295797, 18303192) +
    scale_x_continuous(breaks=seq(18295797, 18303192, 1000)) + #set breaks in x axis ticks
    #these numbers are Left: 5'utr - 1000 and right : 3'utr + 1000
    theme(axis.text.x = element_text(angle = 67, hjust = 1, size = 8)) # rotate x axis tick labels

b

    # --------      AT4G17010-1   --------------
plot.data <- read.csv("yellow.plot.data.csv")
plot.data.gene <- subset(plot.data, gene ==  "AT4G17010-1")   

b <- ggplot(plot.data.gene, aes(bp, Effect.Size, color = Isolate))  +
    geom_point()+
    labs(y = "Effect Size", x = "Base Position in Chrom IV") +
    ggtitle("Effect Size of SNPs in AT4G17010-1 on Yellowness of Lesion") +
    theme(plot.title = element_text(size = 10)) +
    theme(axis.title = element_text(size = 10)) +
      geom_segment( aes(x = front[1:40], xend = back[1:40], y = 0, yend = 0), col = "blue", size=7, alpha = 0.05) +
    annotate("text", x = 9577000, y = -.0000025, label = "coding region", color="blue", alpha = 0.35, size = 3) + #label the coding region
    geom_hline(yintercept = -.00000691996 , color = "coral") + #add threshold horizontal line
    annotate("text", x = 9577500, y = -.0000063, label = "95th percentile threshold", color="coral", size = 3) + #label the line
    geom_hline(yintercept = .00000691996 , color = "coral") + #add threshold horizontal line
    annotate("text", x = 9577500, y = .0000063, label = "95th percentile threshold", color="coral", size = 3) + #label the line
     scale_x_continuous(breaks=seq(9575000, 9579000, 1000)) + #set breaks in x axis ticks
    theme(axis.text.x = element_text(angle = 67, hjust = 1, size = 8)) # rotate x axis tick labels

b

# 18296796
# b + geom_segment(data = gene.cesa2, y = 0, x =  gene.cesa2$pos[1])




#----------====================-------------------




#----------====================-------------------====================---------------------







phenos <- read.csv("Les.LSmeans.csv")
colnames(phenos)
#Ylw.Lesion.Proportion.new 432, 433, 435, 436
#Lesion.Size.mm2 22,23,25,26
#Lesion.Grn.mm2 32, 33, 35, 36
#Lesion.0.m.eccentricity 72, 73, 75, 76
library(beanplot)
#==========================YELLOW====================================
beanplot(phenos$Ylw.Lesion.Proportion.new_Apple517, 
         phenos$Ylw.Lesion.Proportion.new_B05.10, 
         phenos$Ylw.Lesion.Proportion.new_Supersteak, 
         phenos$Ylw.Lesion.Proportion.new_UKRazz,
         names = c("Apple517", "B05.10", "Supersteak", "UKRazz"),
         #ex.axis = .8, 
         ylim = c(0.2, 0.8), 
        xlim = c(0.5, 4.5), 
         main = "Ylw.Lesion.Proportion",
         ylab = "Ylw.Lesion.Proportion",
         xlab = "Bc Genotypes",
         col = c("royalblue1", "aquamarine", "aquamarine"),
        border = "royalblue1", 
         log = "" 
        )
legend("top", bty="n", "Col0", pch = 8, col = "red")   

points(1, phenos[14,432], col = "red", pch = 8) #apple517 Col0
points(2, phenos[14,433], col = "red", pch = 8) #bo510
points(3, phenos[14,435], col = "red", pch = 8) #supersteak 
points(4, phenos[14,436], col = "red", pch = 8) #ukrazz

#==========================GREEN====================================

beanplot(phenos$Lesion.Grn.mm2_Apple517, 
         phenos$Lesion.Grn.mm2_B05.10, 
         phenos$Lesion.Grn.mm2_Supersteak, 
         phenos$Lesion.Grn.mm2_UKRazz,
         names = c("Apple517", "B05.10", "Supersteak", "UKRazz"),
         cex.axis = .8, 
         ylim = c(0, 4), 
         xlim = c(0.5, 4.5), 
         main = "Lesion.Grn.mm2",
         ylab = "Lesion.Grn.mm2",
         xlab = "Bc Genotypes",
         col = c("royalblue1", "aquamarine", "aquamarine"),
         border = "royalblue1", 
         log = "" 
)
legend("top", bty="n", "Col0", pch = 8, col = "red")   

points(1, phenos[14,32], col = "red", pch = 8) #apple517 Col0
points(2, phenos[14,33], col = "red", pch = 8) #bo510
points(3, phenos[14,35], col = "red", pch = 8) #supersteak 
points(4, phenos[14,36], col = "red", pch = 8) #ukrazz

#==========================SIZE====================================


beanplot(phenos$Lesion.Size.mm2_Apple517, 
         phenos$Lesion.Size.mm2_B05.10, 
         phenos$Lesion.Size.mm2_Supersteak, 
         phenos$Lesion.Size.mm2_UKRazz,
         names = c("Apple517", "B05.10", "Supersteak", "UKRazz"),
         cex.axis = .8, 
         ylim = c(7, 70), 
         xlim = c(0.5, 4.5), 
         main = "Lesion.Size.mm2",
         ylab = "Lesion.Size.mm2",
         xlab = "Bc Genotypes",
         col = c("royalblue1", "aquamarine", "aquamarine"),
         border = "royalblue1", 
         log = "" 
)
legend("top", bty="n", "Col0", pch = 8, col = "red")   

points(1, phenos[14,22], col = "red", pch = 8) #apple517 Col0
points(2, phenos[14,23], col = "red", pch = 8) #bo510
points(3, phenos[14,25], col = "red", pch = 8) #supersteak 
points(4, phenos[14,26], col = "red", pch = 8) #ukrazz

#==========================ECCENTRICITY====================================


beanplot(phenos$Lesion.0.m.eccentricity_Apple517, 
         phenos$Lesion.0.m.eccentricity_B05.10, 
         phenos$Lesion.0.m.eccentricity_Supersteak, 
         phenos$Lesion.0.m.eccentricity_UKRazz,
         names = c("Apple517", "B05.10", "Supersteak", "UKRazz"),
         cex.axis = .8, 
         ylim = c(0.3, 0.8), 
         xlim = c(0.5, 4.5), 
         main = "Lesion.0.m.eccentricity",
         ylab = "Lesion.0.m.eccentricity",
         xlab = "Bc Genotypes",
         col = c("royalblue1", "aquamarine", "aquamarine"),
         border = "royalblue1", 
         log = "" 
)
legend("top", bty="n", "Col0", pch = 8, col = "red")   

points(1, phenos[14,72], col = "red", pch = 8) #apple517 Col0
points(2, phenos[14,73], col = "red", pch = 8) #bo510
points(3, phenos[14,75], col = "red", pch = 8) #supersteak 
points(4, phenos[14,76], col = "red", pch = 8) #ukrazz