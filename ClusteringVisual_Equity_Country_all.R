## Testing with CountryData

# Packages needed
# TSclust
# igraph
# MASS
# TTR
install.packages("TSclust")
install.packages("igraph")
install.packages("TTR")
install.packages("MASS")
install.packages("ape")
require(TSclust)
require(igraph)
require(TTR)
require(MASS)
require(ape)

#set output folder
setwd("/Users/yuyanlin/Desktop/Data Science/Insight/Demo/Clustering Analysis/Results")

# Import equity index price data with missing data, align USD/CAD exchange rate to the last column of the country data
CountryDataUSD <- read.delim("/Users/yuyanlin/Desktop/Data Science/Insight/Demo/Clustering Analysis/Production Data/CountryData_USD_2006.txt")
CountryDataLocal<-read.delim("/Users/yuyanlin/Desktop/Data Science/Insight/Demo/Clustering Analysis/Production Data/CountryData_Local_2006.txt")
RegionData <- read.delim("/Users/yuyanlin/Desktop/Data Science/Insight/Demo/Clustering Analysis/Production Data/Country Region Label.txt")


# column names for input to Dendogram in PDC library
labs <- colnames(CountryDataLocal[])
RegionLabel = RegionData[,2]


# listwise deletion of missing data
CountryDataUSD2 <- na.omit(CountryDataUSD)
CountryDataLocal2 <- na.omit(CountryDataLocal)

# Convert USD data to CAD
CountryDataCAD <- CountryDataUSD2[, 1:ncol(CountryDataUSD2)-1]*CountryDataUSD2[, ncol(CountryDataUSD2)]

# Convert data to numeric
CountryDataCAD <- data.matrix(CountryDataCAD)
CountryDataLocal2 <- data.matrix(CountryDataLocal2)
RegionLabel <-data.matrix(RegionLabel)

# Delete Frontier Markets
CountryDataCAD= cbind(CountryDataCAD[,1:44]) #CountryDataCAD[,49:51])
CountryDataLocal2 = cbind(CountryDataLocal2[,1:44]) #CountryDataLocal2[,49:51])
RegionLabel = c(RegionLabel[1:44,]) #RegionLabel[49:51,])

# Convert Daily Data to Weekly and Monthly
iDaily2 = row(as.matrix(CountryDataCAD[,1]), as.factor = FALSE)
iWeekly = seq(1,length(iDaily2),5)
iMonthly = seq(1, length(iDaily2), 21)
CountryDataCADweekly <- CountryDataCAD[c(iWeekly),]
CountryDataCADmonthly <- CountryDataCAD[c(iMonthly),]
CountryDataLocal2weekly <- CountryDataLocal2[c(iWeekly),]
CountryDataLocal2monthly <- CountryDataLocal2[c(iMonthly),]

#Log Return

DailyCadReturns  = diff(log(CountryDataCAD), lag=1)   #1
DailyLocalReturns = diff(log(CountryDataLocal2), lag=1)   #2
WeeklyCadReturns = diff(log(CountryDataCADweekly), lag=1)  #3
WeeklyLocalReturns = diff(log(CountryDataLocal2weekly), lag=1) #4
MonthlyCadReturns = diff(log(CountryDataCADmonthly), lag=1) #5
MonthlyLocalReturns = diff(log(CountryDataLocal2monthly), lag=1) #6

# measures: "EUCL","CORT","COR","ACF","PACF","PER","DWT","AR.PIC","PDC","CID","CDM","NCD"
# distance measure

dis1<-diss(t(DailyCadReturns), "CORT")
dis2<-diss(t(DailyLocalReturns), "CORT")
dis3<-diss(t(WeeklyCadReturns), "CORT")
dis4<-diss(t(WeeklyLocalReturns), "CORT")
dis5<-diss(t(MonthlyCadReturns), "CORT")
dis6<-diss(t(MonthlyLocalReturns), "CORT")

dis<-dis4

# converts the distance measure from a dist object to a matrix
matrix_dis <- as.matrix(dis)

# converts the distance matrix to a graph object that is undireceted
g<-graph.adjacency(matrix_dis, mode="undirected", weighted=TRUE)

# colour country by region
V(g)[grepl("Americas.DM", RegionLabel)]$color = "darkred"
V(g)[grepl("Americas.EM", RegionLabel)]$color = "darkorange1"
#V(g)[grepl("Americas.Frontier", RegionLabel)]$color = "gold1"

V(g)[grepl("EMEA.DM", RegionLabel)]$color = "darkblue"
V(g)[grepl("EMEA.EM", RegionLabel)]$color = "deepskyblue"
#V(g)[grepl("EMEA.Frontier", RegionLabel)]$color = "cyan"

V(g)[grepl("Asia.Pacific.DM", RegionLabel)]$color = "darkgreen"
V(g)[grepl("Asia.Pacific.EM", RegionLabel)]$color = "olivedrab2"
#V(g)[grepl("Asia.Pacific.Frontier", RegionLabel)]$color = "green"


# recudes the label size to allow for clearer graph
V(g)$label.cex = 1   #0.35
E(g)$label.cex = 1    #0.6

# removes arrow head from edges
E(g)$arrow.mode = "-"

# reduces the vertex label name to 3 characters
# V(g)$name = labs2
V(g)$label.font =1        #2
V(g)$frame.color ="grey"
E(g)$frame.color ="grey"

# loads the market cap of the securities
#MCsize = runif(452, 0, 3)*2

# selects
#n <- length(MCsize)
#label_min <- sort(MCsize)[n-20]

# calculates the coordinates of the tree so all distances are relevant
sammon_mapping <- sammon(dis, niter=100, k =2)

# plots minimum spanning tree
min_span_tree = minimum.spanning.tree(g, weights=dis, algorithm="prim")

# plot(minimum.spanning.tree(g, weights=dis, algorithm="prim"), vertex.size=3, layout = sammon_mapping$points,xlim = c(-0.4, 0.4), ylim = c(-0.8, 0.8), vertex.label.dist=0.05)
pdf("MST_Weekly_Local.pdf", width=27.5, height=20) #saved under C, directory can be found using getwd()
plot(min_span_tree, vertex.size=4, layout = sammon_mapping$points, vertex.label.dist=0.3)

axis(1)



#plot(min_span_tree, vertex.size=5, layout = sammon_mapping$points, vertex.label.dist=0.05, edge.label = round(E(min_span_tree)$weight, digits=2))

# adds legend
leg.txt <- c("Americas DM", "Americas EM",  "EMEA DM", "EMEA EM",  "Asia Pacific DM", "Asia Pacific EM")
leg.fill<- c("darkred", "darkorange1","darkblue", "deepskyblue", "darkgreen", "olivedrab2") 
legend(1.1,1.1, leg.txt, leg.fill, bty="n", text.width=2, cex=2)

dev.off()

# plotting sammon mapping

clust <- hclust(dis, method="ward.D")
colors = c(Americas.DM= "darkred", Americas.EM="darkorange1", Asia.Pacific.DM = "darkgreen", Asia.Pacific.EM = "olivedrab2", EMEA.DM ="darkblue",EMEA.EM = "deepskyblue")
pdf("UFO_Weekly_Local.pdf", width=20, height=20) #saved under C, directory can be found using getwd()
plot(as.phylo(clust), type = "fan",   label.offset = 0, tip.color = colors[RegionLabel],cex =2,font=2)


title(main="UFO Dendrogram - COR & WARD",cex.main=3)
leg.txt <- c("Americas DM", "Americas EM", "EMEA DM", "EMEA EM", "Asia Pacific DM", "Asia Pacific EM")
leg.fill<- c("darkred", "darkorange1","darkblue", "deepskyblue", "darkgreen", "olivedrab2") 
legend(0,0, leg.txt, leg.fill, bty="n", text.width=0.5, cex=2)
dev.off()

#alternative is to use:png("plotdendogram.png",width=1500,height=1500), but not good resolution, keep for record





#stop here for demo________________________________









# Create correlation heatmap for the raw dataset
library(ggplot2)
library(plyr)
library(reshape2)
library(scales)
library(RColorBrewer)

cor2<-cor(CountryDataCADweekly)
cor2<-as.data.frame(cor2)
cor2<-data.frame(row=rownames(cor2), cor2)
rownames(cor2) <-NULL
cor2<- melt(cor2)

cor2$value<-cut(cor2$value,breaks=c(-1,-0.75,-0.5,-0.25,0,0.25,0.5,0.75,1),include.lowest=TRUE,label=c("(-0.75,-1)","(-0.5,-0.75)","(-0.25,-0.5)","(0,-0.25)","(0,0.25)","(0.25,0.5)","(0.5,0.75)","(0.75,1)")) # this can be customized to put the correlations in categories using the "cut" function with appropriate labels to show them in the legend, this column now would be discrete and not continuous
cor2$row <- factor(cor2$row, levels=rev(unique(as.character(cor2$variable)))) # reorder the "row" column which would be used as the x axis in the plot after converting it to a factor and ordered now
#po.nopanel <- list(opts(panel.background=theme_blank(),panel.grid.minor=theme_blank(),panel.grid.major=theme_blank())) # useful to get rid of grids of plot taken from https://gist.github.com/1035189/ac763cb4480c7b522483fa90ed0865d66593737c
#now plotting
ggplot(cor2, aes(row, variable)) +
  geom_tile(aes(fill=value),colour="black") +
  scale_fill_brewer(palette = "RdYlGn",name="Correlation")














# Adjusted Rand index to compare the clustering through time with a benchmark clustering, in this example, it is the TP labels

library(zoo)

viewresults2<-rollapply(data = WeeklyLocalReturns, width =260, FUN = getRollingClusters, by.column=FALSE)


TPlabel <- read.delim("R:/Portfolio Management/Portfolio Strategy/Research Projects/Clustering Analysis - Investment Universe/ProductionData/testARI2.txt")
TPlabel <- TPlabel[1:44,2]   #3 is TP labels, 2 is MSCI labels

adjustedRandIndex2 <- function(data)
{
  y <- t(TPlabel)
  ARI <- adjustedRandIndex(data, y)
  return(ARI)
}


ARItest <- rollapply(data = viewresults2, width =1, FUN = adjustedRandIndex2,  by.column = FALSE)
plot(ARItest, type="o", col="darkred")



######Construct the similarity matrix, comparing over time changes in the Adjusted Rand Index

ARImatrix <- matrix(0, nrow(viewresults2), nrow(viewresults2))

adjustedRandIndex2 <- function(data)
{
  ARI <- adjustedRandIndex(data, y)
  return(ARI)
}



for (i in 1:nrow(viewresults2))
{
  y <- t(as.matrix(viewresults2[i,]))
  
  ARImatrix [, i] <-  as.matrix(rollapply(data =  viewresults2, width =1, FUN = adjustedRandIndex2,  by.column = FALSE))
}   


return(ARImatrix)   
