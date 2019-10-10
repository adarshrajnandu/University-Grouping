Uni <- read.csv(file = "G:\\Classes\\Clustering\\Universities.csv")

# Normalizing continuous columns to bring them under same scale
normalized_data<-scale(Uni[,2:7]) #excluding the university name columnbefore normalizing

d <- dist(normalized_data, method = "euclidean") # distance matrix

fit <- hclust(d, method="complete")

plot(fit) # display dendrogram
plot(fit, hang=-1,labels = Uni$Univ)

library(NbClust)
NbClust::NbClust(data =Uni[,-1],min.nc = 2,max.nc = 10,method = "average")

?cutree
rect.hclust(fit, k=5, border="red")

groups <- cutree(fit, k=5) # cut tree into 5 clusters

membership<-as.matrix(groups) # groups or cluster numbers
final <- data.frame(Uni, membership)

View(final)

write.csv(final, file="final.csv",row.names = F)

aggregate(Uni[,-1],by=list(final$membership),median)

