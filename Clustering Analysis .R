library(cluster)
library(clusterCrit)
library(readxl)
library(data.table)
EastWestAirlines <- read_excel("EastWestAirlines.xls", 
                               col_types = c("blank", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric"))
View(EastWestAirlines)
EastWestAirlines.scaled=scale(EastWestAirlines)
view(EastWestAirlines.scaled)

#hierarchical clustering with Euclidean distance and complete linkage
air_dist=round(dist(EastWestAirlines.scaled, method = "euclidean", diag = FALSE, upper = FALSE),2)

air_complete=hclust(air_dist,method = "complete")
plot(air_complete,main="Complete Linkage", xlab="", cex=.9)


intCriteria(EastWestAirlines.scaled,cutree(air_complete,k=2),c("silhouette","dunn","calinski_harabasz","c_index"))
intCriteria(EastWestAirlines.scaled,cutree(air_complete,k=3),c("silhouette","dunn","calinski_harabasz","c_index"))
intCriteria(EastWestAirlines.scaled,cutree(air_complete,k=4),c("silhouette","dunn","calinski_harabasz","c_index"))
intCriteria(EastWestAirlines.scaled,cutree(air_complete,k=5),c("silhouette","dunn","calinski_harabasz","c_index"))

k3_clus=cutree(air_complete,k=3)

#Comparing the cluster centroids to characterize the different clusters 
clustered_3=data.table(EastWestAirlines,k3_clus)
sapply(clustered_3[k3_clus==1], mean, na.rm=TRUE)
sapply(clustered_3[k3_clus==2], mean, na.rm=TRUE)
sapply(clustered_3[k3_clus==3], mean, na.rm=TRUE)

#checking the stability of the clusters
set.seed(425)
air_samp=sample(1:3999,200,replace=FALSE)
air_new=EastWestAirlines[-air_samp,]

air_new_scaled=scale(air_new)
air_new_dist=round(dist(air_new_scaled, method = "euclidean", diag = FALSE, upper = FALSE),2)
air_new_comp=hclust(air_new_dist,method = "complete")
plot(air_new_comp,main="Complete Linkage", xlab="", cex=.9)
intCriteria(air_new_scaled,cutree(air_new_comp,k=3),c("silhouette","dunn","calinski_harabasz","c_index"))
plot(silhouette(cutree(air_new_comp, k=3),air_new_dist))

extCriteria(cutree(air_new_comp,k=3),k3_clus,"Rand")
extCriteria(cutree(air_new_comp,k=3),k3_clus,"Jaccard")

#k-means algorithm with the number of clusters specified by hierarchical clustering

air_k_means=kmeans(air_dist,centers = 3,nstart = 20)
air_k_means
extCriteria(air_k_means$cluster,k3_clus,"Rand")
extCriteria(air_k_means$cluster,k3_clus,"Jaccard")



