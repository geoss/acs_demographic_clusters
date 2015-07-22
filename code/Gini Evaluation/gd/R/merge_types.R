merge_types <-function(clusters,clusters_to_merge){
  options(warn=-1)
  max_c<-max(as.numeric(as.vector(clusters[,1])))
  
  for(j in 1:length(clusters_to_merge)){
    clus<-clusters_to_merge[j]
    

    d1.s<-subset(clusters,cluster==clus)
    
    vec<-row.names(d1.s)

    for(i in 1:length(vec)){
      rows<-rownames(clusters)
      
      aa<-match(vec[i],rows)
      n<-ncol(clusters)
      clusters[aa,n]<-max_c+1
    }
}
  clusters
}
