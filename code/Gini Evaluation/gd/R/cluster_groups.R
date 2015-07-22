cluster_groups<-function(clusters,data,cutoff,new_data_frame=FALSE,specification=NULL){
  options(warn=-1)
  if(new_data_frame){
 
  data<-input_geodemographic(data,specification)
  }
  
  cl<-standardise(clusters,data)
  if((!is.vector(clusters))&(is.data.frame(clusters))){
    clusters<-clusters[,1]
  }
  n_clust<-nrow(cl)
  cl<-dist(cl)
  cl<-hclust(cl,"ward")
  plot(cl,xlab='Types and Groups')
  groups<-cutree(cl,k=cutoff)
  rect.hclust(cl,k=cutoff,border="darkblue")
  clusters_1<-data.frame(clusters)
  clusters_2<-groups
  
  results<-c()
  r<-nrow(clusters_1)
  for(i in 1:r){
    a<-clusters_1[i,1]
    results[i]<-(clusters_2[a])
  }
  results<-data.frame(results)
  results<-cbind(clusters_1,results)
  
  names(results)<-c("Clustering_1_Result","Clustering_2_Result")
  se<-seq(1:n_clust)
  safe<-cbind(se,groups)
  
  rows<-rownames(data)
  rownames(results)<-rows
  results
  
}