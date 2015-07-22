split_types <-
function(data,clusters,clusters_to_split,number_splits){
  options(warn=-1)
  
  
  d<-data
  cl<-clusters_to_split
  n<-ncol(data)
  d1<-cbind(d,clusters)
  count<-0
  
  for(j in 1:length(clusters_to_split)){
    clus<-clusters_to_split[j]

    num<-number_splits[j]
    d1.s<-subset(d1,clusters==clus)
    k<-kmeans(d1.s,num)
    c<-k$cluster

    d1.s<-cbind(d1.s,c)
    vec<-row.names(d1.s)
  
    
    for(i in 1:length(c)){
           
          
      rows<-rownames(data)
      
      aa<-match(vec[i],rows)


           nn<-ncol(d1.s)
           new<-d1.s[i,nn]
    

           d1[aa,n+1]<-(new+max(clusters)+count)

       } 
    count<-count+num 
  }
  temp<-ncol(d1)
  a<-d1[,temp]
  cc<-as.character(clusters_to_split)
  assign(paste("clusters_after_split"),a,envir=.GlobalEnv)
  
  
  assign(paste("splitted_data"),d1,envir=.GlobalEnv)
  
  a<-data.frame(a)
  rownames(a)<-rownames(data)
  a
}
