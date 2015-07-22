cluster_types<-function(standardized_data,k_values=60,n_iter=100){
  options(warn=-1)
  
  SS<-c()
  run<-c()
  count<-0
  
  
  k_means_list<-list()
  best_runs<-list()
  
  
  SSlist<-list()
  for(hog in 1:length(k_values)){
    cluster_number<-k_values[hog]
    
    cat("\n")
    cat("Running kmeans for k= ")
    cat(cluster_number)
    cat("\n")
    
    total <- n_iter
    # create progress bar
    pb <- txtProgressBar(min = 0, max = total, style = 3)
    
    n_clust<-k_values[hog]
    
    for(i in 1:n_iter){
      
      Sys.sleep(0.1)
      # update progress bar
      setTxtProgressBar(pb, i)
      
      a<-kmeans(standardized_data,n_clust,60)
      assign(paste("K_Means_For_K_",n_clust,sep=""),a,envir=.GlobalEnv)
      
      #find the total within cluster sum of squares, compare to previous result, keep the best
      #dump best clusters and best centers to workspace
      #store best result for each k in a list
      
      SS[i]<-a$tot.withinss
      
      if(i==1){
        best<-SS[1]
        cluster_number<-k_values[hog]
        k_means_list[[hog]]<-a
        cluster<-a$cluster
        aa<-data.frame(cluster)
        rownames(aa)<-rownames(standardized_data)
        aaa1<-a$centers
        
        assign(paste("Clusters_for_K_",cluster_number,sep=""),aa,envir=.GlobalEnv)
        assign(paste("Centers_for_K_",cluster_number,sep=""),aaa1,envir=.GlobalEnv)
      }
      if(i>1){
        
        if(SS[i]<=best){
          
          best<-SS[i]
          cluster_number<-k_values[hog]
          k_means_list[[hog]]<-a
          cluster<-a$cluster
          aa<-data.frame(cluster)
          rownames(aa)<-rownames(standardized_data)
          aaa1<-a$centers
          assign(paste("Clusters_for_K_",cluster_number,sep=""),aa,envir=.GlobalEnv)
          assign(paste("Centers_for_K_",cluster_number,sep=""),aaa1,envir=.GlobalEnv)
          
        }
      }
      
     
      
    }
    
    SSlist[[hog]]<-SS
  }
  
  #print the cluster sizes to the console/terminal
  
  for(i in 1:length(k_means_list)){
    a<-k_values[i]
    ktemp<-k_means_list[[i]]
    cat("\n","Size of clusters for K=",a,"\n",ktemp$size,"\n")
  }
  
  K<-c()
  TWSS<-c()
  Run<-c()
  for(i in 1:length(k_values)){
    a<-as.vector(rep(k_values[i],n_iter))
    b<-SSlist[[i]]
    K<-c(K,a)
    TWSS<-c(TWSS,b)
    c<-as.vector(seq(1:n_iter))
    Run<-c(Run,c)
  }

TWSS_data<-cbind(K,Run)
  TWSS_data<-data.frame(cbind(TWSS_data,TWSS))
#   TWSS_data<-TWSS_data[order(TWSS_data$TWSS),]
  len<-length(k_means_list)
  k_means_list[[len+1]]<-TWSS_data
 k_means_list
}