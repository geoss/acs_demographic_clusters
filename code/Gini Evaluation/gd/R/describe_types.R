describe_types<-function(cluster_lookup,data_set,spec_file=NULL,new_data_frame=FALSE,heatmap=TRUE,bar=TRUE){
  described<-list()
  options(warn=-1)
  library(graphics)
  library(gplots)
  if(new_data_frame){
  d1<-input_geodemographic(data_set,spec_file)
  }
  
  else{
    d1<-data_set
  }
  if((!is.vector(cluster_lookup))&(!is.data.frame(cluster_lookup))){
    clusters<-read.csv(cluster_lookup)
  }
    else{
      clusters<-cluster_lookup
    }
  v<-as.vector(seq(1:nrow(d1)))
  clusters<-cbind(v,clusters)
  b<-clusters[,2]
  non_count_name<-c()
  d1<-cbind(d1,b)
  mat<-matrix(nrow=max(b),ncol=0)
  vec<-colnames(d1)
  for(k in 1:(length(vec)-1)){
    d2<-as.vector(by(d1[,k],d1$b,sum))
    mat<-cbind(mat,d2)
  }
  
  l<-length(vec)
  
  d1[,l]<-NULL
  num_clust<-max(b)
  colnames(mat)<-colnames(d1)

  q1<-matrix(nrow=num_clust,ncol=0)
  count<-0
  count2<-0
  nc<-list()
  for(i in 1:ncol(d1)){
    
    if(attr(d1[,i],"non_count?")){
      count2<-count2+1
   
      Names<-colnames(d1)
      nc1<-mat[,i]
      non<-Names[i]
       
      nc[[count2]]<-nc1
      non_count_name[count2]<-non
      
    }    
    if((!attr(d1[,i],"is_denom?"))&(attr(d1[,i],"denominator")!="default")){
      a<-d1[,i]
      denominator<-attr(d1[,i],"denominator")
      Names<-(colnames(d1))
      j<-match(denominator,Names)
      
      numerator<-Names[i]
      count<-count+1
      divisor<-mat[,j]
      dividend<-mat[,i]
      dividend<-dividend+1
      divisor<-divisor+1
      
      r<-sum(mat[,j])
      s<-sum(mat[,i])
      nat_avg<-(s/r)
      
      div<-(dividend/divisor)
      w<- (div/nat_avg)*100
      
      q1<-cbind(q1,w)
      colnames(q1)[count]<-numerator
    }
  }
  a1<-c(0,0,0,0,0,0)
  a2<-c(0,1,1,1,1,0)
  mat<-cbind(a1,a2)
  mat<-cbind(mat,a2)
  mat<-cbind(mat,a2)
  mat<-cbind(mat,a2)
  mat<-cbind(mat,a1)
  layout(mat)
  if(length(nc)>0){
    if (bar == TRUE) {
      dev.new()
      plotlist<-list()
  for(i in 1:length(nc)){
    dev.new(width=5, height=4)
   
    temp2<-rep("steelblue",num_clust)
    title<-non_count_name[i]
    clus_no<-as.vector(1:max(clusters[,2]))
    
    AVG_NC<-nc[[i]]

    ggplot_data<-data.frame(cbind(clus_no,AVG_NC))
    plot<-ggplot(ggplot_data, aes(clus_no, AVG_NC)) + 
      geom_bar(stat = "identity", fill="steelblue",position="identity") + 
      theme(axis.text.x=element_text(angle=-90,hjust=0,vjust=0.5)) + 
      scale_y_continuous("Average Non Count Value") +
      scale_x_discrete("Cluster") +
      ggtitle(title)
  plotlist[[i]]<-plot
  }
      if(is.object(plotlist)){
        described[["noncounts"]]<-plotlist
  }
  }
}


  if (heatmap == TRUE) {
    
    breaks <- c(0,40,80,120,160,200)
    myCol <- c("seagreen","darkseagreen","lightgrey","lightsteelblue","steelblue")
      
    described[["counts"]]<-heatmap.2(q1,Rowv=FALSE,Colv=FALSE,dendrogram="none",key=TRUE,cexRow=0.6,cexCol=0.6,margins = c(8, 8),
             breaks = breaks, col= myCol,density.info="none",trace="none", lhei = c(0.13,0.65),lwid = c(0.2,0.65),
        colsep=c(1:ncol(q1)),rowsep=c(1:num_clust),sepwidth=c(0.001,0.15),sepcolor="white")
#legend("topleft",bty="n",c("Index < 50","50 < Index <= 80","80 < Index <= 120","120 < Index <= 150","Index > 150"),fill=c("seagreen","darkseagreen","lightgrey","lightsteelblue","steelblue"),xpd=NA,border = FALSE)
  }  
  
  
  
  
  if(length(nc)>0) {
  result<-list(q1,nc)
  result
  }
  else{
    result<-q1
  }
  result
    
  assign("q1",q1,.GlobalEnv)  
  dev.new()
  
  
  class(described)="describe"
  described
}