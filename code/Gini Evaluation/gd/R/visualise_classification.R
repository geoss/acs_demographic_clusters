visualise_classification<-function(cluster_lookup,data_set,spec_file=NULL,new_data_frame=FALSE,variables,colours=NULL){
  options(warn=-1)
  library(graphics)
  library(gplots)
  library(RColorBrewer)
  library(scales)
  if(new_data_frame){
    d1<-input_geodemographic(data_set,spec_file)
  }
  
  else{
    d1<-data_set
  }
  bc<-c()
  if((!is.vector(cluster_lookup))&(!is.data.frame(cluster_lookup))){
    clusters<-read.csv(cluster_lookup)
  }
  else{
    clusters<-cluster_lookup
  }
  clusters<-as.character(cluster_lookup[,3])
  for(i in 1:length(clusters)){
    clusters[i]<-gsub("[A-Z]", "", clusters[i])
  }
  types<-clusters
  clusters<-as.character(cluster_lookup[,2])
  for(i in 1:length(clusters)){
    temp<-clusters[i]
    if(nchar(temp)==1){
      
      bc[i]<-match(temp,LETTERS)
    }
    if(nchar(temp)>1){
      temp<-substr(temp,1,1)
      
      bc[i]<-match(temp,LETTERS)
    }
  }
  #CALCULATIONS
  groups<-as.numeric(bc)
  b<-as.numeric(types)
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
  data<-d1
  for(i in 1:(ncol(d1)-1)){
    
    if(attr(data[,i],"non_count?")){
      count2<-count2+1
      Names<-colnames(data)
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
  
  second_clusters<-groups
  Clustering_1_Result<-types
  Clustering_2_Result<-groups
  table<-cbind(Clustering_1_Result,Clustering_2_Result)
  
  table<-data.frame(table)
  
  t<-by(table[,2],table$Clustering_1_Result,one)
  
  second_clust<-c()
  for(i in 1:length(t)){
    a<-as.numeric(t[[i]])
    
    
    second_clust[i]<-a[1]
  }
  
  second_clusters<-sort(second_clust)
  
  
  #COLOURS
  colour<-c()
  if(is.null(colours)){
    
    colour_vector<-c()
    
    colour_vector[1:12]<-brewer.pal(12,"Set3")
    
    if(max(second_clusters)>12){
      rem<-max(second_clusters)-12
      rand<-runif((max(second_clusters)-12),1,442)
      for(i in 1:length(rand)){
        colors<-as.vector(colors())
        a<-rand[i]
        colour_vector[12+i]<-colors[a]
      }
    }
    for(i in 1:length(second_clusters)){
      a<-second_clusters[i]
      colour[i]<-colour_vector[a]
    }
  }
  else {colour=colours}
  
  
  
  c<-ncol(q1)
  q1<-cbind(q1,second_clusters)
  cols<-c()
  for(i in 1:num_clust){
    cd<-match(i,types)
    
    cols[i]<-cd
  }
  
  
  cols<-na.omit(cols)
  
  
  
  #PLOTTING
  
  
  fours<-length(variables)
  
  remainder<-(fours%%4)
  
  fullsets<- floor(fours/4)
  
  if(fullsets==0){
    fullsets=fullsets+1
  }
  jeff<-1
  multiples<-0
  v<-as.vector(seq(1:max(second_clusters)))
  col_vec<-c()
  for(i in 1:length(second_clusters)){
    a<-second_clusters[i]
    b<-colours[i]
    col_vec[a]<-b
  }
  groups_letters<-c()
  for(i in 1:length(v)){
    a<-v[i]
    b<-LETTERS[a]
    groups_letters[i]<-b
  }
  plotlist<-list()
  for(i in 1:ncol(q1)){
    CLUSTERS<-as.vector(1:nrow(q1))
    title=variables[i]
    AVG_IND_SCORE<-q1[,i]
    AVG_IND_SCORE<-AVG_IND_SCORE/100 -1
    ggplot_data<-data.frame(cbind(CLUSTERS,AVG_IND_SCORE))
    plot<-ggplot(ggplot_data, aes(CLUSTERS, AVG_IND_SCORE)) + 
      geom_bar(stat = "identity", fill=colour,position="identity") + 
      theme(axis.text.x=element_text(angle=-90,hjust=0,vjust=0.5)) + 
      geom_text(aes(label = paste(round(AVG_IND_SCORE * 100,digits = 0), "%"), vjust = ifelse(AVG_IND_SCORE >= 0, -0.5, 1.5)), size=3) +
      scale_y_continuous("Average Index Score For Clusters",labels = percent_format()) +
      scale_x_discrete("Cluster") +
      ggtitle(title)
    plotlist[[title]]<-assign(paste("plot_",i,sep=""),plot)
  }
  if(length(variables)>4){
    while(jeff<=fullsets){
      arrange_ggplot2(plotlist[[multiples+1]],plotlist[[multiples+2]],plotlist[[multiples+3]],plotlist[[multiples+4]])
      multiples<-multiples+4
      jeff<-jeff+1
      dev.new()
    }
    
    if(remainder>0){
      if(remainder==1){
        plot(plotlist[[multiples+1]])
      }
      if(remainder==2){
        arrange_ggplot2(plotlist[[multiples+1]],plotlist[[multiples+2]])
      }
      if(remainder==3){
        arrange_ggplot2(plotlist[[multiples+1]],plotlist[[multiples+2]],plotlist[[multiples+3]])
      }
    }
  }
  else{
    if(length(variables)==3){
      arrange_ggplot2(plotlist[[multiples+1]],plotlist[[multiples+2]],plotlist[[multiples+3]])
      
    }
    if(length(variables)==2){
      arrange_ggplot2(plotlist[[multiples+1]],plotlist[[multiples+2]])
    }
    if(length(variables)==1){
      plot(plotlist[[multiples+1]])
    }
  }
  dev.new()
  t.q1<-t(q1)
  results<-list()
  results[["dataframe"]]<-q1
  results[["graphs"]]<-plotlist
  class(results)="visualise"
  results
}
