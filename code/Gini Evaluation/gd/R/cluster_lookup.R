cluster_lookup <-function(results){
  options(warn=-1)
  group<-c()
  type<-c()
  r<-nrow(results)
  final_table<-matrix(nrow=r,ncol=0)
  for(i in 1:nrow(results)){
    a<-results[i,2]
    if(a<=26)
      group[i]<-LETTERS[a]
    if((a>26)&(a<=52)){
      a<-a-26
      temp<-paste(LETTERS[a],LETTERS[a],sep="")
      group[i]<-temp
    }
    if((a>52)&(a<=78)){
      a<-a-52
      temp<-paste(LETTERS[a],LETTERS[a],LETTERS[a],sep="")
      group[i]<-temp
    }
    
  }
  for(i in 1:length(group)){
    temp<-paste(group[i],results[i,1],sep="")
    type[i]<-temp
  }
  
  c<-as.vector(rownames(results))
  
  final_table<-cbind(final_table,c)
  
  final_table<-cbind(final_table,group)
  final_table<-cbind(final_table,type)
  
  
  
  final_table<-data.frame(final_table)
  names(final_table)[1]<-"Zone Code"
  names(final_table)[2]<-"Group"
  names(final_table)[3]<-"Type"
  final_table
}