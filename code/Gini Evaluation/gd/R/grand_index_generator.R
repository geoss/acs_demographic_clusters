grand_index_generator<-function(clusters,data){
  if(!is.vector(clusters)){
    clusters<-clusters[,1]
  }
    data_after_input<-data
    b<-clusters
    k_value<-max(b)
    n_clust<-k_value
    data_after_input<-cbind(data_after_input,b)
    m<-matrix(nrow=n_clust,ncol=0)
    vec<-colnames(data_after_input)
    
    #convert the matrix to rows that represent clusters and not observations
    #similar to sql "group by"
    
    for(k in 1:(length(vec)-1)){
      d2<-as.vector(by(as.numeric(data_after_input[,k]),data_after_input$b,sum))
      m<-cbind(m,d2)
      
    }
    l<-length(vec)
    
    data_after_input[,l]<-NULL
    
    colnames(m)<-colnames(data_after_input)
    
    q<-matrix(nrow=nrow(m),ncol=0)
    count<-0
    numerators<-c()
    for(i in 1:ncol(data_after_input)){
      if(!(attr(data_after_input[,i],"is_denom?"))&(attr(data_after_input[,i],"denominator")!="default")){
        denominator<-attr(data_after_input[,i],"denominator")
        Names<-(colnames(data_after_input))
        numerator<-Names[i]
        count<-count+1
        numerators[count]<-numerator
        j<-match(denominator,Names)
        
        divisor<-m[,j]
        dividend<-m[,i]
        dividend<-dividend+1
        divisor<-divisor+1
        
        r<-sum(m[,j])
        s<-sum(m[,i])
        nat_avg<-(s/r)
        div<-(dividend/divisor)
        div<- (div/nat_avg)*100
        q<-cbind(q,div)
        
      }
    }
    colnames(q)<-numerators
     
  

q
}
