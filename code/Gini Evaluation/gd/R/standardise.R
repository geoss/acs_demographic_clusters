standardise <-
function(clusters,data_after_input){
  options(warn=-1)
  if(!is.vector(clusters)){
    clusters<-clusters[,1]
  }
  b<-clusters
  k_values<-max(b)
  n_clust<-k_values
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
  
  assign(paste("Clustered_data_for_K_",k_values,sep=""),m,envir=.GlobalEnv)
  
  q<-matrix(nrow=n_clust,ncol=0)
  count<-0
  
  #now re-transform the data as specified in the spec file
  
  for(i in 1:ncol(data_after_input)){
    if(!is.null(attr(data_after_input[,i],"non_count?"))){
    if(attr(data_after_input[,i],"non_count?")){
      count<-count+1
      Names<-colnames(data_after_input)
      nc<-m[,i]
      
      nc<-scale(nc)
      q<-cbind(q,nc)  
      colnames(q)[count]<-Names[i]
    }    
    }
    if((!attr(data_after_input[,i],"is_denom?"))&(attr(data_after_input[,i],"denominator")!="default")){
      count<-count+1
      denominator<-attr(data_after_input[,i],"denominator")
      Names<-(colnames(data_after_input))
      numerator<-Names[i]
      
      j<-match(denominator,Names)
      
      divisor<-m[,j]
      dividend<-m[,i]
      dividend<-dividend+1
      divisor<-divisor+1
      
      r<-sum(m[,j])
      s<-sum(m[,i])
      nat_avg<-(s/r)     
      ##compute percentages/indexes,log/bc transform, zscore and put in matrix
      
      if(attr(data_after_input[,i],"transform")=="PCT,BC"){
        
        div<-(dividend/divisor)*100
        bc<-boxcox(div~1,plotit=FALSE)
        bestlambda<-with(bc, x[which.max(y)])
        INDBC<-bcPower(div,bestlambda)
        INDBC<-scale(INDBC)
        q<-cbind(q,INDBC)  
        colnames(q)[count]<-numerator
      }
      if(attr(data_after_input[,i],"transform")=="PCT,LOG"){
        div<-(dividend/divisor)*100
        PCTLOG<-log(div)
        PCTLOG<-scale(PCTLOG)
        q<-cbind(q,PCTLOG)        
        colnames(q)[count]<-numerator
      }
      if(attr(data_after_input[,i],"transform")=="IND,BC"){
        div<-(dividend/divisor)
        div<- (div/nat_avg)*100
        bc<-boxcox(div~1,plotit=FALSE)
        bestlambda<-with(bc, x[which.max(y)])
        INDBC<-bcPower(div,bestlambda)
        INDBC<-scale(INDBC)
        q<-cbind(q,INDBC)     
        colnames(q)[count]<-numerator
      }
      if(attr(data_after_input[,i],"transform")=="IND,LOG"){ 
        div<-(dividend/divisor)
        w<- (div/nat_avg)*100
        INDLOG<-log(w)
        INDLOG<-scale(INDLOG)
        q<-cbind(q,INDLOG) 
        colnames(q)[count]<-numerator
      }
      if(attr(data_after_input[,i],"transform")=="PCT,NONE"){
        div<-(dividend/divisor)*100
        
        PCT<-scale(div)
        q<-cbind(q,PCT)        
        colnames(q)[count]<-numerator
      }
      if(attr(data_after_input[,i],"transform")=="IND,NONE"){ 
        div<-(dividend/divisor)
        w<- (div/nat_avg)*100
        
        INDNONE<-scale(w)
        q<-cbind(q,INDNONE) 
        colnames(q)[count]<-numerator
      }
    }    
  }  
  q
}
