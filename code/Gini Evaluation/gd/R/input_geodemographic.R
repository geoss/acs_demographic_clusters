input_geodemographic<-function(data,spec){
  options(warn=-1)
  ##read in data and spec document.
  a<-c()
  if(is.data.frame(spec)){
    
    a<-as.vector(t(spec))
  }
  
  if(is.data.frame(data)){
    data<-data
  }
  if(!is.data.frame(spec)){
    a<-readLines(spec,warn=FALSE)
  }
  if(!is.data.frame(data)){
    data<-read.csv(data,check.names=FALSE)
  }
  dashcount<-0
  digitcount<-0
  for(i in 1:ncol(data)){
    Names<-colnames(data)
    a1<-Names[i]
    
    sstring<-substr(a1,1,1)
    
    if(length(grep('-',a1))>0){
      cat("'",a1,"'"," is not a valid variable name","\n")
      dashcount<-dashcount+1
    }
    if(length(grep('[0-9]',sstring))>0){
      cat("'",a1,"'"," is not a valid variable name","\n")
      digitcount<-digitcount+1
    }
    
  }
  
  if((digitcount>0)|(dashcount>0))
    stop()
  
  
  
  
  ##create appropriate attributes to make dataframe relational. set all to zero or false to start with.
  for(i in 1:ncol(data)){
    attr(data[,i],"denominator")<-"default"
    attr(data[,i],"is_denom?")<-FALSE
    attr(data[,i],"non_count?")<-FALSE
    attr(data[,i],"transform")<-"default"
  }
  
  
  nc<-ncol(data)
  
  
  spec_length<-length(a)
  
  
  ##Loop through lines of spec document to obtain details about the data
  
  
  
  for(i in 1:spec_length){
    b<-a[i]
    
    
    b<-gsub(" ","",b, fixed=TRUE)
    if(length(grep('zonecodes',b))>0){
      if(length(grep('zonecodes',b))>0){
        
        b<-gsub(" ","",b,fixed=TRUE)
        
        b<-gsub("zonecodes","",b,fixed=TRUE)
        
        Names<-colnames(data)
        zones<-match(b,Names)
        rownames(data)<-data[,zones]
        data<-data[,-zones]
      }
    }
    ##First find variables with denominators
    ##Then save them as the corresponding variables' "denominator" attributes.
    ##Also save boolean "is_denom?" and "non_count?" attributes.
    
    if(length(grep('noncount',b))>0){
      
      b<-gsub(" ","",b,fixed=TRUE)
      
      b<-gsub("noncount","",b,fixed=TRUE)
      
      b<-strsplit(b[1],',')
      
      b<-b[[1]]
      
      l2<-length(b)
      for(i in 1:l2){
        n<-b[i]
        
        Names<-colnames(data)
        p<-match(n,Names)
        
        attr(data[,p],"non_count?")<-TRUE
        attr(data[,p],"is_denom?")<-FALSE
      }
    }
    
    if(length(grep('count',b))>0){
      b<-gsub("count","",b,fixed=TRUE)
      b<-strsplit(b,'|',fixed=TRUE)
      b<-b[[1]]
      denom<-b[2]
      
      attr(data[,b[2]],"is_denom?")<-TRUE
      attr(data[,b[2]],"non_count?")<-FALSE
      b<-strsplit(b[1],',')
      b<-b[[1]]
      l1<-length(b)
      
      for(j in 1:l1){
        tt<-length(grep('-',b[j]))
        
        if(tt==1){
          c<-strsplit(b[j],'-')
          d<-c[[1]]
          e<-d[1]
          f<-d[2]
          
          Names<-colnames(data)
          
          
          l<-match(e,Names)
          
          
          m<-match(f,Names)
          
          temp<-seq(l,m)
          templength<-length(temp)
          for(j in 1:templength){
            attr(data[,temp[j]],"denominator")<-denom
            attr(data[,temp[j]],"is_denom?")<-FALSE
            attr(data[,temp[j]],"non_count?")<-FALSE
          }
        }
        if(tt==0){
          g<-b[j]
          
          Names<-colnames(data)
          h<-match(g,Names)
          
          attr(data[,h],"denominator")<-denom
          attr(data[,h],"is_denom?")<-FALSE
          attr(data[,h],"non_count?")<-FALSE
        }
      }
    }
    
    
    
    
    ##Find noncounts in the spec documents, and update their "non_count?" attribute
    
    
    
    ##find the transform information for each specified variable, update the attribute
    
    if(length(grep('transform',b))>0){
      
      b<-gsub(" ","",b,fixed=TRUE)
      
      b<-gsub("transform","",b,fixed=TRUE)
      b<-strsplit(b,'|',fixed=TRUE)
      b<-b[[1]]
      
      trans_info<-b[2]
      
      c<-strsplit(b[1],',')
      c<-c[[1]]
      l1<-length(c)
      
      for(j in 1:l1){
        tt<-length(grep('-',c[j]))
        if(tt==0){
          g<-c[j]
          
          Names<-colnames(data)
          h<-match(g,Names)
          
          attr(data[,h],"transform")<-trans_info
          
          p
          
        }
        if(tt==1){
          c1<-strsplit(c[j],'-')
          
          d<-c1[[1]]
          
          e<-d[1]
          
          f<-d[2]
          
          Names<-colnames(data)
          l<-match(e,Names)
          
          m<-match(f,Names)
          
          temp<-seq(l,m)
          
          
          templength<-length(temp)
          for(j in 1:templength){
            
            attr(data[,temp[j]],"transform")<-trans_info
            
          }
        }
        
        
        
      }
      
    }
    
  }
  
  
  
  vec<-c()
  atts_not_specified<-c()
  for(i in 1:ncol(data)){
    if(!attr(data[,i],"is_denom?")&(!attr(data[,i],"non_count?"))&(attr(data[,i],"denominator")=="default")){
      Names<-colnames(data)
      vec[i]<-i
      atts_not_specified[i]<-Names[i]
    }
  }
  if(length(atts_not_specified)>0){
  atts_not_specified<-na.omit(atts_not_specified)
  cat("The following variables were not included in the specification file as count or non-count variables, and will not be in the resulting data frame. If you wish for any of these variables to be included, please ensure they are in the specification file.","\n","\n",atts_not_specified,"\n","\n")
  vec<-na.omit(vec)
  
  data<-data[-vec]
  }
  no_transform_info<-c()
  for(i in 1:ncol(data)){
    if(((attr(data[,i],"transform"))=="default")&(!attr(data[,i],"is_denom?")))
      no_transform_info[i]<-colnames(data)[i]
  }
  
  nti<-na.omit(no_transform_info)
  
  if(length(nti)>0){
    
    cat("The following variables had no transform information entered: ","\n","\n",nti,"\n","\n","The transform information for these variables will be assumed to be NONE.")
    
    
    
    for(i in 1:length(nti)){
      temp<-toString(nti[i])
      Names<-colnames(data)
      te<-match(temp,Names)
      
      attr(data[,te],"transform")<-"NONE"
    }
  }
  
  
  data
}