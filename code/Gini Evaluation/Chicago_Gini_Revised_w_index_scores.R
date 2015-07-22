#########################
###CHICAGO GINI REVISIONS
#########################
library(plyr)
#Load chicago crime and usa tract data frames
load("~/Dropbox/geodemo/Code/Gini Evaluation/Chicago_Crime_Tract.RData")
load("~/Dropbox/geodemo/RData Files/tract_data_with_classes_063013.Rdata")

usa.trt.cl$X10 <- as.factor(usa.trt.cl$X10)
chicago_tracts <- usa.trt.cl[grep("Cook County, Illinois", usa.trt.cl$Geo_NAME), ]
chicago_tracts$new_id <- substr(chicago_tracts$FIPS, 6, 11) #make common ID

#add total pop
chiPop <- read.csv("~/Dropbox/geodemo/Data/ChicagoPop.csv")

#merge files
chicago_tracts <- merge(x= chicago_tracts, y = crime, by.x="new_id", by.y="Census.Tract", sort=TRUE, all=TRUE)
chicago_tracts <- merge(x= chicago_tracts, y = chiPop, by.x="FIPS", by.y="Geo_FIPS", sort=TRUE, all=TRUE)


#drop missing tracts
chicago_tracts <- chicago_tracts[complete.cases(chicago_tracts),]
#chicago_tracts$X10 <- as.numeric(chicago_tracts$X10)


#Tally crimes by group
crime_sum <- aggregate(. ~ X10, data= chicago_tracts[,c(142,150:178)], sum)
chicago_means_by_group <- aggregate(. ~ X10, data= chicago_tracts[,c(142,150:178)], mean, na.rm=TRUE)
chicago_means_by_crime <- apply(chicago_tracts[,c(150:178)], mean, MARGIN = 2)
chi_crime_idx_score <- sweep(chicago_means_by_group[,2:30], 2, chicago_means_by_crime, '/') * 100 #index scores
crime_tot <- apply(crime_sum, FUN = sum, MARGIN = 2)
pop_tot <- aggregate(. ~ X10, data= chicago_tracts[, c("X10", "Population")], sum)
crime_rate_group <- sweep(crime_sum[,2:30], 1, pop_tot[,2], '/') *100000
crime_rate_avg <- crime_tot[2:30]/sum(chicago_tracts$Population) * 1000
chi_crime_rate_idx_score <- sweep(crime_rate_group, 2, crime_rate_avg , '/') * 100

#of total percent by crime
(crime_tot[2:30]/sum(crime_tot[2:30])) *100

#percent of pop by Group
pop_tot[,2]/sum(pop_tot[,2])

#percent of crime by group
apply(crime_sum[,2:30], FUN = sum, MARGIN = 1)/sum(apply(crime_sum[,2:30], FUN = sum, MARGIN = 1))


##Calculate the GINI INDEX
##uses columns of crime_sum table above
## For Checking VariableFromCrime_sum <- crime_sum[,3]
giniX10 <- function(VariableFromCrime_sum){
  Gdiff <- vector(length = length(crime_sum$X10)^2)
  ctr <- 1
  for (i in as.numeric(crime_sum$X10)){
    for (j in as.numeric(crime_sum$X10)){ 
      Gdiff[ctr] <- abs(VariableFromCrime_sum[i] - VariableFromCrime_sum[j])
      ctr <- ctr +1
    }
  }
  sum(Gdiff, na.rm = TRUE)/(2*length(as.numeric(crime_sum$X10))^2*mean(VariableFromCrime_sum, na.rm = TRUE))
}

chiGini <- data.frame(crimeType=names(crime_sum)[2:30], gini=NA)

for(i in names(crime_sum)[2:30]){
  chiGini[chiGini$crimeType==i, "crimeType"] <- i
  chiGini[chiGini$crimeType==i, "gini"] <- giniX10(crime_sum[,i])
}


#Combine gini and index scores
write.csv(cbind(chiGini, t(chi_crime_rate_idx_score)), "~/Dropbox/geodemo/External Validation/chicago_table.csv")

summary(chicago_tracts$X10)/sum(summary(chicago_tracts$X10))

