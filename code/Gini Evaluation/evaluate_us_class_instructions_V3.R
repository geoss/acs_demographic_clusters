#setwd("/Users/alex/Dropbox/geodemo/Gini Evaluation")
setwd("/Volumes/Macintosh HD 2/Dropbox/geodemo/Code/Gini Evaluation")

library(gdata)

# Get dependencies
install.packages("car")
install.packages("gplots")
install.packages("ggplot2")

# Get gd
temp <- tempfile(fileext = ".zip")
download.file("http://dl.dropbox.com/u/881843/RPubsData/gd/gd.zip", temp)
unzip(temp)
install.packages("gd", repos = NULL, type = "source", depend = TRUE)
unlink(temp)

library(gd)

source("evaluate_class_V5.R")


#Sort input data out

load("/Users/alex/Dropbox/geodemo/RData Files/tract_data_with_classes_063013.Rdata")

usa.trt <- read.table("/Users/alex/Dropbox/geodemo/Data/R10494789_SL140.csv", header=TRUE, skip=1, sep=",")#raw data
table_lookup <- read.csv("liam.csv")
select_vars <- as.character(table_lookup$var)
select_vars <- gsub("PCT_","",select_vars)

#Add denom
select_vars_den <- unique(paste("ACS11_5yr_",as.character(table_lookup$Denominator),sep=''))
select_vars_den <- select_vars_den[2:length(select_vars_den)]
select_vars <- c(select_vars,select_vars_den)

#Create counts_correct
counts_correct <- usa.trt[,select_vars]

#extract the clusters for the new rows
clusters_2.10.31.55<-usa.trt.cl[,c(3,141:144)]
results_10<-clusters_2.10.31.55[,c(1,2,4)]

#Remove NA values from results_10
results_10 <- results_10[!is.na(results_10$X10),]


#Create lookup
#lookup_12<-cluster_lookup(results_12)#contains the 72 clusters solution as types, and the 12 as groups

#Adapt naming for types

g <- data.frame(c(1:10),LETTERS[1:10])
colnames(g) <- c("Group","Group_Let")
results_10<- merge(results_10,g, by.x="X10",by.y="Group",all.x=TRUE)
results_10$Type <- paste(results_10$Group_Let,results_10$X55,sep='') 
  
type_codes <- unique(as.character(results_10$Type))
type_codes <- type_codes[order(type_codes)]
letters <- substr(type_codes,1,1)
numbers <- 1:length(letters)
new_type <- paste(letters,numbers,sep='')
type_look <- data.frame(type_codes,new_type)
  
results_10 <- merge(results_10,type_look, by.x="Type",by.y="type_codes",all.x=TRUE)
results_10$Type <- NULL
results_10$X10 <- NULL
results_10$X55 <- NULL  

colnames(results_10) <- c("Zone Code", "Group","Type")

write.csv(results_10,"lookup.csv")

#counts_newids <- counts_correct[which(is.na(area_matches)==F),] #limits rows to only those cases used in k means


counts_newids <- merge(counts_correct, results_10, by.x="Geo_FIPS",by.y="Zone Code", all.x=TRUE)
counts_newids <- counts_newids[!is.na(counts_newids$Group),]

##########
#The following code conducts the internal evalyation on the basis of the prep. in the previous code....
############

#Gini Maker (Types)
gini_list <- select_vars <- as.character(table_lookup$var)
gini_list <- gsub("PCT_","",select_vars)
gini_list <- gini_list[9:length(gini_list)]
gini_results<-data.frame()

#Replace results 10 with the new ordering for limit subset
results_10 <- counts_newids[,c(1,159,160)]
counts_newids <- counts_newids[,-c(159,160)]

for (i in (1:length(gini_list))){
  #worst code ever!
  numerator <- paste("PCT_",gini_list[i],sep='')
  denominator <- paste("ACS11_5yr_",as.character(table_lookup[table_lookup$var==numerator,]$Denominator),sep='')
  numerator <- gsub("PCT_","",numerator)
  #checks for areas where there are na values
  area_ok <- is.na((counts_newids[,numerator]))
  gini_coefficient<-evaluate_class(counts_newids[!area_ok,4:ncol(counts_newids)],results_10[!area_ok,],"types",numerator,denominator)
  temp<-cbind(numerator,gini_coefficient)
  gini_results<-rbind(gini_results,temp)
  print(temp)
  rm(numerator,denominator,gini_coefficient,temp)
}

gini_results_Types <- gini_results
colnames(gini_results_Types) <- c("Variable","Gini_Types")
gini_results_Types$Variable <- paste("PCT_",gini_results_Types$Variable,sep='')





#Gini Maker (Groups)
gini_list <- select_vars <- as.character(table_lookup$var)
gini_list <- gsub("PCT_","",select_vars)
gini_list <- gini_list[9:length(gini_list)]
gini_results<-data.frame()

#Replace results 10 with the new ordering for limit subset
results_10 <- counts_newids[,c(1,159,160)]
counts_newids <- counts_newids[,-c(159,160)]

for (i in (1:length(gini_list))){
  #worst code ever!
  numerator <- paste("PCT_",gini_list[i],sep='')
  denominator <- paste("ACS11_5yr_",as.character(table_lookup[table_lookup$var==numerator,]$Denominator),sep='')
  numerator <- gsub("PCT_","",numerator)
  #checks for areas where there are na values
  area_ok <- is.na((counts_newids[,numerator]))
  gini_coefficient<-evaluate_class(counts_newids[!area_ok,4:ncol(counts_newids)],results_10[!area_ok,],"groups",numerator,denominator)
  temp<-cbind(numerator,gini_coefficient)
  gini_results<-rbind(gini_results,temp)
  print(temp)
  rm(numerator,denominator,gini_coefficient,temp)
}

gini_results_Groups <- gini_results
colnames(gini_results_Groups) <- c("Variable","Gini_Groups")
gini_results_Groups$Variable <- paste("PCT_",gini_results_Groups$Variable,sep='')

#Create Gini Master List
gini_master <- merge(gini_results_Types,gini_results_Groups, by="Variable")
gini_master <- merge(gini_master, table_lookup, by.x="Variable",by.y="var",all.x=TRUE)
gini_master$desc <- gsub("_"," ",gini_master$desc)
gini_master <- gini_master[,c(1,5,2:3,6:7)]

gini_master$Gini_Types <- as.numeric(as.character(gini_master$Gini_Types))
gini_master$Gini_Groups <- as.numeric(as.character(gini_master$Gini_Groups))

#Plot for gini type
require(ggplot2)
pdf(file='55_gini.pdf')
ggplot(gini_master, aes(factor(Domain), as.numeric(as.character(Gini_Types)))) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + ylab("Gini Coefficient") + xlab("Domain") + theme(legend.position="none") + geom_boxplot(aes(fill=factor(Domain))) + 
  scale_fill_brewer(palette="Set3")
dev.off()


#Plot for gini group 10
require(ggplot2)
pdf(file='10_gini.pdf')
ggplot(gini_master, aes(factor(Domain), as.numeric(as.character(Gini_Groups)))) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + ylab("Gini Coefficient") + xlab("Domain") + theme(legend.position="none") + geom_boxplot(aes(fill=factor(Domain))) + scale_fill_brewer(palette="Set3")
dev.off()


#Write out CSV of the results for internal evaluation
tmp_gini <- gini_master[order(as.numeric(as.character(gini_master$Gini_Types)),decreasing =TRUE),c("desc","Concept","Domain","Gini_Types")] #5 outliers

d <- unique(tmp_gini$Domain)

for (i in 1:length(d)){
  assign(paste(d[i]),tmp_gini[tmp_gini$Domain==paste(d[i]),])
  write.csv(get(paste(d[i])),paste("results_domain_",d[i],".csv",sep=''))
}

#Cleanup
remove(usa.trt)
save.image("gini_results.RData")


##################################################################################################################
## External Evaluation ###
##################################################################################################################

setwd("/Users/alex/Dropbox/geodemo/Gini Evaluation")
library(gdata)

# Get dependencies
install.packages("car")
install.packages("gplots")
install.packages("ggplot2")

# Get gd
temp <- tempfile(fileext = ".zip")
download.file("http://dl.dropbox.com/u/881843/RPubsData/gd/gd.zip", temp)
unzip(temp)
install.packages("gd", repos = NULL, type = "source", depend = TRUE)
unlink(temp)

library(gd)

source("evaluate_class_V5.R")

####################################################################################################
###########################################Donations################################################
####################################################################################################
setwd("/Users/alex/Dropbox/geodemo/Gini Evaluation")

#Load - classification etc (these are created from the internal evaluation)
load("gini_results.RData") #results_10 is the classification
#Load - donations data
load("/Users/alex/Dropbox/geodemo/External Validation/RData_Processed/Donations_Tract.RData")
#Create a total population file
t_pop <- counts_correct[,c("Geo_FIPS","ACS11_5yr_B01001001")]
#Merge classification and pop
geodem <- merge(results_10, t_pop, by.x="Geo_FIPS",by.y="Geo_FIPS", all.x=TRUE)

## Test 1 - To what extent can tracts with populations more likely to donate be detected?
test1 <- merge(geodem, t_All_match_TRACT_2010, by.x="Geo_FIPS",by.y="trtid10", all.x=TRUE)
test1[is.na(test1)] <- 0 #replace all NA with 0 - i.e. no donations
test1_lookup <- test1[,1:3]#Create a lookup

numerator <- "DON"
denominator <- "ACS11_5yr_B01001001"
gini_test1<-evaluate_class(test1[4:19],test1_lookup,"types",numerator,denominator)

## Test 2 - To what extent can those tracts where people are donating < $1k, >$1k and  > $5K be detected?
numerator <- c("L_1000","G_1000","G_5000")
denominator <- "ACS11_5yr_B01001001"
gini_test2<-evaluate_class(test1[4:19],test1_lookup,"types",numerator,denominator)

## Test 3 - To what extent can donars from different occupational groups be detected?
numerator <- names(test1[9:19])
denominator <- "ACS11_5yr_B01001001"
gini_test3<-evaluate_class(test1[4:19],test1_lookup,"types",numerator,denominator)

## Test 3b - To what extent can donars from different occupational groups be detected?
numerator <- names(test1[9:19])
denominator <- "DON"
gini_test3<-evaluate_class(test1[4:19],test1_lookup,"types",numerator,denominator)



###Generate table of population by different occupational groups and cluster

library(doBy)

pop_occ_group <- summaryBy(DON + L_1000 + G_1000 + G_5000 + HomeMaker + Higher_Prof + Retired + Professor + Legal + Medical + Farmer + Finance + Engineer + Student + Creative  ~ Group, data = test1, FUN = function(x) { round(sum(x)) } )

names(pop_occ_group) <- c("Group","Donate","Donate Less $1000","Donate $1000 or more","Donate $5000 or more","Home Maker","Higher Professionals","Retired","Professor","Legal","Medical","Farmer","Finance","Engineer","Student","Creative")

pct <- round(prop.table(as.matrix(pop_occ_group[2:16]),2) * 100,1)
pop_occ_group_pct <- cbind(pop_occ_group[1],pct)

pop_occ_group_ind<- round(pop_occ_group_pct[3:16] / pop_occ_group_pct$Donate *100)


pop_occ_group_pct_t <- t(pop_occ_group_pct)
colnames(pop_occ_group_pct_t) <- LETTERS[1:10]
pop_occ_group_pct_t <- pop_occ_group_pct_t[2:nrow(pop_occ_group_pct_t),]
pop_occ_group_pct_t <- as.data.frame(pop_occ_group_pct_t)



pop_occ_group_ind_t <- t(pop_occ_group_ind)
colnames(pop_occ_group_ind_t) <- LETTERS[1:10]


#Create output tables

Gini <- c(0.43,0.54,0.59,0.48,0.43,0.42,0.57,0.59,0.38,0.48,0.52,0.44,0.58,0.68)

pop_occ_group_ind_t <- cbind(Gini, pop_occ_group_ind_t) #Index Table
pop_occ_group_pct_t <- cbind(Gini, pop_occ_group_pct_t[-1,]) #Pct Table


library(xtable)



####################################################################################################
###########################################Mortgage#################################################
####################################################################################################
setwd("/Users/alex/Dropbox/geodemo/Gini Evaluation")


#Load - classification etc (these are created from the internal evaluation)
load("gini_results.RData") #results_10 is the classification
#Load - donations data
load("/Users/alex/Dropbox/geodemo/External Validation/RData_Processed/mortgage_data.RData")
#Create a total population file
t_pop <- counts_correct[,c("Geo_FIPS","ACS11_5yr_B01001001")]
#Merge classification and pop
geodem <- merge(results_10, t_pop, by.x="Geo_FIPS",by.y="Geo_FIPS", all.x=TRUE)


#Test 1 - Identify owner occupied housing within neibourhoods in 2010...
test1 <- merge(mortgage_data_all_2010, geodem, by.y="Geo_FIPS",by.x="trtid10", all.x=TRUE)
test1 <- test1[!is.na(test1$Group),]#remove those tract without matches
lookup <- test1[,c(1,14,15)]

numerator <- "N_OOHPL"
denominator <- "N_HU_2010"
gini_test1<-evaluate_class(test1[2:13],lookup,"types",numerator,denominator)

#Test 2 - Identify investor housing within neibourhoods in 2010...
numerator <- "N_IHPL"
denominator <- "N_HU_2010"
gini_test2<-evaluate_class(test1[2:13],lookup,"types",numerator,denominator)

#Test 3 - Identify family housing within neibourhoods in 2010... 
numerator <- "N_FHU_2010"
denominator <- "N_HU_2010"
gini_test3<-evaluate_class(test1[2:13],lookup,"types",numerator,denominator)

#Test 4 - High cost home loans - personal - 04-06
numerator <- "N_HCHPL_04_06"
denominator <- "N_HCHPL_04_06_WIRI"
gini_test4<-evaluate_class(test1[2:13],lookup,"types",numerator,denominator)

#Test 5 - High cost home loans - investment - 04 06
numerator <- "N_HCIHPL_04_06"
denominator <- "N_HCHPL_04_06_WIRI"
gini_test5<-evaluate_class(test1[2:13],lookup,"types",numerator,denominator)

#Test 6 - High cost refinance loans - 04 - 06
numerator <- "N_HCRL_04_06"
denominator <- "N_RL_04_06_WIRI"
gini_test6<-evaluate_class(test1[2:13],lookup,"types",numerator,denominator)


####################################################################################################
###########################################CRIME####################################################
####################################################################################################
setwd("/Users/alex/Dropbox/geodemo/Gini Evaluation")

#Load - classification etc (these are created from the internal evaluation)
load("gini_results.RData") #results_10 is the classification
#Load - crime data
load("/Users/alex/Dropbox/geodemo/External Validation/RData_Processed/Chicago_Crime_Tract.RData")#crime
#Create a total population file
t_pop <- counts_correct[,c("Geo_FIPS","ACS11_5yr_B01001001")]
#Merge classification and pop
geodem <- merge(results_10, t_pop, by.x="Geo_FIPS",by.y="Geo_FIPS", all.x=TRUE)

#Read in a list chicago tracts
f <- rep("character",3)
chicago <- read.csv("/Users/alex/Dropbox/geodemo/External Validation/chicago crime/Chicago_Tracts.csv",,colClasses=f)
chicago$Geo_FIPS <- paste("17",chicago$COUNTY,chicago$TRACT,sep='')

#Correct crime tract ID
crime$Census.Tract.New <- ifelse(nchar(crime$Census.Tract) ==5, paste("0",crime$Census.Tract,sep=''), crime$Census.Tract)

#Append crime and geodem to Chicago tracts
Chicago_Tract <- merge(chicago, geodem, by.x="Geo_FIPS",by.y="Geo_FIPS",all.x=TRUE )
Chicago_Tract <- merge(Chicago_Tract,crime, by.x="TRACT",by.y="Census.Tract.New",all.x=TRUE )

#Remove rows with NA (6 tracts in total)
Chicago_Tract <- Chicago_Tract[!is.na(Chicago_Tract$Group),]
Chicago_Tract <- Chicago_Tract[!is.na(Chicago_Tract$All),]

#Test 1 - All instance of crime relative to the total population
lookup <- Chicago_Tract[,c(2,5,6)]
numerator <- "All"
denominator <- "ACS11_5yr_B01001001"
gini_test1<-evaluate_class(Chicago_Tract[,c(7,10,13:41)],lookup,"types",numerator,denominator)

#Test 2 - Burglary
numerator <- "Burglary"
denominator <- "ACS11_5yr_B01001001"
gini_test2<-evaluate_class(Chicago_Tract[,c(7,10,13:41)],lookup,"types",numerator,denominator)

#Test 2 - Burglary
numerator <- names(Chicago_Tract[13:41])
denominator <- "ACS11_5yr_B01001001"
gini_test2<-evaluate_class(Chicago_Tract[,c(7,10,13:41)],lookup,"types",numerator,denominator)


############################################################################################################################################
################################################################### Hypothetical Test ######################################################
############################################################################################################################################

#Load - classification etc (these are created from the internal evaluation)
load("gini_results.RData") #results_10 is the classification

#Create a total population file
t_pop <- counts_correct[,c("Geo_FIPS","ACS11_5yr_B01001001")]

#Merge classification and pop
geodem <- merge(results_10, t_pop, by.x="Geo_FIPS",by.y="Geo_FIPS", all.x=TRUE)

#for a count variable that is more or less the same everywhere:
usa.trt.cl$even.test <- rpois(dim(usa.trt.cl)[1],lambda=1500) #denominator is just population

#For class specific count variables:
usa.trt.cl$X3_test  <- ifelse(usa.trt.cl$X10==3,rpois(dim(usa.trt.cl)[1],1500),0) #largest group
usa.trt.cl$X4_test  <- ifelse(usa.trt.cl$X10==4,rpois(dim(usa.trt.cl)[1],1500),0) #smallest group
usa.trt.cl$X9_test  <- ifelse(usa.trt.cl$X10==9,rpois(dim(usa.trt.cl)[1],1500),0) #medium sized group


#Synthetic Data
synth <- usa.trt.cl[,c("FIPS","even.test","X3_test","X4_test","X9_test")]

#Merge classification and synth
synth <- merge(synth, geodem, by.x="FIPS",by.y="Geo_FIPS", all.x=TRUE)

#Remove NA
synth <- synth[!is.na(synth$Group),]


#Test 1 - All instance of crime relative to the total population
lookup <- synth[,c(1,6,7)]
numerator <- c("even.test","X3_test","X4_test","X9_test")
denominator <- "ACS11_5yr_B01001001"
gini_test1<-evaluate_class(synth[,c(2:5,8)],lookup,"types",numerator,denominator)



