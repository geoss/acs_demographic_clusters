
############External validation###############################################
#################################################################################

######################### 
####Mortgage Data########
#########################

setwd("/Users/alex/Dropbox/geodemo/External Validation/US Mortgage Lending SUmmary")

r_list <- list.files(path=".")

long_names <- c("Level of Geography","Census Tract Identifier","City Name","County and State","Metropolitan Area","Median Value of First Lien Home Purchase Loans (1-4 Units), 2010","Number of First Lien Home Purchase Loans (1-4 Units), 2010","Number of First Lien Home Purchase Loans (1-4 Units), 2010, Per 1,000 1-4 Family Units","Number of First Lien Investor Home Purchase Loans (1-4 Units), 2010","Number of First Lien Owner-Occupant Home Purchase Loans (1-4 Units), 2010","Percent of First Lien Home Purchase Loans (1-4 Units) that are Investors, 2010","Number of First Lien Investor Home Purchase Loans (1-4 Units), 2010, Per 1,000 1-4 Family Units","Total Number of Housing Units, 2000","Total Number of 1-4 Family Housing Units, 2000","Total Number of Housing Units, 2010","Total Number of 1-4 Family Housing Units, 2010","Number of High-Cost Home Purchase Loans (1-4 Units), 2004-2006","Number of Home Purchase Loans (1-4 Units) with Interest Rate Info, 2004-2006","Percent of Home Purchase Loans (1-4 Units) that are High-Cost, 2004-2006","Number of High-Cost Home Purchase Loans, 2004-2006, Per 1,000 1-4 Family Units","Number of High-Cost Home Purchase Loans, 2004-2006, Per 1,000 Total Housing Units","Number of High-Cost Investor Home Purchase Loans (1-4 Units), 2004-2006","Number of High-Cost Investor Home Purchase Loans, 2004-2006, Per 1,000 1-4 Family Units","Number of High-Cost Refinance Loans (1-4 Units), 2004-2006","Number of Refinance Loans (1-4 Units) with Interest Rate Info, 2004-2006","Percent of Refinance Loans (1-4 Units) that are High-Cost, 2004-2006","Number of High-Cost Refinance Loans, 2004-2006, Per 1,000 1-4 Family Units","Number of High-Cost Refinance Loans, 2004-2006, Per 1,000 Total Housing Units")
short_names <- paste("V",1:28,sep='')
lookup_mortgage <- cbind(short_names,long_names)

mortgage_data_all <- data.frame()


convert <- function(x){as.numeric(as.character(x))}


for (i in 1:length(r_list)){
  #Load Lookup tables
  assign(paste(r_list[i]),read.xls(paste(r_list[i]), sheet = 1,method="tab"))
  #Remove hidden columns
  #drops <- c("X","X.1","X.2","X.3","X.4")
  drops <- c(3,5,7,9,10)
  assign("temp", get(paste(r_list[i]))[,-drops])
  #Apply short names
  names(temp)<- short_names
  assign(paste(r_list[i]),temp)
  remove(temp)
  #Remove non counts and non tract geography
  drops <- c("V1","V3","V4","V5","V6","V8","V11","V12","V19","V20","V21","V23","V26","V27","V28")
  assign(paste(r_list[i]), get(paste(r_list[i]))[!is.na(get(paste(r_list[i]))$V2),!(names(get(paste(r_list[i]))) %in% drops)])
  assign(paste(r_list[i]), as.data.frame(sapply(get(paste(r_list[i])), convert)))
  #Append Data and remove temp
  mortgage_data_all <- rbind(mortgage_data_all,get(paste(r_list[i])))
  assign(paste(r_list[i]),NULL)
  print(r_list[i])
}


#Number of First Lien Home Purchase Loans (1-4 Units), 2010 [N_THPL]
#Number of First Lien Investor Home Purchase Loans (1-4 Units), 2010 [N_IHPL]
#Number of First Lien Owner-Occupant Home Purchase Loans (1-4 Units), 2010 [N_OOHPL]
#Total Number of Housing Units, 2000 [N_HU_2000]
#Total Number of 1-4 Family Housing Units, 2000 [N_FHU_2000]
#Total Number of Housing Units, 2010 [N_HU_2010]
#Total Number of 1-4 Family Housing Units, 2010 [N_FHU_2010]
#Number of High-Cost Home Purchase Loans (1-4 Units), 2004-2006 [N_HCHPL_04_06]
#Number of Home Purchase Loans (1-4 Units) with Interest Rate Info, 2004-2006 [N_HCHPL_04_06_WIRI]
#Number of High-Cost Investor Home Purchase Loans (1-4 Units), 2004-2006 [N_HCIHPL_04_06]
#Number of High-Cost Refinance Loans (1-4 Units), 2004-2006 [N_HCRL_04_06]
#Number of Refinance Loans (1-4 Units) with Interest Rate Info, 2004-2006 [N_RL_04_06_WIRI]

nn <- c("TRACT","N_THPL","N_IHPL","N_OOHPL","N_HU_2000","N_FHU_2000","N_HU_2010","N_FHU_2010","N_HCHPL_04_06","N_HCHPL_04_06_WIRI","N_HCIHPL_04_06","N_HCRL_04_06","N_RL_04_06_WIRI")

names(mortgage_data_all) <- nn



#Convert the 2000 tract in 2010 tracts

#Import lookup (http://www.s4.brown.edu/us2010/Researcher/LTBDDload/DataList.aspx)
#The reference for technical documentation is John R. Logan, Zengwang Xu, and Brian Stults. 2012. “Interpolating US Decennial Census Tract Data from as Early as 1970 to 2010: A Longitudinal Tract Database” Professional Geographer, forthcoming.

Tract_2000_2010 <- read.csv("/Users/alex/Dropbox/geodemo/External Validation/crosswalk_2000_2010.csv")
Tract_2000_2010 <- Tract_2000_2010[,1:3]
mortgage_data_all <- merge(mortgage_data_all, Tract_2000_2010, by.x="TRACT",by.y="trtid00", all.x=TRUE)
mortgage_data_all <- mortgage_data_all[!is.na(mortgage_data_all$weight),]

#Multiply 2000 tract scores up to 2010
v_list <- names(mortgage_data_all[2:13])

tmp_all <- mortgage_data_all[,c(1,14)]

for (i in 1:length(v_list)){
  tmp <- with(mortgage_data_all, mortgage_data_all[,paste(v_list[i])] * mortgage_data_all[15])
  names(tmp) <- paste(v_list[i])
  tmp_all <- cbind(tmp_all,tmp)
  remove(tmp)
}


#Aggregate 2010 scores
options(scipen=500)
library(sqldf)

mortgage_data_all_2010 <- sqldf("select trtid10, sum(N_HU_2000) as N_HU_2000,  sum(N_HCHPL_04_06) as N_HCHPL_04_06,	sum(N_RL_04_06_WIRI) as N_RL_04_06_WIRI,	sum(N_THPL) as N_THPL,	sum(N_FHU_2000) as N_FHU_2000,	sum(N_IHPL) as N_IHPL,	sum(N_HU_2010) as N_HU_2010,	sum(N_HCIHPL_04_06) as N_HCIHPL_04_06,	sum(N_OOHPL) as N_OOHPL,	sum(N_FHU_2010) as N_FHU_2010,	sum(N_HCRL_04_06) as N_HCRL_04_06,	sum(N_HCHPL_04_06_WIRI) as N_HCHPL_04_06_WIRI from tmp_all group by trtid10")

save(mortgage_data_all_2010,file="/Users/alex/Dropbox/geodemo/External Validation/RData_Processed/mortgage_data.RData")



#######################################Campaign Contribution###################################################
###############################################################################################################


setwd("/Users/alex/Dropbox/geodemo/External Validation/")

contrib_names <- c("CMTE_ID",  "AMNDT_IND",  "RPT_TP",	"TRANSACTION_PGI",	"IMAGE_NUM",	"TRANSACTION_TP",	"ENTITY_TP",	"NAME",	"CITY",	"STATE",	"ZIP_CODE",	"EMPLOYER",	"OCCUPATION",	"TRANSACTION_DT",	"TRANSACTION_AMT",	"OTHER_ID",	"TRAN_ID",	"FILE_NUM",	"MEMO_CD",	"MEMO_TEXT",	"SUB_ID")

contributions <- read.delim("itcont.txt",header = FALSE,sep = "|")
names(contributions) <- contrib_names

contributions <- contributions[as.numeric(contributions$TRANSACTION_AMT) > 0,]


trans_id <- c("10",  "10J",	"11",	"11J",	"12",	"13",	"15",	"15C",	"15E",	"15F",	"15I",	"15J",	"15T",	"15Z",	"16C",	"16F",	"16G",	"16H",	"16J",	"16K",	"16L",	"16R",	"16U",	"17R",	"17U",	"17Y",	"17Z",	"18G",	"18H",	"18J",	"18K",	"18L",	"18S",	"18U",	"19",	"19J",	"20",	"20A",	"20B",	"20C",	"20D",	"20F",	"20G",	"20R",	"20V",	"20Y",	"21Y",	"22G",	"22H",	"22J",	"22K",	"22L",	"22R",	"22U",	"22X",	"22Y",	"22Z",	"23Y",	"24A",	"24C",	"24E",	"24F",	"24G",	"24H",	"24I",	"24K",	"24N",	"24P",	"24R",	"24T",	"24U",	"24Z",	"28L",	"29")
trans_names <- c("Non-Federal Receipt from Persons",  "Memo Receipt from JF Super PAC",	"Tribal Contribution",	"Memo Receipt from JF Tribal",	"Non-Federal Other Receipt - Levin Account (L-2)",	"Inaugural Donation Accepted",	"Contribution",	"Contribution from Candidate",	"Earmarked Contribution",	"Loans forgiven by Candidate",	"Earmarked Intermediary In",	"Memo (Filer's Percentage of Contribution Given to Join Fundraising Committee)",	"Earmarked Intermediary Treasury In",	"In-Kind Contribution Received from Registered Filer",	"Loans Received from the Candidate",	"Loans Received from Banks",	"Loan from Individual",	"Loan from from Registered Filers",	"Loan Repayments from Individual",	"Loan Repayments from from Registered Filer",	"Loan Repayments Received from Unregistered Entity",	"Loans Received from Registered Filers",	"Loan Received from Unregistered Entity",	"Contribution Refund Received from Registered Entity",	"Refunds/Rebates/Returns Received from Unregistered Entity",	"Refunds/Rebates/Returns from Individual or Corporation",	"Refunds/Rebates/Returns from Candidate or Committee",	"Transfer In Affiliated",	"Honorarium Received",	"Memo (Filer's Percentage of Contribution Given to Join Fundraising Committee)",	"Contribution Received from Registered Filer",	"Bundled Contribution",	"Receipts from Secretary of State",	"Contribution Received from Unregistered Committee",	"Electioneering Communication Donation Received",	"Memo (Electioneering Communication Percentage of Donation Given to Join Fundraising Committee)",	"Disbursement - Exempt from Limits",	"Non-Federal Disbursement - Levin Account (L-4A) Voter Registration",	"Non-Federal Disbursement - Levin Account (L-4B) Voter Identification",	"Loan Repayments Made to Candidate",	"Non-Federal Disbursement - Levin Account (L-4D) Generic Campaign",	"Loan Repayments Made to Banks",	"Loan Repayments Made to Individual",	"Loan Repayments Made to Registered Filer",	"Non-Federal Disbursement - Levin Account (L-4C) Get Out The Vote",	"Non-Federal Refund",	"Tribal Refund",	"Loan to Individual",	"Loan to Candidate or Committee",	"Loan Repayment to Individual",	"Loan Repayment to Candidate or Committee",	"Loan Repayment to Bank",	"Contribution Refund to Unregistered Entity",	"Loan Repaid to Unregistered Entity",	"Loan Made to Unregistered Entity",	"Contribution Refund to Individual",	"Contribution Refund to Candidate or Committee",	"Inaugural Donation Refund",	"Independent Expenditure Against",	"Coordinated Expenditure",	"Independent Expenditure For",	"Communication Cost for Candidate (C7)",	"Transfer Out Affiliated",	"Honorarium to Candidate",	"Earmarked Intermediary Out",	"Contribution Made to Non-Affiliated",	"Communication Cost Against Candidate (C7)",	"Contribution Made to Possible Candidate",	"Election Recount Disbursement",	"Earmarked Intermediary Treasury Out",	"Contribution Made to Unregistered Entity",	"In-Kind Contribution Made to Registered Filer",	"Refund of Bundled Contribution",	"Electioneering Communication Disbursement or Obligation")
translookup <- cbind(trans_id,trans_names)

#Summary of the AMT
summary(contributions$TRANSACTION_AMT)
sd(contributions$TRANSACTION_AMT)

#jobs summary
jobs <- as.data.frame(table(contributions$OCCUPATION))
jobs <- jobs[order(jobs$Freq,decreasing = TRUE),]

#Creating some categories
contributions$HomeMaker <- 0
contributions$Higher_Prof <- 0
contributions$Retired <- 0
contributions$Professor <-0 
contributions$Legal <- 0
contributions$Engineer <-0 
contributions$Medical<- 0
contributions$Farmer<- 0
contributions$Finance<- 0
contributions$Student<- 0
contributions$Creative<- 0

#These were assigned by manually looking over the top 1000 (~80% diff categories)
contributions$HomeMaker[contributions$OCCUPATION %in% c("MOTHER","SPOUSE","MOM","HOMEMAKER","HOUSEWIFE","HOME MAKER")] <- 1
contributions$Higher_Prof[contributions$OCCUPATION %in% c("C.E.O","PRESIDENT/ CEO","CHIEF EXECUTIVE","EXECUTIVE VICE-PRESIDENT","CEO/OWNER","C. E. O.","EXEC. DIRECTOR","EXEC. VICE PRESIDENT","CORPORATE VICE PRESIDENT","CHAIRMAN & CHIEF EXECUTIVE OFFICER","ASSISTANT VICE PRESIDENT","V.P.","SENIOR DIRECTOR","SR VICE PRESIDENT","PRESIDENT/C.E.O.","CHAIRMAN & C.E.O.","CEO/PRESIDENT","PRESIDENT & C.E.O.","PRESIDENT & CHIEF EXECUTIVE OFFICER","PRESIDENT & COO","OWNER/PRESIDENT","CHIEF OF STAFF","SR. VICE PRESIDENT","PRESIDENT/OWNER","VICE-PRESIDENT","PRESIDENT AND CHIEF EXECUTIVE OFFICER","CHAIRMAN/CEO","CHAIRMAN AND CEO","PRESIDENT","OWNER","CEO","VICE PRESIDENT","DIRECTOR","CHAIRMAN","MANAGING DIRECTOR","EXECUTIVE DIRECTOR","PRESIDENT & CEO","SENIOR VICE PRESIDENT","PRESIDENT/CEO","EXECUTIVE VICE PRESIDENT","CHIEF EXECUTIVE OFFICER","VP","C.E.O.","CHAIRMAN & CEO","MD","MANAGING PARTNER","PARTNER","PRESIDENT AND CEO","PARTNER/PRINCIPAL","CHAIRMAN OF THE BOARD","VICE CHAIRMAN","SENIOR VP")] <- 1
contributions$Retired[contributions$OCCUPATION %in% c("RETIREE","RETIRED")] <- 1
contributions$Professor[contributions$OCCUPATION %in% c("RETIRED PROFESSOR","PROFESSOR OF PHYSICS","ADJUNCT PROFESSOR","UNIVERSITY PROFESSOR","PROFESSOR EMERITUS","DEAN","ASSOCIATE PROFESSOR","LECTURER","PROFESSOR","FACULTY","ASSISTANT PROFESSOR")] <- 1
contributions$Legal[contributions$OCCUPATION %in% c("TAX ATTORNEY","ATTORNEY/CONSULTANT","PATENT ATTORNEY","ATTORNEY/PARTNER","JUDGE","LAWYER","ATTORNEY AT LAW","PARALEGAL")] <- 1
contributions$Engineer[contributions$OCCUPATION %in% c("VP ENGINEERING","CHEMICAL ENGINEER","PETROLEUM ENGINEER","ENGINEER","ENGINEERING MANAGER")] <- 1
contributions$Medical[contributions$OCCUPATION %in% c("FAMILY PHYSICIAN","PEDIATRIC DENTIST","PHYSICAN","ORTHOPEDIC SURGEON","CARDIOLOGIST","ORAL SURGEON","M.D.","NEUROSURGEON","ADULT CARDIOLOGY","PEDIATRICIAN","UROLOGIST","PSYCHIATRIST","EMERGENCY PHYSICIAN","ANESTHESIOLOGIST","DENTIST","PHYSICIAN","ORTHOPAEDIC SURGEON","SURGEON","PATHOLOGIST","DOCTOR OF OPTOMETRY","MEDICAL DOCTOR")] <- 1
contributions$Farmer[contributions$OCCUPATION %in% c("GROWER","RANCHER","FARMER/RANCHER","HORSE BREEDER","FORESTER","RANCHING","FARM MANAGER","FARMER","FARMING","DAIRYMAN","DAIRY FARMER")] <- 1
contributions$Finance[contributions$OCCUPATION %in% c("MERCHANT BANKER","HEDGE FUND MANAGER","FINANCE DIRECTOR","ACCOUNTING","ACTUARY","STOCK BROKER","C.F.O.","MORTGAGE BANKER","STOCKBROKER","FINANCIAL SERVICES","FINANCIAL ANALYST","VENTURE CAPITAL","VENTURE CAPITALIST","INVESTMENT BANKING","BANKER","ACCOUNTANT","FINANCE","INVESTMENT BANKER","FINANCIAL ADVISOR","CFO","CPA","CHIEF FINANCIAL OFFICER","TRADER","BANKING","FINANCIAL CONSULTANT")] <- 1
contributions$Student[contributions$OCCUPATION %in% c("STUDENT","GRADUATE STUDENT")] <- 1
contributions$Creative[contributions$OCCUPATION %in% c("WRITER/DIRECTOR","MUSEUM DIRECTOR","GRAPHIC ARTIST","ART CONSULTANT","GRAPHIC DESIGN","POET","DESIGN","JEWELRY DESIGNER","FILM EDITOR","FILM MAKER","CURATOR","FILM DIRECTOR","SCREENWRITER","LANDSCAPE DESIGNER","ART DIRECTOR","SONGWRITER","TV PRODUCER","SCULPTOR","EDITOR","WRITER/EDITOR","LANDSCAPE ARCHITECT","CREATIVE DIRECTOR","GRAPHIC DESIGNER","ACTRESS","MUSICIAN","ACTOR","ADVERTISING","ART DEALER")] <- 1
  

  
contributions$DON_L1000 <- 0
contributions$DON_G1000 <- 0
contributions$DON_G5000 <- 0

contributions$DON_L1000[contributions$TRANSACTION_AMT <= 1000] <- 1
contributions$DON_G1000[contributions$TRANSACTION_AMT > 1000] <- 1
contributions$DON_G5000[contributions$TRANSACTION_AMT > 5000] <- 1

library(sqldf)

L_1000 <- sqldf("select ZIP_CODE,sum(DON_L1000) as DON_L1000 from contributions group by ZIP_CODE")
G_1000 <- sqldf("select ZIP_CODE,sum(DON_G1000) as DON_G1000 from contributions group by ZIP_CODE")
G_5000 <- sqldf("select ZIP_CODE,sum(DON_G5000) as DON_G5000 from contributions group by ZIP_CODE")
t_All <- sqldf("select ZIP_CODE,count(*) as DON from contributions group by ZIP_CODE")
HomeMaker <- sqldf("select ZIP_CODE,sum(HomeMaker) as HomeMaker from contributions group by ZIP_CODE")
Higher_Prof <- sqldf("select ZIP_CODE,sum(Higher_Prof) as Higher_Prof from contributions group by ZIP_CODE")
Retired <- sqldf("select ZIP_CODE,sum(Retired) as Retired from contributions group by ZIP_CODE")
Professor <- sqldf("select ZIP_CODE,sum(Professor) as Professor from contributions group by ZIP_CODE")
Legal <- sqldf("select ZIP_CODE,sum(Legal) as Legal from contributions group by ZIP_CODE")
Engineer <- sqldf("select ZIP_CODE,sum(Engineer) as Engineer from contributions group by ZIP_CODE")
Medical <- sqldf("select ZIP_CODE,sum(Medical) as Medical from contributions group by ZIP_CODE")
Farmer <- sqldf("select ZIP_CODE,sum(Farmer) as Farmer from contributions group by ZIP_CODE")
Finance <- sqldf("select ZIP_CODE,sum(Finance) as Finance from contributions group by ZIP_CODE")
Student <- sqldf("select ZIP_CODE,sum(Student) as Student from contributions group by ZIP_CODE")
Creative <- sqldf("select ZIP_CODE,sum(Creative) as Creative from contributions group by ZIP_CODE")

t_All <- merge(t_All, L_1000, by="ZIP_CODE", all.x=TRUE)
t_All <- merge(t_All, G_1000, by="ZIP_CODE", all.x=TRUE)
t_All <- merge(t_All, HomeMaker, by="ZIP_CODE", all.x=TRUE)
t_All <- merge(t_All, Higher_Prof, by="ZIP_CODE", all.x=TRUE)
t_All <- merge(t_All, Retired, by="ZIP_CODE", all.x=TRUE)
t_All <- merge(t_All, Professor, by="ZIP_CODE", all.x=TRUE)
t_All <- merge(t_All, Engineer, by="ZIP_CODE", all.x=TRUE)
t_All <- merge(t_All, Legal, by="ZIP_CODE", all.x=TRUE)
t_All <- merge(t_All, Medical, by="ZIP_CODE", all.x=TRUE)
t_All <- merge(t_All, Farmer, by="ZIP_CODE", all.x=TRUE)
t_All <- merge(t_All, Finance, by="ZIP_CODE", all.x=TRUE)
t_All <- merge(t_All, Student, by="ZIP_CODE", all.x=TRUE)
t_All <- merge(t_All, Creative, by="ZIP_CODE", all.x=TRUE)
t_All <- merge(t_All, G_5000, by="ZIP_CODE", all.x=TRUE)


library(gdata)

crosswalk <- read.xls("ZIP_TRACT_CROSSWALK.xlsx")
crosswalk <- crosswalk[-1,]
t_All <- merge(t_All, crosswalk, by.x="ZIP_CODE",by.y="ZIP", all.x=TRUE)
t_All_no_match <- t_All[is.na(t_All$TRACT),]
t_All_match <- t_All[!is.na(t_All$TRACT),]

#Aggregate and calculations

t_All_match$DON_T <- t_All_match$DON * t_All_match$RES_RATIO
t_All_match$L_1000_T <- t_All_match$DON_L1000 * t_All_match$RES_RATIO
t_All_match$G_1000_T <- t_All_match$DON_G1000 * t_All_match$RES_RATIO
t_All_match$G_5000_T <- t_All_match$DON_G5000 * t_All_match$RES_RATIO
t_All_match$HomeMaker_T <- t_All_match$HomeMaker * t_All_match$RES_RATIO
t_All_match$Higher_Prof_T <- t_All_match$Higher_Prof * t_All_match$RES_RATIO
t_All_match$Retired_T <- t_All_match$Retired * t_All_match$RES_RATIO
t_All_match$Professor_T <- t_All_match$Professor * t_All_match$RES_RATIO
t_All_match$Legal_T <- t_All_match$Legal * t_All_match$RES_RATIO
t_All_match$Medical_T <- t_All_match$Medical * t_All_match$RES_RATIO
t_All_match$Farmer_T <- t_All_match$Farmer * t_All_match$RES_RATIO
t_All_match$Finance_T <- t_All_match$Finance * t_All_match$RES_RATIO
t_All_match$Engineer_T <- t_All_match$Engineer * t_All_match$RES_RATIO
t_All_match$Student_T <- t_All_match$Student * t_All_match$RES_RATIO
t_All_match$Creative_T <- t_All_match$Creative * t_All_match$RES_RATIO


t_All_match <- t_All_match[,c("TRACT","DON_T","L_1000_T","G_1000_T","G_5000_T","HomeMaker_T","Higher_Prof_T","Retired_T","Professor_T","Legal_T","Medical_T","Farmer_T","Finance_T","Engineer_T","Student_T","Creative_T")]

options(scipen=500)
t_All_match_TRACT <- sqldf("select TRACT,sum(DON_T) as DON,  sum(L_1000_T) as L_1000,	sum(G_1000_T) as G_1000,	sum(G_5000_T) as G_5000,	sum(HomeMaker_T) as HomeMaker,	sum(Higher_Prof_T) as Higher_Prof,	sum(Retired_T) as Retired,	sum(Professor_T) as Professor,	sum(Legal_T) as Legal,	sum(Medical_T) as Medical,	sum(Farmer_T) as Farmer,	sum(Finance_T) as Finance,	sum(Engineer_T) as Engineer,	sum(Student_T) as Student,	sum(Creative_T) as Creative from t_All_match group by TRACT")



#Convert the 2000 tract in 2010 tracts

#Import lookup (http://www.s4.brown.edu/us2010/Researcher/LTBDDload/DataList.aspx)
#The reference for technical documentation is John R. Logan, Zengwang Xu, and Brian Stults. 2012. “Interpolating US Decennial Census Tract Data from as Early as 1970 to 2010: A Longitudinal Tract Database” Professional Geographer, forthcoming.

Tract_2000_2010 <- read.csv("/Users/alex/Dropbox/geodemo/External Validation/crosswalk_2000_2010.csv")
Tract_2000_2010 <- Tract_2000_2010[,1:3]
t_All_match_TRACT <- merge(t_All_match_TRACT, Tract_2000_2010, by.x="TRACT",by.y="trtid00", all.x=TRUE)
t_All_match_TRACT <- t_All_match_TRACT[!is.na(t_All_match_TRACT$weight),]

#Multiply 2000 tract scores up to 2010
v_list <- names(t_All_match_TRACT[2:16])

tmp_all <- t_All_match_TRACT[1]

for (i in 1:length(v_list)){
tmp <- with(t_All_match_TRACT, t_All_match_TRACT[,paste(v_list[i])] * t_All_match_TRACT[18])
names(tmp) <- paste(v_list[i])
tmp_all <- cbind(tmp_all,tmp)
remove(tmp)
}

t_All_match_TRACT <- merge(t_All_match_TRACT, tmp_all, by.x="trtid00",by.y="TRACT",in.x=TRUE)

#Aggregate 2010 scores
options(scipen=500)
t_All_match_TRACT_2010 <- sqldf("select trtid10,sum(DON) as DON,  sum(L_1000) as L_1000,  sum(G_1000) as G_1000,	sum(G_5000) as G_5000,	sum(HomeMaker) as HomeMaker,	sum(Higher_Prof) as Higher_Prof,	sum(Retired) as Retired,	sum(Professor) as Professor,	sum(Legal) as Legal,	sum(Medical) as Medical,	sum(Farmer) as Farmer,	sum(Finance) as Finance,	sum(Engineer) as Engineer,	sum(Student) as Student,	sum(Creative) as Creative from t_All_match_TRACT group by trtid10")

save(t_All_match_TRACT_2010,file="/Users/alex/Dropbox/geodemo/External Validation/RData_Processed/Donations_Tract.RData")












##############################################################################################################
##############################################################################################################
##############################################################################################################


#######################################Crime Chicago###################################################
###############################################################################################################

f <- c("numeric","character",rep("numeric",32))
crime <- read.csv("/Users/alex/Dropbox/geodemo/External Validation/chicago crime/chicago all crime 12 months prior to june 2013.csv",colClasses=f)

save(crime,file="Chicago_Crime_Tract.RData")
