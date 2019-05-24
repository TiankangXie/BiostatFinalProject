rm(list = ls())

#Change to your dir
setwd("F:\\SP2019\\Biostat2\\Final")

#Read in for the data
#================================================================
library(readr)
library(dplyr)

info_NIS <- read_fwf('https://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/FileSpecifications_NIS_2016_Core.TXT', 
                     fwf_positions(start = c(1, 5, 10, 37, 41, 71, 75, 79, 81, 86), 
                                   end = c(3, 8, 35, 39, 69, 73, 77, 79, 84, 185)),  #use NA here for ragged (file ends abruptly when NA in last column)
                     skip = 20) #skip = lines to skip till beggining reading the file


head(info_NIS)


NIS <- read_fwf('NIS_2016_Core.ASC', fwf_positions(start = info_NIS$X6, end = info_NIS$X7, col_names = info_NIS$X5)) 





info_HOSPITAL <- read_fwf('https://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/FileSpecifications_NIS_2016_Hospital.TXT', 
                          fwf_positions(start = c(1, 5, 10, 37, 41, 71, 75, 79, 81, 86), 
                                        end = c(3, 8, 35, 39, 69, 73, 77, 79, 84, 185)),  #use NA here for ragged (file ends abruptly when NA in last column)
                          skip = 20) #skip = lines to skip till beggining reading the file


head(info_HOSPITAL)

HOSPITAL <- read_fwf('NIS_2016_Hospital.ASC', fwf_positions(start = info_HOSPITAL$X6, end = info_HOSPITAL$X7, col_names = info_HOSPITAL$X5))   

head(HOSPITAL)

#Making the first object that will hold the SEVERITY file information:

info_SEVERITY <- read_fwf('https://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/FileSpecifications_NIS_2016_Severity.TXT', 
                          fwf_positions(start = c(1, 5, 10, 37, 41, 71, 75, 79, 81, 86), 
                                        end = c(3, 8, 35, 39, 69, 73, 77, 79, 84, 185)),  #use NA here for ragged (file ends abruptly when NA in last column)
                          skip = 20) #skip = lines to skip till beggining reading the file


info_SEVERITY


# Loading the SEVERITY dataset:
#SEVERITY <- read_fwf('NIS_2016_Severity.ASC', fwf_positions(start = info_SEVERITY$X6, end = info_SEVERITY$X7, col_names = info_SEVERITY$X5))   
#head(SEVERITY)
#NIS$I10_PR1
#==================================================



#=================================================
#Dropping unnecessary variables in the core file:
keep <- names(NIS) %in% c("AGE", "AWEEKEND", "DISPUNIFORM", "DQTR", "DRG", "ELECTIVE", "FEMALE", "HCUP_ED", "HOSP_NIS", "I10_DX1", "I10_DX2", "I10_DX3", "I10_DX4", "I10_DX5", "I10_DX6", "I10_DX7", "I10_DX8", "I10_DX9", "I10_DX10", "I10_DX11", "I10_DX12", "I10_DX13", "I10_DX14", "I10_DX15", "I10_DX16", "I10_DX17", "I10_DX18", "I10_DX19", "I10_DX20", "I10_DX21", "I10_DX22", "I10_DX23", "I10_DX24", "I10_DX25", "I10_DX26", "I10_DX27", "I10_DX28", "I10_DX29", "I10_DX30", "I10_NDX", "I10_NPR", "I10_PR1", "I10_PR2", "I10_PR3", "I10_PR4", "I10_PR5", "I10_PR6", "I10_PR7", "I10_PR8", "I10_PR9", "I10_PR10", "I10_PR11", "I10_PR12", "I10_PR13", "I10_PR14", "I10_PR15", "KEY_NIS", "LOS", "PAY1", "PL_NCHS", "RACE", "TOTCHG", "TRAN_IN", "YEAR", "ZIPINC_QRTL","NIS_STRATUM" ) 
NIS <- NIS[keep]


#Dropping unnecessary variables in the hospital file:
keeph <- names(HOSPITAL) %in% c("HOSP_NIS", "TOTAL_DISC")
HOSPITAL <- HOSPITAL[keeph]

#keeps <- names(SEVERITY) %in% c("HOSP_NIS", "APRDRG_Severity", "APRDRG_Risk_Mortality", "KEY_NIS")
#SEVERITY <- SEVERITY[keeps]

#Merging the hospital file:
NIS <- inner_join(NIS, HOSPITAL, by = "HOSP_NIS")




#Merging the severity file:
NIS <- inner_join(NIS, SEVERITY, by = "KEY_NIS")




NIS[,"Census_Division"] <- NIS[,"NIS_STRATUM"] %/% 1000
NIS[,"Control"] <- (NIS[,"NIS_STRATUM"] %% 1000) %/% 100
NIS[,"LocTeach"] <- ((NIS[,"NIS_STRATUM"] %% 1000) %% 100) %/% 10
NIS[,"Bedize"] <- (((NIS[,"NIS_STRATUM"] %% 1000) %% 100) %% 10) %/% 1

#sss = NIS2[,c("NIS_STRATUM","Census_Division","Control")]

rm(list = c("HOSPITAL","SEVERITY"))

#Restrict the data to women only
NIS = NIS[NIS$FEMALE==1,]



#Filter Z values
list.vals = c("Z370","Z3759", "Z371", "Z3760",  "Z372", "Z3761" , "Z373" , "Z3762" , "Z374" , "Z3763" , "Z3764" , "Z3751" , "Z3769" , "Z3752" , "Z377" , "Z3753" , "Z379", "Z3754","Z23")

rs = apply(NIS, 1, function(r) any(r %in% list.vals))

NIS = NIS[rs,]
colnames(NIS)[9] = "HOSP_NIS.x"


#NIS$SEVERITY = ifelse(NIS$APRDRG_Severity>=2,1,0)

#Manually select 150 samples from very balanced dataset
set.seed(1)
NIS_short = as.data.frame(NIS[1,])
for(i in 1:3){
  for(j in 1:3){
    for(k in 1:3){
      dataset_tmp = NIS$HOSP_NIS.x[which(NIS$Control==i & NIS$LocTeach ==j & NIS$Bedize ==k)]
      #print(i)
      #print(j)
      #print(k)
      store_hospital = dataset_tmp[sample(length(dataset_tmp), size=150, replace =F)]
      #In  total we sampled 405 hospitals
      new_frame = NIS[which(NIS$HOSP_NIS.x %in% store_hospital),]
      #new_frame2 = new_frame[sample(nrow(new_frame), size = round(0.2*nrow(new_frame)), replace = F),]
      NIS_short = rbind(NIS_short,new_frame)
      #print('````````````````````````')
    }
  }
}

NIS_short = NIS_short[-1,]
rm(list = c("NIS"))


vag_del = c('10D07Z3','10D07Z4','10D07Z5','10D07Z6','10D07Z7','10D07Z8','10E0XZZ')
ins_del = c('10D07Z3','10D07Z6','10D07Z4','10D07Z8','10D07Z5')


NIS_short_vagdel = NIS_short %>% filter_at(vars(starts_with("I10_PR")), any_vars(. %in% vag_del))
NIS_short_insdel = NIS_short %>% filter_at(vars(starts_with("I10_PR")), any_vars(. %in% ins_del))

#Compute rate
rate_by_ID = c()

#This is the code for the trauma
codesm1<- c("O702", "O7022", "O7020", "O7023", "O7021", "O703")

#Compute Rate for Vag Delivery
NIS_short_sub1 <- NIS_short_vagdel %>% filter_at(vars(starts_with("I10_DX")), any_vars(. %in% codesm1))
hospitals_ID = unique(NIS_short_vagdel[,'HOSP_NIS.x'])

for(i in hospitals_ID){
  rate_by_ID = c(rate_by_ID,sum(NIS_short_sub1$HOSP_NIS.x == i) / sum(NIS_short_vagdel$HOSP_NIS.x == i))
}

NIS_short_vagdel$rate = NA
for(i in 1:nrow(NIS_short_vagdel)){
  idx = which(NIS_short_vagdel$HOSP_NIS.x[i] == hospitals_ID)
  NIS_short_vagdel$rate[i] = rate_by_ID[idx]
}
#---------------------------------------------------------------------------------------------------
#Compute rate for the instrumental delivery
rate_by_ID2 = c()

codesm1<- c("O702", "O7022", "O7020", "O7023", "O7021", "O703")

NIS_short_sub1 <- NIS_short_insdel %>% filter_at(vars(starts_with("I10_DX")), any_vars(. %in% codesm1))
hospitals_ID = unique(NIS_short_insdel[,'HOSP_NIS.x'])

for(i in hospitals_ID){
  rate_by_ID2 = c(rate_by_ID2,sum(NIS_short_sub1$HOSP_NIS.x == i) / sum(NIS_short_insdel$HOSP_NIS.x == i))
}

NIS_short_insdel$rate = NA
for(i in 1:nrow(NIS_short_insdel)){
  idx = which(NIS_short_insdel$HOSP_NIS.x[i] == hospitals_ID)
  NIS_short_insdel$rate[i] = rate_by_ID2[idx]
}
NIS_short_insdel$method = "Instrument"
NIS_short_vagdel$method = "Vaginal"
NIS_full = rbind(NIS_short_insdel,NIS_short_vagdel)
NIS_full$rate = NIS_full$rate*1000
NIS_full$AGE[26690] = 66
NIS_full = NIS_full[-which(NIS_full$AGE ==0),]


library(ggplot2)

#===============================================================
#Plotting the diagrams
#AGE
NSS = NIS_full %>%
  group_by(method,AGE) %>%
  summarise(mean = mean(rate))

ggplot(NSS, aes(fill=method, y=mean, x=AGE)) + 
  geom_bar(position="dodge", stat="identity")+
  ggtitle(label = "Histogram of Third and fourth degree obstetric trauma diagnosis rate")+
  xlab("Age")+
  ylab("Trauma Rate per 1000")+
  scale_fill_discrete(name = "Delivery Method")+
  theme(plot.title = element_text(size=10))

#Control
NSS = NIS_full %>%
  group_by(method,Control) %>%
  summarise(mean = mean(rate))

ggplot(NSS, aes(fill=method, y=mean, x=as.factor(Control))) + 
  geom_bar(position="dodge", stat="identity")+
  ggtitle(label = "Histogram of Third and fourth degree obstetric trauma diagnosis rate")+
  xlab("Control Type")+
  ylab("Trauma Rate per 1000")+
  scale_fill_discrete(name = "Delivery Method")+
  theme(plot.title = element_text(size=10))+
  scale_x_discrete(labels=c("1" = "Govt","2" = "Prv_NonProfit","3" = "Prv_Profit"))

#LocTeach
NSS = NIS_full %>%
  group_by(method,LocTeach) %>%
  summarise(mean = mean(rate))

ggplot(NSS, aes(fill=method, y=mean, x=as.factor(LocTeach))) + 
  geom_bar(position="dodge", stat="identity")+
  ggtitle(label = "Histogram of Third and fourth degree obstetric trauma diagnosis rate")+
  xlab("Teaching Status")+
  ylab("Trauma Rate per 1000")+
  scale_fill_discrete(name = "Delivery Method")+
  theme(plot.title = element_text(size=10))+
  scale_x_discrete(labels=c("1" = "Rural","2" = "Urb_Nonteach","3" = "Urb_teach"))

#size
NSS = NIS_full %>%
  group_by(method,Bedize) %>%
  summarise(mean = mean(rate))

ggplot(NSS, aes(fill=method, y=mean, x=as.factor(Bedize))) + 
  geom_bar(position="dodge", stat="identity")+
  ggtitle(label = "Histogram of Third and fourth degree obstetric trauma diagnosis rate")+
  xlab("Size")+
  ylab("Trauma Rate per 1000")+
  scale_fill_discrete(name = "Delivery Method")+
  theme(plot.title = element_text(size=10))+
  scale_x_discrete(labels=c("1" = "Small","2" = "Medium","3" = "Large"))

#weekend
NSS = NIS_full %>%
  group_by(method,RACE) %>%
  summarise(mean = mean(rate))

ggplot(NSS, aes(fill=method, y=mean, x=as.factor(RACE))) + 
  geom_bar(position="dodge", stat="identity")+
  ggtitle(label = "Histogram of Third and fourth degree obstetric trauma diagnosis rate")+
  xlab("Weekend")+
  ylab("Trauma Rate per 1000")+
  scale_fill_discrete(name = "Delivery Method")+
  theme(plot.title = element_text(size=10))+
  scale_x_discrete(labels=c("-8" = "UNK","-9" = "REFUS","1" = "WHT","2" = "BLK","3" = "HISP","4" = "ASN","5" = "NATIVE","6" = "OTHR"))



#Compute Graphs

library(lme4)
library(lmerTest)
#==================================================================
#P vals, hospital profiling for instrumental delivery
NIS_short_insdel$HOSP_NIS.x = as.factor(NIS_short_insdel$HOSP_NIS.x)

m1 <- lmer(rate ~ AGE  + (1 | HOSP_NIS.x) + as.factor(AWEEKEND)  + as.factor(Control) + as.factor(LocTeach) + as.factor(Bedize) + TOTAL_DISC + as.factor(RACE), data =NIS_short_insdel,nAGQ = 7)

summary(m1)
rand.out1 <- ranef(m1, condVar = TRUE)
# Empirical Bayes â???opredictionâ??? of center effects
EmpBayes.logit1 <- rand.out1$HOSP_NIS.x[,1]

bayes_estimate1 = data.frame(hospital = rownames(rand.out1$HOSP_NIS.x),estimate = EmpBayes.logit1)
write.csv(bayes_estimate1,file = "instrument_bayes.csv")
#==================================================================
#P vals, hospital profiling for nromal delivery
NIS_short_vagdel$HOSP_NIS.x = as.factor(NIS_short_vagdel$HOSP_NIS.x)
m2 <- lmer(rate ~ AGE  + (1 | HOSP_NIS.x) + as.factor(AWEEKEND)  + as.factor(Control) + as.factor(LocTeach) + as.factor(Bedize) + TOTAL_DISC + as.factor(RACE), data =NIS_short_vagdel,nAGQ = 7)
summary(m2)


rand.out2 <- ranef(m2, condVar = TRUE)
# Empirical Bayes â???opredictionâ??? of center effects
EmpBayes.logit2 <- rand.out2$HOSP_NIS.x[,1]
bayes_estimate2 = data.frame(hospital = rownames(rand.out2$HOSP_NIS.x),estimate = EmpBayes.logit2)
write.csv(bayes_estimate2,file = "vaginal_bayes.csv")
# The standard deviation of these predictions:
#summary(m)

