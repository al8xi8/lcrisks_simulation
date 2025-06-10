
### Simulating Lung Cancer Screening and No-Screening Scenarios Using the LCrisks Model and Split NHIS Input Data ###

rm(list = ls())
library(lcrisks)

# --- Load Functions ---
getwd()
setwd("/Users/al8xi8/Documents/GitHub/lcrisks_simulation")

source("scripts/source/LCSpecMortality.R")
source("scripts/source/histology.R")
source("scripts/source/followup_new.R")
source("scripts/source/treatment.R")
source("scripts/source/lifetable_LCRAT.R")
source("scripts/source/screening_core.R")
source("scripts/source/screening_newages.R")
source("scripts/source/stage_pst.R")
source("scripts/source/global_lungrads.R")

path1 = ""

#regular quit year constraint
source("scripts/source/screening.R")
folder = ""

LCSmodel_noscrn <- function(s, o, d, k){#s: specificity; o: overdiagnosis; d: burden; k: # of repetition
  
  #AC
  #modify k1 to read in the split data correctly from 000 to 191
  k <- 0
  if (k < 10) {
    k1 = paste0("00", k)
  } else if (k < 100) {
    k1 = paste0("0", k)
  } else {
    k1 = as.character(k)
  }  
  #AC
  #revise this part to read in our own NHIS split data:
  inp = read.table(
    file = paste0("datasets/split_nhis/nhis_", k1, ".csv"),
    header = FALSE,
    sep = ",",
    stringsAsFactors = FALSE,
    colClasses = rep("numeric", 24)  # all 24 columns are numeric
  )
  
  N = N_ldct = N_cxr = length(inp[,1])
  #initiating output matrix of the no screening arm
  #input data names
  # [1] "sex"                   "ID"                    "age"                   "smoke_status"         
  # [5] "education"             "bmi"                   "family_history_binary" "copd"                 
  # [9] "history_cancer"        "age_start"             "age_stop"              "years_smoked"         
  # [13] "quit_years"            "intensity"             "race.lcrisk"           "Hypertension"         
  # [17] "Coronary"              "Angina"                "Heart"                 "Otherheart"           
  # [21] "Stroke"                "Diabetes"              "Chronic"               "kidneys"              
  # [25] "Liver"                 "Health"                "Year"  
  #out1 = c("Age at entry", "Lung cancer age", "Death age-LC", "Death age-OC", "Death age", "Lung cancer type", "Lung cancer stage", "Quality of life", "Burden", "End of life Prob of LC", "Baseline Prob of LC")
  out1 = matrix(0,ncol = 217, nrow = 1, byrow = TRUE)
  #######################################################################
  #                           No screening arm                          #
  #######################################################################
  for (i in 1:N){#1:N
    output1 = c()
    output1 = lifetable(inp[i,],d)
    #lifetable return:
    #return(c(sex, LCAge, dthAge_LC, dthAge_OC, dthAge, LCType, LCStage, burden, lc_prob, list(PST[1:6]), list(pky),list(curryearsquit)))
    out1 = rbind(out1, as.numeric(unlist(output1[1:12])))
  }
  
  write.table(out1, paste("output_noscrn/no_scrn_",k,".csv",sep=""), row.names = FALSE, col.names = FALSE, sep = ",")
}


#######################################################################
#                           screening function                        #
#######################################################################
LCSmodel_scrn <- function(s, o, d, k, cessprob, coverage, birthcohort, scrstage, scredage, scrpky, scrqt){  
  if(birthcohort==1960){ #using 1960 for now;
    init_f <<- 1.13
    init_m <<- 1.26
  }
  else if(birthcohort==1950){
    init_f <<- 1.54
    init_m <<- 1.65
  }
  
  
  out2 = matrix(0,ncol = 17, nrow = 1, byrow = TRUE)
  fuy2 = matrix(0,ncol = 9*(scredage-scrstage+1), nrow = 1, byrow = TRUE)
  simTime=100
  
  #read split data in to get starting age
  k1=k
  inp=read.table(file=paste("split_cessage/Francedata_",k1,".csv",sep=""),header=FALSE,sep=',',stringsAsFactors = FALSE,colClasses = c(c("character","numeric","integer","character",rep("numeric",11)),rep("NULL",29)))
  
  #input parameter list for screening() function
  input2 = read.csv(paste0(path1,"output_noscrn_ocdfromlcrisk_cessage/no_scrn_",k,".csv"),sep=",",header=T)
  input2 = cbind(input2[,1:9],0,input2[,10:ncol(input2)])
  
  #read in plco risk for each individual
  plcorisk = read.csv(paste0("risk_plco/risk_",k,".csv"),sep=",",header=T)

  #######################################################################
  #                           Overdiagnosis                             #
  #######################################################################
  id = c()
  #LC probability at the end of life
  prob = c()
  for (i in 1:nrow(input2)){
    if(input2[i,2]==0){
      id = c(id,i)
      prob = c(prob, input2[i,9])
    }
  }
  ovd = sample(id,round(over[o]*length(input2[input2[,2],1])),prob=prob)
  if(length(ovd)>0){
    #reassigning variables
    for (i in 1:length(ovd)){
      input2[[ovd[i],10]]=1 #assign variable overdiagnosis to 1
      input2[[ovd[i],2]] = input2[ovd[i],5] #assign LCAge to dthAge_NS
      input2[[ovd[i],6]] = 2 #LC type is adenocarcinoma, which includes BAC
      input2[[ovd[i],7]] = 1 #LC stage is 1
      #assigning preclinical sojourn time in stage 1 based on gender
      if(input2[[ovd[i],1]]==1) {input2[[ovd[i],11]][1] = rweibull(1, b, 2*MPSTMaleAD[1]/gamma(1+1/b))} #male
      else{input2[[ovd[i],11]][1] = rweibull(1, b, 2*MPSTFemaleAD/gamma(1+1/b))} #female
    }
  }
  
  #######################################################################
  #                       LDCT Screening arm                            #
  #######################################################################
  for (i in 1:nrow(input2)){
    age=inp[i,3]
    #USPSTF
    #    print(i)
    id=10000*k+i
    numScrn = 9*(scredage-scrstage+1)
    output2 = screening(c(input2[i,1:8],input2[i,10],list(input2[i,11:16]),list(input2[i,17:117]),list(input2[i,118:218]),inp[i,4:15]), age, s, d, 1,scrstage, scredage, scrpky, scrqt, numScrn, cessprob,coverage,birthcohort,id,list(plcorisk[i,]))
    out2 = rbind(out2, as.numeric(c(input2[i,1], output2[1:15],age)))
    fuy2 = rbind(fuy2, output2[16:(16+numScrn-1)])
  }
  #folder="output_55803015/"
  write.table(out2, paste0(folder,"output_scrn_lcriskocd/scrn_tf",s,o,d,"_",simTime,"_",scrstage,"_",scredage,"_",scrpky,"_",scrqt,"_",coverage,"_",cessprob,"_",k,".csv"), row.names = FALSE, col.names = FALSE, sep = ",")
  write.table(fuy2, paste0(folder,"output_scrn_lcriskocd/fuy_tf",s,o,d,"_",simTime,"_",scrstage,"_",scredage,"_",scrpky,"_",scrqt,"_",coverage,"_",cessprob,"_",k,".csv",sep=""), row.names = FALSE, col.names = FALSE, sep = ",")
}

library(parallel)

ncores <- 100

cl <- makeCluster(ncores,type="FORK")

#s, o, d, k, cessprob, coverage, birthcohort, scrstage, scredage, scrpky, scrqt
#Pack year
#a=cbind(scrstage,scredage,scrpky,scrqt)
#clusterMap(cl, LCSmodel_scrn, k=rep(c(0:176),24), scrstage=rep(c(50,50,55,55),each=177*6), scredage=rep(c(74,80,74,80),each=177*6), scrpky=rep(rep(c(20,30),each=177*3),4), scrqt=rep(rep(c(10,15,99),each=177),8), MoreArgs = list(s=1, o=1, d=1, cessprob=0, coverage=100, birthcohort=1960))

# clusterMap(cl, LCSmodel_scrn, k=rep(c(0:9),24), scrstage=rep(c(50,50,55,55),each=10*6), scredage=rep(c(74,80,74,80),each=10*6), scrpky=rep(rep(c(20,30),each=10*3),4), scrqt=rep(rep(c(10,15,99),each=10),8), MoreArgs = list(s=1, o=1, d=1, cessprob=0, coverage=100, birthcohort=1960))
#nelson
# clusterMap(cl, LCSmodel_scrn, k=rep(c(0:176),12), scrstage=rep(c(50,50,55,55),each=177*3), scredage=rep(c(74,80,74,80),each=177*3), scrqt=rep(rep(c(10,15,99),each=177),4), MoreArgs = list(s=1, o=1, d=1, cessprob=0, coverage=100, birthcohort=1960,scrpky="NELSON"))
#plco
# clusterMap(cl, LCSmodel_scrn, k=rep(c(0:176),12), scrstage=rep(c(50,50,55,55),each=177*3), scredage=rep(c(74,80,74,80),each=177*3), scrqt=rep(rep(c(0.01,0.015,0.02),each=177),4), MoreArgs = list(s=1, o=1, d=1, cessprob=0, coverage=100, birthcohort=1960, scrpky="PLCO"))

# clusterMap(cl, LCSmodel_scrn, k=rep(c(0:176),12), scrstage=rep(c(50,50,55,55),each=177*3), scredage=rep(c(74,80,74,80),each=177*3), scrqt=rep(rep(c(0.005, 0.0072, 0.008),each=177),4), MoreArgs = list(s=1, o=1, d=1, cessprob=0, coverage=100, birthcohort=1960, scrpky="PLCO"))

#pilot
# clusterMap(cl, LCSmodel_scrn, k=rep(c(0:176),2), scredage=rep(c(74,80),each=177), MoreArgs = list(s=1, o=1, d=1, cessprob=0, coverage=100, birthcohort=1960,  scrstage=50,  scrqt=15, scrpky="Pilot"))

stopCluster(cl)