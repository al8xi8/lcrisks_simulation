
screening <- function(inp, age, s, d, RNDgroup, scrstage, scredage, scrpky, scrqt, numScrn, cessprob, coverage,birthcohort,id,plcorisk){  
 # set.seed(id)
  
  inp1=inp
  sex = inp1[[1]] #1=male; 2=female
  LCAge = inp1[[2]]
  dthAge_LC_NS = inp1[[3]]
  dthAge_OC_NS = inp1[[4]]
  dthAge_NS = inp1[[5]]
  LCType = inp1[[6]]
  LCStage = inp1[[7]]
  burden_NS = inp1[[8]]#only treatment burden
  overdiagnosis = inp1[[9]]
  PST = as.numeric(inp1[[10]])
  pky = as.numeric(inp1[[11]])
  curryearsquit = as.numeric(inp1[[12]])
  currAge = age
  startAge = age
  
  #calculate plco risks by age (0:100)
  cpd = ifelse(is.na(inp1[[23]]),0,inp1[[23]])
  race = inp1[[24]]+1
  edu = inp1[[14]]
  bmi = inp1[[15]]
  copd = inp1[[17]]
  ph = inp1[[18]]
  fh = inp1[[16]]
  agestart = ifelse(inp1[[13]]=="nonsmoker",0,inp1[[19]])
  agequit = ifelse(inp1[[13]]=="nonsmoker",0,inp1[[20]])
  
  current=rep(0,101) #age 0 to 100
  former=rep(0,101)
  intensity=rep(0,101)
  if(agestart>0){ #ever smokers
    current[(agestart+1):(agequit+1)]=1
    former[(agequit+2):101]=1
    intensity[(agestart+1):101]=cpd #keep cpd value after quitting for former smokers
    currsmkdur = cumsum(current)
  }
  else{ #never smokers
    intensity=rep(0,101)
    currsmkdur = rep(0,101)
    curryearsquit = rep(0,101)
    pky = rep(0,101)
  }
  
  plco = plcorisk[[1]]
  
  #read in cessation prob from SHG
  # if(birthcohort>0){
  #   if(sex==1){
  #     cessprob = (cessprob-1)*cess_m[,paste0(birthcohort)]
  #   }
  #   else{
  #     cessprob = (cessprob-1)*cess_f[,paste0(birthcohort)]
  #   }
  # }
  cessprob_age=cessprob/100
  #simulation duration  
  
  if(LCAge!=0){
    end = min(startAge+simTime, dthAge_NS, LCAge, 100)
  }
  else{
    end = min(startAge+simTime, dthAge_NS, 100)
  }
  
  diagdth = 0
  dthAge = dthAge_NS
  dthAge_LC = dthAge_LC_NS
  dthAge_OC = dthAge_OC_NS
  
  quitted = 0
  count_cess = 0
  eligible=0  
  stage = 1
  burden=burden_NS
  scryear = 1
  detected = 0
  survTime = 0
  fuy = matrix(NA,nrow=1,ncol=numScrn)
  tp=NA #-999 did not screen; 0 screen but negative; 1i screen but false positive; 2i screen and true positive; i: 1-8 follow-up test number
  fp=matrix(NA,ncol=8,nrow=1)
  
  counter=0
  
  #check coverage - 0, 10, 15, 20
  
  if(coverage==0){noscrn=1}
  else{noscrn=0}
  
  rprob = runif(1)
  if(rprob<coverage/100){covered=1}
  else{covered=0}
  
  while(currAge<end && detected == 0 && diagdth == 0 && scryear<=numScrn){
     #screening coverage = 0.4
    if( noscrn==0 && covered==1 && 
        (class(scrpky)=="numeric" && currAge>(scrstage-1) && currAge<(scredage+1) && (pky[currAge+1]+0.01)>scrpky && curryearsquit[currAge+1]<(scrqt+1)) || 
        (scrpky=="NELSON" && currAge>(scrstage-1) && currAge<(scredage+1) && curryearsquit[currAge+1]<(scrqt+1) && ((!is.na(intensity[currAge+1]) && intensity[currAge+1]>15 && currsmkdur[currAge+1]>25)||(!is.na(intensity[currAge+1]) && intensity[currAge+1]>10 && currsmkdur[currAge+1]>30))) ||
        (scrpky=="PLCO" && currAge>(scrstage-1) && currAge<(scredage+1) && plco[currAge+1]>=scrqt) ||
        (scrpky=="Pilot" && currAge>(scrstage-1) && currAge<(scredage+1) && curryearsquit[currAge+1]<(scrqt+1) && ((!is.na(intensity[currAge+1]) && intensity[currAge+1]>=15 && currsmkdur[currAge+1]>=25)||(!is.na(intensity[currAge+1]) && intensity[currAge+1]>=10 && currsmkdur[currAge+1]>=30)||(pky[currAge+1]+0.01)>20))){
      eligible = 1
      counter = counter+1
      output=getscreen(scryear, RNDgroup, d, s, currAge, overdiagnosis, sex, LCAge, LCType, LCStage, list(PST), dthAge_LC, dthAge_OC, dthAge, burden,id)
      LCAge=output[2]
      LCStage=output[3]
      survTime=output[4]
      death=output[5] 
      diagdth=output[6]
      dthAge=output[7]
      dthAge_LC=output[8]
      detected=output[9]
      tp=output[10]
      fp=output[11:18]
      fuy[9*(currAge-scrstage)+1]=tp
      fuy[(9*(currAge-scrstage)+2):(9*(currAge-scrstage)+9)] = fp
      #decide if the person quit smoking or not
      if(detected==0 & quitted==0 & diagdth==0 & curryearsquit[currAge+1]==0 & pky[currAge+1]>0 & scryear==1){count_cess=1}
      rprob = runif(1)#cessation probability=0.05, 0.10, 0.15, 0.20, 0.25
      # if(length(cessprob)>0){
      #   cessprob_age=cessprob[currAge]
      # }
      # else{
      #   cessprob_age=cessprob
      # }
      if(detected==0 & quitted==0 & diagdth==0 & curryearsquit[currAge+1]==0 & pky[currAge+1]>0 & rprob<=cessprob_age & scryear==1){ #allow quitting at the first screen
        quitted = 1
        output2 = newages(sex,list(pky),list(PST),currAge,LCAge,LCType,LCStage,dthAge_LC,dthAge_OC,id)
        LCAge = output2[[1]]
        dthAge_LC = output2[[2]] 
        dthAge_OC = output2[[3]] 
        dthAge = output2[[4]]
        LCType = output2[[5]]
        LCStage = output2[[6]]
        PST = output2[[7]]
        pky = output2[[8]]
        curryearsquit = output2[[9]]
        burden = output2[[10]]
        if(LCAge!=0){
          end = min(startAge+simTime, dthAge, LCAge, 100)
        }
        else{
          end = min(startAge+simTime, dthAge, 100)
        }
      }
    }
    else if( noscrn==1 && currAge>(scrstage-1) && currAge<(scredage+1) && (pky[currAge+1]+0.01)>scrpky && curryearsquit[currAge+1]<(scrqt+1)){
      eligible = 1
      counter = counter+1
      #decide if the person quit smoking or not
      if(detected==0 & quitted==0 & diagdth==0 & curryearsquit[currAge+1]==0 & pky[currAge+1]>0 & scryear==1){count_cess=1}
      rprob = runif(1)#cessation probability=0.05, 0.10, 0.15, 0.20, 0.25
      # if(length(cessprob)>0){
      #   cessprob_age=cessprob[currAge]
      # }
      # else{
      #   cessprob_age=cessprob
      # }
      if(detected==0 & quitted==0 & diagdth==0 & curryearsquit[currAge+1]==0 & pky[currAge+1]>0 & rprob<=cessprob_age & scryear==1){ #allow quitting at the first screen
        quitted = 1
        output2 = newages(sex,list(pky),list(PST),currAge,LCAge,LCType,LCStage,dthAge_LC,dthAge_OC,id)
        LCAge = output2[[1]]
        dthAge_LC = output2[[2]] 
        dthAge_OC = output2[[3]] 
        dthAge = output2[[4]]
        LCType = output2[[5]]
        LCStage = output2[[6]]
        PST = output2[[7]]
        pky = output2[[8]]
        curryearsquit = output2[[9]]
        burden = output2[[10]]
        if(LCAge!=0){
          end = min(startAge+simTime, dthAge, LCAge, 100)
        }
        else{
          end = min(startAge+simTime, dthAge, 100)
        }
      }
    }
    ###############################################
    #Update screening year                        #
    ###############################################
    if(currAge>(scrstage-1) && currAge<(scredage+1) && counter>=1){scryear = scryear+1}
    ###########################################
    #Update age and smoking status            #
    ###########################################
    currAge = currAge+1
  }
  #manage death age
  if(detected==1&overdiagnosis==0){
    #update current age
    if(dthAge_LC==0|dthAge_LC>=dthAge_OC){
      if(dthAge_OC<=min(startAge+simTime, 100)){dthAge=dthAge_OC}
      else{dthAge=0}
    }
    else{
      if(dthAge_LC<=min(startAge+simTime, 100)){dthAge=dthAge_LC}
      else{dthAge=0}
    }
  }
  return(c(LCAge, dthAge_LC, dthAge_OC, dthAge, LCType, LCStage, 0, burden, eligible, overdiagnosis, detected, survTime, diagdth, quitted, count_cess, fuy))
}
