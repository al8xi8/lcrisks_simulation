
### Life Table Function for Lung Cancer Risk Assessment Tool (LCRAT) ###

getwd()
setwd("/Users/al8xi8/Documents/GitHub/lcrisks_simulation")

# --- Load Libraries and Packages ---
install.packages("VGAM")
install.packages("/Users/al8xi8/Documents/GitHub/lcrisks_simulation/lcrisks_package", repos = NULL, type = "source")
library(VGAM)
library(lcrisks)


lifetable<-function(inp,d){ #d-disutilities related to treatment only
  #return a person's life table with respect to clinical and preclinical lung cancer histology and stage, lung cancer related death or other-causes death
  
  qaly_new = 0
  qaly_aLC = 0
  qaly_bLC = 0
  burden = 0
  survTime = 0
  LCType = 0
  LCStage = 0
  LCAge = 0
  dthAge = 0
  dthAge_LC = 0
  dthAge_OC = 0
  PST = c(0,0,0,0,0,0)
  
  #AC
  #Modify the entry number to match the order of the input dataset
  sex = as.numeric(ifelse(inp[2] == 1, 2, 1))  # female = 1 → sex = 2
  currAge = as.numeric(inp[1])
  startAge = currAge #keep the age at entry
  edu = as.numeric(inp[10])
  bmi = as.numeric(inp[9])
  copd = as.numeric(inp[7])
  prshist = as.numeric(inp[11])
  fmhist = as.numeric(inp[8])
  race = as.numeric(inp[6]) #0 white
  
  currsmk = as.numeric(ifelse(inp[4] == 0, 1, 0))  # qtyears = 0 → current smoker
  cpd = as.numeric(ifelse(is.na(inp[5]), 0, inp[5]))  # avecpd
  currsmkdur = as.numeric(ifelse(is.na(inp[3]), 0, inp[3]))  # smkyears
  curryearsquit = as.numeric(ifelse(is.na(inp[4]), 0, inp[4]))  # qtyears
 
  agestart = as.numeric(inp[1] - inp[3])  # age - smkyears
  agequit = as.numeric(inp[1] - inp[4])   # age - qtyears
  
  if(currsmk==0&curryearsquit>0){fmrsmk=1}
  else{fmrsmk=0}
  
  current=rep(0,101) #age 0 to 100
  former=rep(0,101)
  intensity=rep(0,101)
  
  if(currsmk==1|fmrsmk==1){ #ever smokers
    current[(agestart+1):(agequit+1)]=1
    former[(agequit+2):101]=1
    intensity[(agestart+1):101]=cpd #keep cpd value after quitting for former smokers
    smkdur = cumsum(current)
    yrsquit = cumsum(former)
    pky = intensity*smkdur/20
  }
  else{ #never smokers
    intensity=rep(0.1,101)
    smkdur = rep(0.1,101)
    yrsquit = rep(0.1,101)
    pky = rep(0.1,101)
  }
  #set up input for lcrisk function
  #read in comorbidity 
  
  
  end = 100
  if(length(inp)<20){
    age=c(startAge:end)
    female=sex-1
    smkyears=smkdur[(startAge+1):(end+1)]
    qtyears=yrsquit[(startAge+1):(end+1)]
    cpd=intensity[(startAge+1):(end+1)]
    race=race
    fam.lung.trend=fmhist
    bmi=bmi
    edu6=edu
    prior.cancer=prshist
    emp=copd
    hypertension=0
    chd=0
    angina=0
    heartattack=0
    heartdisease=1
    stroke=0
    diab=0
    bron=0
    kidney=0
    liver=0
    speceq=0
    year=as.numeric(inp[27])
    pkyr.cat=smkyears*cpd/20
  }
  else{ 
    age=c(startAge:end)
    female=sex-1
    smkyears=smkdur[(startAge+1):(end+1)]
    qtyears=yrsquit[(startAge+1):(end+1)]
    cpd=intensity[(startAge+1):(end+1)]
    race=race
    fam.lung.trend=fmhist
    bmi=bmi
    edu6=edu
    prior.cancer=prshist
    emp=copd
    #AC
    #Modify the entry number to match the order of the input dataset
    hypertension   = as.numeric(ifelse(is.na(inp[12]), 0, inp[12]))
    chd            = as.numeric(ifelse(is.na(inp[13]), 0, inp[13]))
    angina         = as.numeric(ifelse(is.na(inp[14]), 0, inp[14]))
    heartattack    = as.numeric(ifelse(is.na(inp[15]), 0, inp[15]))
    heartdisease   = as.numeric(ifelse(is.na(inp[16]), 0, inp[16]))
    stroke         = as.numeric(ifelse(is.na(inp[17]), 0, inp[17]))
    diab           = as.numeric(ifelse(is.na(inp[18]), 0, inp[18]))
    bron           = as.numeric(ifelse(is.na(inp[19]), 0, inp[19]))
    kidney         = as.numeric(ifelse(is.na(inp[20]), 0, inp[20]))
    liver          = as.numeric(ifelse(is.na(inp[21]), 0, inp[21]))
    speceq         = as.numeric(ifelse(is.na(inp[22]), 0, inp[22]))
    # hypertension=NA
    # chd=NA
    # angina=NA
    # heartattack=NA
    # heartdisease=NA
    # stroke=NA
    # diab=NA
    # bron=NA
    # kidney=NA
    # liver=NA
    # speceq=NA
    year           = as.numeric(ifelse(is.na(inp[23]), 2020, inp[23]))  # Default to 2020 if missing
    pkyr.cat=smkyears*cpd/20
  }
  
  persons <- as.data.frame(cbind(age, female, smkyears, qtyears, cpd, race, emp, fam.lung.trend, bmi, edu6, prior.cancer, hypertension, chd, angina, heartattack, heartdisease, stroke, diab, bron, kidney, liver, speceq, year, pkyr.cat))
  persons$race=as.factor(race)
  
  avecpd = cpd
  copd=emp
  fmhist=fam.lung.trend
  edu=edu6
  prshist=prior.cancer
  person = cbind(age, female, smkyears, qtyears, avecpd, race, copd, fmhist, bmi, edu, prshist, hypertension, chd, angina, heartattack, heartdisease, stroke, diab, bron, kidney, liver, speceq, year)
  
  #LC incidence and mortality risk
  lungcancer = lcrisk(person,nyears=1,nyears.mortality=1) #1-year lung cancer risk #,impute.missing=TRUE,nyears.mortality=1
  lcdrisk <- lungcancer[,3]/1000
  
  #other cause mortality and conditional survival
  alldth = lungcancer[,13]
  print(alldth)
  #AC
  # --- Replace zeros and NAs in all-cause mortality estimates (alldth) ---
  if (any(alldth == 0)) {
    first_zero <- min(which(alldth == 0))
    if (first_zero > 1) {
      alldth[first_zero:length(alldth)] <- alldth[first_zero - 1]
    }
  }
  
  if (any(is.na(alldth))) {
    last_na <- max(which(is.na(alldth)))
    if ((last_na + 1) <= length(alldth)) {
      alldth[1:last_na] <- alldth[last_na + 1]
    }
  }
  
  #other cause mortality risk
  ocdth = alldth-lcdrisk
  
  #other cause survival
  ocdsurv = cumprod(1-ocdth)
  
  #conditional survival
  ocdsurv = ocdsurv/ocdsurv[1]

  #lung cancer risk
  lc_prob = lungcancer[,5]/1000
  
  ########################################
  #other cause mortality - lcrisks package
  ########################################
  #other-cause mortality age
  rprob = runif(1, 0, 1)
  for(i in 1:length(ocdsurv)){
    if(rprob>ocdsurv[i]){
      dthAge_OC=startAge+i #0.5*i
      break
    }
  }

  if(dthAge_OC==0|dthAge_OC>100){dthAge_OC=100}
  #if dthAge_OC=0, die at 100
  end = min(startAge + simTime, dthAge_OC, 99) #end of the loop; last age was set to 99 because lc survival prob ends at 99
  
  
  #obtain lung cancer age
  lchaz1 = lc_prob[1:(length(lc_prob)-1)]
  lchaz2 = lc_prob[2:length(lc_prob)]
  lch = (lchaz1+lchaz2)/2
  lccumhaz = cumsum(lch)
  lcSurv = exp(-lccumhaz)
  lcSurv_cond = lcSurv/lcSurv[1]
  lcSurv_cond[is.na(lcSurv_cond)]=1
  rprob = runif(1, 0, 1)
  if((end-startAge)>=1){
    for(i in 1:(end-startAge)){
      if(rprob>lcSurv_cond[i]){
        LCAge=startAge+(i-0.5)
        break
      }
    }
  }
  
  
  if(LCAge>0){
    output = histology(sex, prshist, fmhist, copd, current[LCAge+0.5], former[LCAge+0.5], bmi, intensity[LCAge+0.5], smkdur[LCAge+0.5], yrsquit[LCAge+0.5])
    LCType = output[1]
    LCStage = output[2]
    PST = output[3:8]
    ###########################################
    #Lung cancer specific survival time
    ###########################################
    if(LCAge<=59){age_cat = 1}
    else if(LCAge<=69){age_cat = 2}
    else if(LCAge<=79){age_cat = 3}
    else{age_cat = 4}
    survTime = LCSpecMortality(sex, LCType, LCStage, age_cat)
    dthAge_LC = min(survTime+LCAge, 100)
    ###########################################
    #Update treatment burden
    ###########################################
    burden = treatment(LCStage, d)
  }
  
  if(dthAge_LC!=0){
    currAge=min(startAge+simTime, dthAge_OC, dthAge_LC, 100)
  }
  else{
    currAge=end
  }
  
  #manage death age
  if(dthAge_LC==0|dthAge_LC>=dthAge_OC){
    if(dthAge_OC<=min(startAge+simTime, 100)){dthAge=dthAge_OC}
    else{dthAge=0}
  }
  else{
    if(dthAge_LC<=min(startAge+simTime, 100)){dthAge=dthAge_LC}
    else{dthAge=0}
  }
  #end of life prob of LC
  lc_prob_end <- lc_prob[(round(dthAge,0)-startAge+1)]
  
  
  return(c(sex, LCAge, dthAge_LC, dthAge_OC, dthAge, LCType, LCStage, burden, lc_prob_end, list(PST[1:6]), list(pky),list(yrsquit)))
}

