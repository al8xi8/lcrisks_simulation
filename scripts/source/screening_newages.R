newages<-function(sex,pky,PST_old,currAge,LCAge_old,LCType_old,LCStage_old,dthAge_LC_old,dthAge_OC_old,id){ 
  #set.seed(id+4000000)
  bmi = 0
  copd = 0
  prshist = 0
  fmhist = 0
  dthAge_LC = 0 
  dthAge_OC = 0
  burden = 0
  LCAge_new = 0
  LCType_new = 0
  LCStage_new = 0
  PST_new = c(0,0,0,0,0,0)
  PST_old = PST_old[[1]]
  
  pky = pky[[1]]
  #PST = pst_old[[1]]

  startAge=currAge
  pky0 = c(0,pky[1:(length(pky)-1)])
  cpd = 20*(pky-pky0)
  smkstat = ifelse(pky==0,-1,1)#-1:never, 0: former, 1:current
  a = which(cpd>0)
  if(length(a)>0){
    if(max(a)<101){
      smkstat[(max(a)+1):101]=0
    }
  }
  cpd[(currAge+1):101]=0
  smkstat[(currAge+1):101]=0
  currsmk = ifelse(smkstat==1,1,0)
  fmrsmk = ifelse(smkstat==0,1,0)
  pky=cumsum(cpd)/20
  currsmkdur = cumsum(currsmk)
  curryearsquit = cumsum(fmrsmk)
  
  #obtain other causes death age
  ocMortRate = c()
  if(sum(pky)==0){#never smoker
    ocMortRate = hzocm(startAge:100,startAge,sex,0,-1)
  }
  else{
    for(i in startAge:100){
      ocMortRate = c(ocMortRate, hzocm(i,startAge,sex,pky[i+1],currsmk[i+1]))
    }
  }
  ocMortRate1 = ocMortRate[1:(length(ocMortRate)-1)]
  ocMortRate2 = ocMortRate[2:length(ocMortRate)]
  ocMortR=(ocMortRate1+ocMortRate2)/2
  ocdthcumhaz = cumsum(ocMortR) #c(0, (h0+h1)/2, (h0+h1)/2+(h1+h2)/2, ....
  ocSurv = exp(-ocdthcumhaz)
  ocSurv_cond = ocSurv/ocSurv[1]
  rprob = runif(1, 0, 1)
  for(i in 1:length(ocSurv)){
    if(rprob>ocSurv[i]){
      dthAge_OC=startAge+i-0.5 #0.5*i
      break
    }
  }
  if(dthAge_OC==0|dthAge_OC>100){dthAge_OC=100}
  dthAge_OC = max(dthAge_OC_old,dthAge_OC)
  end = min(startAge + simTime, dthAge_OC, 100)
  
  if((currAge<LCAge_old-sum(PST_old)-2)||LCAge_old==0){ #screening happens two years before lung cancer onset or no lung cancer before
    if(sex==1){model=model.HPFS} #LUNG CANCER INCIDENCE MODEL FOR MALES
    else if (sex==2){model=model.NHS}
    #profile
    profile = list(breaks=c(0:100),dose=c(cpd))
    #obtain lung cancer age
    if(LCAge_old==0){
      if(dthAge_OC_old==dthAge_OC){
        LCAge_new=0
        startAge=end
      }
      else{
        startAge=dthAge_OC_old
      }
    }
    else{
      startAge=currAge
    }
    
    if((end-startAge)>0.5){
      lchaz = tscehaz(model,profile,startAge:100)#set end to 200
      lchaz1 = lchaz[1:(length(lchaz)-1)]
      lchaz2 = lchaz[2:length(lchaz)]
      lch = (lchaz1+lchaz2)/2
      lccumhaz = cumsum(lch)
      lcSurv = exp(-lccumhaz)
      lcSurv_cond = lcSurv/lcSurv[1]
      rprob = runif(1, 0, 1)
      for(i in 1:length(lcSurv_cond)){
        if(rprob>lcSurv_cond[i]){
          LCAge_new=startAge+(i-0.5)
          break
        }
      }
      if(LCAge_new>end){LCAge_new=0}
    }
    if(LCAge_old>0&LCAge_new>0){LCAge_new=max(LCAge_old,LCAge_new)}
    
    if(LCAge_new==0){
      LCAge_new=0
      LCType_new=0
      LCStage_new=0
      PST_new=0
    }
    else{
      output = histology_newages(sex, prshist, fmhist, copd, currsmk[LCAge_new-0.5], fmrsmk[LCAge_new-0.5], bmi, cpd[LCAge_new-0.5], currsmkdur[LCAge_new-0.5], curryearsquit[LCAge_new-0.5],LCAge_old,LCStage_old,id)
      LCType_new = output[1]
      LCStage_new = output[2]
      PST_new = output[3:8]
      ###########################################
      #Lung cancer specific survival time
      ###########################################
      if(LCAge_new<=59){age_cat = 1}
      else if(LCAge_new<=69){age_cat = 2}
      else if(LCAge_new<=79){age_cat = 3}
      else{age_cat = 4}
      survTime = LCSpecMortality(sex, LCType_new, LCStage_new, age_cat,id)
      dthAge_LC = max(min(survTime+LCAge_new, 100),dthAge_LC_old) #not earlier than old dthAge_LC
      ###########################################
      #Update treatment burden
      ###########################################
      burden = treatment(LCStage_new, 1,id)
    }
  }
  else{ #screening starts after lung cancer onset or lung cancer onset is within 2-years of screening age
    LCAge_new = LCAge_old
    LCType_new = LCType_old
    LCStage_new = LCStage_old
    dthAge_LC = dthAge_LC_old
    PST_new = PST_old
  }
  dthAge = ifelse(dthAge_LC==0, dthAge_OC, min(dthAge_LC, dthAge_OC))
  return(c(LCAge_new, dthAge_LC, dthAge_OC, dthAge, LCType_new, LCStage_new,list(PST_new),list(pky),list(curryearsquit),burden))
}
