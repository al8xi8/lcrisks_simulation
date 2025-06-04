#RNDgroup, 1="Spiral CT", 2="X-ray"
getscreen <- function(scryear, RNDgroup, d, s, currAge, overdiagnosis, sex, LCAge, LCType, LCStage, PST, dthAge_LC_NS, dthAge_OC_NS, dthAge_NS, burden_NS,id) { #global specificity, sensitivity
  #set.seed(id+1000000)
  
  quitted = 0
  scryear = scryear
  RNDgroup = RNDgroup
  burden=burden_NS
  s = s
  currAge=currAge
  LCAge = LCAge
  LCType = LCType
  LCStage = LCStage
  PST = PST[[1]]
  death=0
  diagdth=0
  dthAge=dthAge_NS
  dthAge_LC=dthAge_LC_NS
  survTime=0
  detected=0
  tp=0
  fp=matrix(NA,ncol=9,nrow=1)
  if(LCAge == 0){#no lung cancer
    prob_tn = runif(1,0,1)
    if(prob_tn>spec[RNDgroup, min(scryear,3)]){#Round 1, false positive
      fp = followup(min(scryear,3),0,RNDgroup,id)
      tp = 1
      death = fp[9]
      if(death==1){
        diagdth = 1
        dthAge=currAge
        dthAge_LC=currAge
        return(c(LCAge, LCStage, survTime, death, diagdth, dthAge, dthAge_LC, detected))
      }
    }
  }
  else{#had lung cancer in no screening arm
    #no preclinically diagnosable cancer
    if(currAge<LCAge-sum(PST)){
      prob_tn = runif(1,0,1)
      if(prob_tn>spec[RNDgroup, min(scryear,3)]){#Round 1, false positive
        fp = followup(min(scryear,3),0,RNDgroup,id) 
        tp = 1
        death = fp[9]
        if(death==1){
          diagdth = 1
          dthAge=currAge
          dthAge_LC=currAge
          return(c(LCAge, LCStage, survTime, death, diagdth, dthAge, dthAge_LC, detected))
        }
      }
    }
    else if(currAge<LCAge){ 
      #Preclinically diagnosable
      if((LCAge-currAge)>sum(PST[2:6])){stage = 1}
      else if((LCAge-currAge)>sum(PST[3:6])){stage = 2}
      else if((LCAge-currAge)>sum(PST[4:6])){stage = 3}
      else if((LCAge-currAge)>sum(PST[5:6])){stage = 4}
      else if((LCAge-currAge)>sum(PST[6:6])){stage = 5}
      else{stage = 6}
      if(RNDgroup==1){sens=sens_ct[min(scryear,3),]}
      else{sens=sens_cxr[min(scryear,3),]}
      prob_tp = runif(1,0,1)
      if(prob_tp < sens[6*(LCType-1)+stage]){ #Find it! #Round 1, true positive
        detected = 1
        fp = followup(min(scryear,3),1,RNDgroup,id)
        tp = 2
        death = fp[9] 
        if(death==1){
          diagdth = 1
          LCAge = currAge
          LCStage = stage
          dthAge=currAge
          dthAge_LC=currAge
          return(c(burden, LCAge, LCStage, survTime, death, diagdth, dthAge, dthAge_LC, detected))
        }
        else{
          LCStage = stage
          LCAge = currAge+0.5
          burden = treatment(LCStage, d, id)
        }
        #############################################
        #Reassign lung cancer specific survival time#
        #For only not overdiagnosed individuals     #
        #############################################
        if(LCAge<=59){age_cat = 1}
        else if(LCAge<=69){age_cat = 2}
        else if(LCAge<=79){age_cat = 3}
        else{age_cat = 4}
        if(overdiagnosis==0){
          survTime = LCSpecMortality(sex, LCType, LCStage, age_cat, id)
          dthAge_LC = min(survTime+LCAge, 100)
          dthAge_LC = max(dthAge_LC, dthAge_LC_NS) 
          #handle dthAge outside screening_core
        }
        ##############################################
        #Die of LC within this screening year        #
        #For only not overdiagnosed individuals      #
        #Overdiagnosed individuals have dthAge_LC = 0#
        ##############################################
#         if(dthAge_LC!=0&(dthAge_LC-currAge)<=1){
#           death = 1
#           dthAge = dthAge_LC
#         }
      }
    }
  }
  #let the person quit

  return(c(burden, LCAge, LCStage, survTime, death, diagdth, dthAge, dthAge_LC, detected,tp,fp[1:8]))
}
