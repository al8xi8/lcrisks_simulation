
histology_newages <- function(sex, prshist, fmhist, copd, currsmk, fmrsmk, bmi, cpd, currsmkdur_atdiag, curryearsquit_atdiag, LCAge_old, LCStage_old,id){
  #set.seed(id+5000000)
  LCType = 0 #1: SM, 2: AD, 3: SQ, 4: OTH
  LCStage = 0
  PST = c(0,0,0,0,0,0)
  p_rand1 <- runif(1)
  #input = c(1, sex, prshist, copd, currsmk, fmrsmk, bmi, cpd, currsmkdur_atdiag, curryearsquit_atdiag)
  pky_atdiag = cpd*currsmkdur_atdiag/20
  input = c(1, sex, currsmk, fmrsmk, currsmkdur_atdiag, curryearsquit_atdiag, pky_atdiag)
  #probability of getting small cell lung cancer given with lung cancer
  #m <- exp(coef[,2:11]%*%input)
  m <- exp(coef[,2:8]%*%input)
  p1 = (1+m[1]+m[2]+m[3])^(-1)
  p2 = m[1]*p1
  p3 = m[2]*p1
  p4 = m[3]*p1
  if(p_rand1<p1) {#SM
    LCType = 1
    if(sex==1){LCStage = stage(probLCStageMaleSM)}
    else{LCStage = stage(probLCStageFemaleSM)}
  }
  else if(p_rand1<(p1+p2)) {#AD
    LCType = 2
    if(sex==1){LCStage = stage(probLCStageMaleAD)}
    else{LCStage = stage(probLCStageFemaleAD)}
  }
  else if(p_rand1<(p1+p2+p3)) {
    LCType = 3
    if(sex==1){LCStage = stage(probLCStageMaleSQ)}
    else{LCStage = stage(probLCStageFemaleSQ)}
  }
  else {
    LCType = 4
    if(sex==1){LCStage = stage(probLCStageMaleOTH)}
    else{LCStage = stage(probLCStageFemaleOTH)}
  }
  if(LCAge_old>0){LCStage=min(LCStage,LCStage_old)}
  #Getting mean sojourn time
  if(sex==1){
    a = MPSTMale[LCType, LCStage]/gamma(1+1/b)
    j = LCStage
    PST[j] = rweibull(1, b, a)/2
    j = j-1
    while(j>0){
      #print(MPSTMale[LCType, j])
      a = MPSTMale[LCType, j]/gamma(1+1/b)
      PST[j] = rweibull(1, b, a)
      j = j-1
    }
  }
  else{
    a = MPSTFemale[LCType, LCStage]/gamma(1+1/b)
    j = LCStage
    PST[j] = rweibull(1, b, a)/2
    j = j-1
    while(j>0){
      #print(MPSTFemale[LCType, j])
      a = MPSTFemale[LCType, j]/gamma(1+1/b)
      PST[j] = rweibull(1, b, a)
      j = j-1
    }
  }
  return(c(LCType, LCStage, PST))
}
