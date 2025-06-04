################################################################
#No invasive procedure complications
#include only follow-up burden
#no burden on death from invasive procedure
#LC specific mortality model should have accounted for that
################################################################

complications <- function(procedure, true, RNDgroup, id){ 
  #set.seed(id+7000000)
  #procedures can be either [1] thoracotomy/-scopy or mediastinoscopy; [2] bronchoscopy; [3] needle biopsy; [4] non-invasive procedures
  death = 0
  i = procedure[[1]]
  cmp_prob <- runif(1, 0, 1)
  pprob <- cmpprob[[RNDgroup]]
  dthprob <- cmpdthprob[[RNDgroup]]
  if(length(i)==1){
    s=1-prod(1-sum(pprob[i,]))
  }
  else{
    s=1-prod(1-rowSums(pprob[i,]))
  }
  
  if(cmp_prob<s){#have at least one complica1tions
    temp=ifelse(i<4,4,ifelse(i<5,3,ifelse(i<6,2,1)))
    dth_f = runif(1,0,1)
    if(dth_f<1-prod(1-sum(dthprob[true, temp]))){death=1}
  }
  return(death)
}
followup <- function(round, true, RNDgroup,id){
  #set.seed(id+6000000)
  death = 0
  R=round
  procedure_prob=procedureprob[[RNDgroup]]
  fup_proc = matrix(NA,ncol=8,nrow=1)
  #Assuming a person could receive more than 1 follow up tests
  #CXR, Chest CT, FDG PET or FDG PET/CT, Percutaneous cytology or biopsy, Bronchoscopy, Mediastinoscopy/tomy, thoracoscopy, thoracoscopy, 
  #First 8 entries: confirmed lung cancer (TRUE POSITIVE), second 8: not confirmed (FALSE POSITIVE)
  prob <- runif(8, 0, 1)
  if (true == 1){ #true positive
    proc_rec <- which(prob<procedure_prob[R,c(1:8)])
  }
  else{ #false positive
    proc_rec <- which(prob<procedure_prob[R,c(9:16)])
  }
  if(length(proc_rec)>0){
    fup_proc[proc_rec]=1
    death = complications(list(proc_rec), true, RNDgroup, id)
  }
  return(c(fup_proc,death)) #did not receive any follow-test
}
