LCSpecMortality = function(sex,type,stage,age,id){
  #set.seed(id+2000000)
  #Age 1: 50-59, 2: 60-69, 3: 70-79, 4: 80+
  #Stage 1a, 1b, 2, 3a, 3b, 4
  #Type 1: Small cell, 2: Adenocarcinoma, 3: Squamous, 4: Other non-small cell
  #Gender 1: Male, 2: Female
  row = 96*(sex-1)+24*(type-1)+4*(stage-1)+age
  C = 1/(1+exp(-parameter[row,1]))#1/(1+0.8*exp(-parameter[row,1]))
  Mu = parameter[row,2]
  Sig = exp(parameter[row,3])#/sqrt(2)
  #print(parameter[row,1])
  surv_prob = runif(1,0,1)
  if(surv_prob<C){surv_length = 100}
  else{
    p = (surv_prob-C)/(1-C)
    surv_length = qlnorm(p, Mu, Sig)/12
  }
  return(surv_length)
}

# test = function(sex,type,stage,age){
#   i=0
#   v_bin=c()
#   v_len=c()
#   row = 64*(sex-1)+16*(type-1)+4*(stage-1)+age
#   parameter = read.csv(file='lung cancer specific mortality.csv', head=TRUE,sep=",")
#   C = 1/(1+exp(-parameter[row,1]))
#   Mu = parameter[row,2]
#   Sig = exp(parameter[row,3])#/sqrt(2)
#   while(i<1000){
#     surv_length = LCSpecMortality(sex,type,stage,age)
#     v_len=c(v_len,surv_length)
#     if(surv_length==1200){v_bin=c(v_bin,0)}
#     else{v_bin=c(v_bin,1)}
#     i=i+1
#   }
#   mysurv = Surv(time=v_len,event=v_bin)
#   myfit = survfit(mysurv~1)
#   plot(myfit, xlim=c(0,5))
#   lines(0:100,C+(1-C)*(1-plnorm(seq(0,1200,12), Mu, Sig)),col='red')
# }
