#######################################################################
#                         List of Global Variables                    #
#######################################################################

#######################################################################
#                        LC stage distribution                        #
#######################################################################
#stage 1A 1B 2 3A 3B (4)
probLCStageMaleSM <- c(0.025, 0.029, 0.021, 0.092, 0.166)
probLCStageMaleAD <- c(0.143, 0.105, 0.045, 0.077, 0.151) 
probLCStageMaleSQ <- c(0.119, 0.145, 0.08, 0.133, 0.193) 
probLCStageMaleOTH <- c(0.071, 0.072, 0.036, 0.087, 0.165)
probLCStageFemaleSM <- c(0.037, 0.029, 0.024, 0.111, 0.195)
probLCStageFemaleAD <- c(0.198, 0.112, 0.046, 0.08, 0.141) 
probLCStageFemaleSQ <- c(0.17, 0.149, 0.075, 0.12, 0.178) 
probLCStageFemaleOTH <- c(0.092, 0.071, 0.036, 0.092, 0.161)

#######################################################################
#                        Follow-up probabilities                      #
#######################################################################
###### Procedure Probabilities
###### LDCT

T0_prob = c(0.414814815,0.67037037,0.633333333,0.362962963,0.585185185,0.177777778,0.162962963,0.577777778,0.19216265,0.815215609,0.091326447,0.009345794,0.024266273,0.001967536,0.00623053,0.006722414)
T1_prob = c(0.303571429,0.660714286,0.666666667,0.279761905,0.613095238,0.154761905,0.238095238,0.714285714,0.085514303,0.294430919,0.036214242,0.004108338,0.011412051,0.000912964,0.002434571,0.004260499)
T2_prob = c(0.379146919,0.696682464,0.739336493,0.327014218,0.535545024,0.104265403,0.3507109,0.654028436,0.246646473,0.632193855,0.102553007,0.010385115,0.03202077,0.001298139,0.009519688,0.011250541)
procedure_prob_ct = matrix(c(T0_prob, T1_prob, T2_prob), 3, 16, 1)

###### Chest X-ray

T0_prob = c(0.463235294,0.963235294,0.661764706,0.485294118,0.558823529,0.161764706,0.102941176,0.573529412,0.394117647,0.693627451,0.043627451,0.008333333,0.015196078,0,0.003921569,0.008823529)
T1_prob = c(0.430769231,0.907692308,0.784615385,0.415384615,0.569230769,0.153846154,0.092307692,0.6,0.253774263,0.493170381,0.038820992,0.007189073,0.013659238,0.001437815,0.003594536,0.003594536)
T2_prob = c(0.307692308,0.923076923,0.807692308,0.487179487,0.564102564,0.205128205,0.179487179,0.487179487,0.387940842,0.728100114,0.056882821,0.01592719,0.020477816,0.005688282,0.006825939,0.006825939)
procedure_prob_cxr = matrix(c(T0_prob, T1_prob, T2_prob), 3, 16, 1)

procedureprob = list(procedure_prob_ct, procedure_prob_cxr)

###### Complication Probabilities
#major[1], intermediate[2], minor [3] 

###### LDCT

p1TP = c(0.133237822,0.161891117,0.025787966)
p2TP = c(0.024590164,0.057377049,0.008196721)
p3TP = c(0,0.129032258,0)
p4TP = c(0.065217391,0.043478261,0.02173913)
p1FP = c(0.014326648,0.02722063,0.005730659)
p2FP = c(0.007326007,0.032967033,0)
p3FP = c(0,0.077777778,0.011111111)
p4FP = c(0.000189081,0.000850863,0.00014181)
prob_ct = matrix(c(p1TP, p2TP, p3TP, p4TP, p1FP, p2FP, p3FP, p4FP), ncol=3, nrow=8, byrow=TRUE)

###### Chest X-ray

p1TP = c(0.133237822,0.161891117,0.025787966)
p2TP = c(0.024590164,0.057377049,0.008196721)
p3TP = c(0,0.129032258,0)
p4TP = c(0.065217391,0.043478261,0.02173913)
p1FP = c(0.014326648,0.02722063,0.005730659)
p2FP = c(0.007326007,0.032967033,0)
p3FP = c(0,0.077777778,0.011111111)
p4FP = c(0.000189081,0.000850863,0.00014181)
prob_cxr = matrix(c(p1TP, p2TP, p3TP, p4TP, p1FP, p2FP, p3FP, p4FP), ncol=3, nrow=8, byrow=TRUE)

cmpprob = list(prob_ct, prob_cxr)

###### Death from complication probability

###### LDCT

dth_TP = c(0.012893983, 0.073770492, 0.032258065, 0.02173913)
dth_FP = c(0.00286533, 0.014652015, 0, 0.000378161)
dthprob_ct = matrix(c(dth_TP, dth_FP), ncol = 4, nrow = 2, byrow = TRUE)

###### Chest X-ray

dth_TP = c(0.012893983, 0.073770492, 0.032258065, 0.02173913)
dth_FP = c(0.00286533, 0.014652015, 0, 0.000378161)
dthprob_cxr = matrix(c(dth_TP, dth_FP), ncol = 4, nrow = 2, byrow = TRUE)

cmpdthprob = list(dthprob_ct, dthprob_cxr)

#######################################################################
#                        Treatment Probabilities                      #
#######################################################################

#trt_s1_r0 = c(22, 39, 628, 12, 93, 13)
#2011 NLST supp table 3 - combined ct and cxr
#1) no treatment; 2) chemo only, radiation only, other; 3) surgery only; 4) chemo and radiation; 5) surgery and chemo/radiation; 6) surgery, chemo and radiation
#1A
trt_s1a_r0 = c(16, 31, 508, 5, 41, 9)
trt_s1a_r0 = trt_s1a_r0/sum(trt_s1a_r0)
#1B
trt_s1b_r0 = c(6, 8, 120, 7, 52, 4)
trt_s1b_r0 = trt_s1b_r0/sum(trt_s1b_r0)
#2
trt_s2_r0 = c(3, 5, 36, 25, 61, 15)
trt_s2_r0 = trt_s2_r0/sum(trt_s2_r0)
#3A
trt_s3a_r0 = c(8, 24, 13, 99, 32, 31)
trt_s3a_r0 = trt_s3a_r0/sum(trt_s3a_r0)
#3B
trt_s3b_r0 = c(23, 49, 21, 96, 35, 18)
trt_s3b_r0 = trt_s3b_r0/sum(trt_s3b_r0)
#4
trt_s4_r0 = c(78, 285, 19, 136, 23, 10)
trt_s4_r0 = trt_s4_r0/sum(trt_s4_r0) 
#rows: stage
#columns: treatment
trt_r0 = matrix(c(trt_s1a_r0, trt_s1b_r0, trt_s2_r0, trt_s3a_r0, trt_s1b_r0, trt_s4_r0), 6, 6, 1)

#######################################################################
#              Mean preclinical sojourn time                          #
#######################################################################
b = 1.4411
MPSTMaleSM = c(1.25, 0.44, 0.32, 0.32, 0.25, 0.51) #Mean preclinical sojourn time by histology and stage
MPSTMaleAD <- c(1.82, 0.64, 0.46, 0.46, 0.36, 0.74) 
MPSTMaleSQ <- c(2.16, 0.76, 0.55, 0.55, 0.42, 0.88)
MPSTMaleOTH <- c(1.96, 0.69, 0.5, 0.5, 0.39, 0.8)
MPSTFemaleSM = c(1.36, 0.48, 0.34, 0.35, 0.27, 0.55) #Mean preclinical sojourn time by histology and stage
MPSTFemaleAD <- c(2.44, 0.86, 0.62, 0.62, 0.48, 0.99) 
MPSTFemaleSQ <- c(2.15, 0.76, 0.55, 0.55, 0.42, 0.88)
MPSTFemaleOTH <- c(2.31, 0.81, 0.59, 0.59, 0.45, 0.94)
MPSTMale = matrix(c(MPSTMaleSM, MPSTMaleAD, MPSTMaleSQ, MPSTMaleOTH), 4, 6, 1)
MPSTFemale = matrix(c(MPSTFemaleSM, MPSTFemaleAD, MPSTFemaleSQ, MPSTFemaleOTH), 4, 6, 1)

#######################################################################
#                      Sensitivity and Specificity                    #
#######################################################################

###### LDCT     

#T0, T1 screening sensitivity
#by stage
#1a 1b 2 3a 3b 4
sens_sm_1_ct = c(0.0883, 0.1028, 0.1119, 0.4158, 0.8706, 0.9935)
sens_ad_1_ct = c(0.5663, 0.6412, 0.6448, 0.7593, 0.8021, 0.9888)
sens_sq_1_ct = c(0.3095, 0.3805, 0.3919, 0.6967, 0.7939, 0.9766)
sens_oth_1_ct = c(0.2078, 0.2475, 0.2478, 0.604, 0.6827, 0.9567) #SM AD SQ OTH
sens_r1_ct = matrix(c(sens_sm_1_ct, sens_ad_1_ct, sens_sq_1_ct, sens_oth_1_ct), 1, 24)
#T2 screening sensitivity
sens_sm_3_ct = c(0.1493, 0.1720, 0.1859, 0.5635, 0.9242, 0.9964)
sens_ad_3_ct = c(0.7031, 0.7642, 0.767, 0.8512, 0.8802, 0.9938)
sens_sq_3_ct = c(0.4484, 0.5269, 0.5389, 0.8064, 0.8748, 0.9869)
sens_oth_3_ct = c(0.3223, 0.3736, 0.3739, 0.7344, 0.796, 0.9757)
sens_r3_ct = matrix(c(sens_sm_3_ct, sens_ad_3_ct, sens_sq_3_ct, sens_oth_3_ct), 1, 24)

#LungRads
sens_ct = rbind(84.9/93.5*sens_r1_ct, 78.6/93.8*sens_r1_ct, 78.6/93.8*sens_r3_ct) #Should be 93.8 instead of 93.6???
# sens_ct = rbind(sens_r1_ct, sens_r1_ct, sens_r3_ct) #0.65-20.5%; 0.64-19.7%

###### Chest X-ray  

#T0, T1 screening sensitivity
sens_sm_1_cxr = c(0.0251, 0.0425, 0.0664, 0.1471, 0.5318, 0.9731)
sens_ad_1_cxr = c(0.1691, 0.2713, 0.2726, 0.4811, 0.4929, 0.9631)
sens_sq_1_cxr = c(0.0972, 0.289, 0.3002, 0.4631, 0.4796, 0.7862)
sens_oth_1_cxr = c(0.0627, 0.0757, 0.0757, 0.2978, 0.344, 0.3694) #SM AD SQ OTH
sens_r1_cxr = matrix(c(sens_sm_1_cxr, sens_ad_1_cxr, sens_sq_1_cxr, sens_oth_1_cxr), 1,24)
#T2 screening sensitivity
sens_sm_3_cxr = c(0.0252, 0.0427, 0.0666, 0.1478, 0.5328, 0.9732)
sens_ad_3_cxr = c(0.1696, 0.2721, 0.2734, 0.4820, 0.4939, 0.9632)
sens_sq_3_cxr = c(0.0975, 0.2898, 0.301, 0.464, 0.4806, 0.7868)
sens_oth_3_cxr = c(0.0629, 0.076, 0.076, 0.2986, 0.3449, 0.3703)
sens_r3_cxr = matrix(c(sens_sm_3_cxr, sens_ad_3_cxr, sens_sq_3_cxr, sens_oth_3_cxr), 1, 24)

sens_cxr = rbind(sens_r1_cxr, sens_r1_cxr, sens_r3_cxr)

#specificity - baseline, favor, against screening (s)
#T0, T1 screening specificity
#NLST - baseline, favor screening and anti screening
# spec_ct = c(0.734, 0.834, 0.634)
# spec_cxr = c(0.735, 0.835, 0.635)

#####LungRads - Round 1, 2, 3
#####need to change screening_core.R parameters from spec[RNDgroup, s]  to spec[RNDgroup, min(scryear,3)] (2 occurences)
#####need to change sensitivity above too
spec_ct = c(0.872, 0.947, 0.947)
spec_cxr = c(0.735, 0.835, 0.635)
#1="Spiral CT" 2="X-ray"
spec = matrix(c(spec_ct, spec_cxr), nrow=2, ncol=3, byrow=TRUE)

#######################################################################
#     Burdens-treatment, screening, followup & complications          #
#######################################################################
#disutility (d)
trtburden = matrix(c(0, 0.14, 0.18, 0.28, 0.32, 0.46, 0, 0.19, 0.05, 0.38, 0.24, 0.44, 0, 0.03, 0.23, 0.06, 0.26, 0.29), nrow = 3, ncol = 6, byrow=T)
fupburden = c(0.03, 0.01, 0.05)
scrburden = c(0.001, 0.0001, 0.01) #base screening/follow-up/procedure burden
cmpburden = matrix(c(0.01, 0.03, 0.18, 0.005, 0.01, 0.05, 0.02, 0.06, 0.23), nrow = 3, ncol = 3, byrow=T) 
#overdiagnosis (o)
over = c(0.1, 0.6, 1.5) 
#######################################################################
#                           Utilities                                 #
#######################################################################
#baseline quality male & female
quality = c(1,1) #1 as baseline (0.76,0.74)
#archive
#quality_y1 = c(0.43, 0.43, 0.4, 0.39, 0.95, 0.94)
#quality_y2 = c(0.45, 0.45, 0.42, 0.41, 0.97, 0.96)


#favor screening
#modify only IA
# quality_y1 = c(0.99,0.71,0.68,0.67,0.67,0.66)
# quality_y2 = c(1,0.73,0.7,0.69,0.69,0.68)
#modify only IV
# quality_y1 = c(0.71,0.71,0.68,0.67,0.67,0.38)
# quality_y2 = c(0.73,0.73,0.7,0.69,0.69,0.40)
#anti screening
#modify only IA
# quality_y1 = c(0.43,0.71,0.68,0.67,0.67,0.66)
# quality_y2 = c(0.45,0.73,0.7,0.69,0.69,0.68)
#modify only IV
# quality_y1 = c(0.71,0.71,0.68,0.67,0.67,0.94)
# quality_y2 = c(0.73,0.73,0.7,0.69,0.69,0.96)
#baseline
quality_y1 = c(0.71,0.71,0.68,0.67,0.67,0.66)
quality_y2 = c(0.73,0.73,0.7,0.69,0.69,0.68)
#utility yearly discount rate
discount = exp(-0.03)#1/1.03
#0; 0.06
#######################################################################
#                           Input Data                                #
#######################################################################
setwd("/Users/al8xi8/Documents/GitHub/lcrisks_simulation")

###### Histology model
#parameter for lung cancer specific mortality model
parameter = read.csv(file="scripts/source/lung_cancer_specific_mortality.csv", head=TRUE,sep=",")
#coefficients for lung cancer histology model
#hist_mod includes family history
hist.mod = as.matrix(read.csv(file="scripts/source/hist_mod_sm.csv",header=FALSE,sep=","))
coef <- matrix(0, ncol=8, nrow=3) #6 covariates, 7 including 2 levels of smking status, 1 intercept, 1 hist
coef[,1]=c(2,3,4)
for(i in 0:6){
  coef[,i+2]=c(as.numeric(hist.mod[3*i+2,5]), as.numeric(hist.mod[3*i+3,5]), as.numeric(hist.mod[3*i+4,5]))
}

init_f=1.13
init_m=1.26
# init_f=1
# init_m=1

NHSp = c(3.0000E+00, 7.7098E-02, 1.2603E-07, 1.6973E-01, 5.2948E-01, 9.7439E-02, 6.1668E-01, 5.0000E+00, 1.2603E-07*init_f)
HPFSp = c(3.0000E+00, 7.7098E-02, 1.2603E-07, 2.5756E-01, 3.9298E-01, 3.3504E-01, 2.4500E-01, 5.0000E+00, 1.2603E-07*init_m)

#######################################################################
#                     Simulation Setup                                #
#######################################################################
#total simulation duration
simTime = 100

#######################################################################
#                    Never Smoker incidence                           #
#######################################################################
never.fem=c(6.68392256,10.84604721,17.0898676,25.38985144,34.87551474,45.16846654,56.80377437,56.80377437)/100000
never.m=c(8.782813035,14.25193125,22.4564409,33.36279201,45.82715056,59.35230295,74.64133906,74.64133906)/100000

never.fem=stepfun(c(54,59,64,69,74,79,80),never.fem,right=TRUE)
never.m=stepfun(c(54,59,64,69,74,79,80),never.m,right=TRUE)

#######################################################################
#                     Adjusting factor                                #
#######################################################################
#inflation factor by age group
#female: 0.98,0.97,0.9,0.75,0.77,0.72

fac.m = c(1.17, 1.24, 1.53, 1.41, 1.19, 1.12)
fac.m=stepfun(c(54,59,64,69,74,79),c(fac.m,1.12),right=TRUE)

fac.fem = c(0.98,0.97,0.9,0.75,0.77,0.72)
fac.fem=stepfun(c(54,59,64,69,74,79),c(fac.fem,0.72),right=TRUE)

# fac.m = c(1.17*1.31, 1.24*1.31, 1.53*1.29, 1.41*1.25, 1.19*1.21, 1.12*1.13)
# fac.m=stepfun(c(54,59,64,69,74,79),c(fac.m,1.12*1.13),right=TRUE)
# 
# fac.fem = c(1.31, 1.28, 1.17, 0.97, 0.94, 0.86)
# fac.fem=stepfun(c(54,59,64,69,74,79),c(fac.fem,0.86),right=TRUE)
#setting up screening eligibility
# scrstage = 55
# scredage = 80
# scrpky = 30
# scrqt = 15
# if(cms=='TF'){#USPSTF
#   scrstage = 55
#   scredage = 80
#   scrpky = 30
#   scrqt = 15
#   #number of screenings
#   numScrn = 9*(scredage-scrstage+1) #one column indicating screening or not; 8 other columns for follow-up procedure
# }
# else if(cms=='CMS'){#CMS
#   scrstage = 55
#   scredage = 77
#   scrpky = 30
#   scrqt = 15
#   #number of screenings
#   numScrn = 9*(scredage-scrstage+1) #one column indicating screening or not; 8 other columns for follow-up procedure
# }
# else{
#   print("error")
# }







