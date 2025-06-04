
rm(list = ls())

library(splitstackshape)
library(mice)

#multiple imputation for IHIS 2010-2014 dataset

#first expand the dataset based on weight
#reading in

# sex = inp[1]
# currAge = inp[2]
# startAge = inp[2] #keep the age at entry
# edu = inp[3]
# bmi = inp[4]
# copd = inp[5]
# prshist = inp[6]
# fmhist = inp[7]
# race = inp[8]
# currsmk = inp[9]
# cpd = inp[10]
# currsmkdur = inp[11]
# curryearsquit = inp[12]
# pky = cpd*currsmkdur/20

data = read.table("input4080ever.csv",sep=",",header=T)#read.table("input_full.csv",sep=",",header=T)

data = data[!is.na(data$smkstat)&data$YEAR %in% c(2010:2018),]

input = as.data.frame(cbind(data$SAMPWEIGHT,data$YEAR,data$currentage,factor(data$SEX),data$race,data$education,data$BMI,data$copd,data$personalhistory,data$FMHIST,data$smkstat,data$currentsmokingstatus,data$averageCPD,data$currentsmokingduration,data$currentyearsquit,data[,24:34]))

names(input) = c("SAMPWEIGHT","YEAR","AGE","SEX","RACE","EDU","BMI","COPD","PERSONHIST","FAMHIST","SMKSTAT","CURRSMK","CPD","SMKDUR","YRSQUIT","HYPER","CORONARY","ANGINA","ATTACK","OTHERH","STROKE","DIABETE","CHRBRON","KIDNEY","LIVER","EQUIP")

summary(input)

# input$SAMPWEIGHT = round(input$SAMPWEIGHT/200)

# #only ever-smokers
# input = input[input$SMKSTAT!=1,]

# input2 = expandRows(input, "SAMPWEIGHT")
# input3 = input2[input2$YEAR%in%c(2015:2018),]

input2 = input

input2$CURRSMK = ifelse(input2$SMKSTAT==1,0,input2$CURRSMK)
input2$CPD = ifelse(input2$SMKSTAT==1,0,input2$CPD)
input2$SMKDUR = ifelse(input2$SMKSTAT==1,0,input2$SMKDUR)
input2$YRSQUIT = ifelse(input2$SMKSTAT==3,input2$YRSQUIT,0)

input2$RACE = factor(input2$RACE)
input2$EDU = factor(input2$EDU)
input2$PERSONHIST = factor(input2$PERSONHIST)
input2$FAMHIST = factor(input2$FAMHIST)
input2$COPD = factor(input2$COPD)
input2$HYPER = factor(input2$HYPER)
input2$CORONARY = factor(input2$CORONARY)
input2$ANGINA = factor(input2$ANGINA)
input2$ATTACK = factor(input2$ATTACK)
input2$OTHERH = factor(input2$OTHERH)
input2$STROKE = factor(input2$STROKE)
input2$DIABETE = factor(input2$DIABETE)
input2$CHRBRON = factor(input2$CHRBRON)
input2$KIDNEY = factor(input2$KIDNEY)
input2$LIVER = factor(input2$LIVER)
input2$EQUIP = factor(input2$EQUIP)

input2 = input2[(input2$SMKDUR>0|is.na(input2$SMKDUR)),]

#never = input2[input2$SMKSTAT==1,] #698857
current = input2[input2$SMKSTAT==2,] #212144
former = input2[input2$SMKSTAT==3,] #255456

# never = mice(never,method=c(rep("NA",3),"polyreg","polyreg","pmm","logreg","logreg","logreg",rep("NA",5)))
current = mice(current,method=c(rep("NA",4),"polyreg","polyreg","pmm","logreg","logreg","polyreg",rep("NA",2),"pmm","pmm","NA",rep("logreg",11)))
former = mice(former,method=c(rep("NA",4),"polyreg","polyreg","pmm","logreg","logreg","polyreg",rep("NA",2),rep("pmm",3),rep("logreg",11)))

# never = complete(never)
current = complete(current)
former = complete(former)

all = rbind(current,former)

all_1518 = all[all$YEAR%in%c(2015:2018),]

all_1518$SAMPWEIGHT = round(all_1518$SAMPWEIGHT/100)

input2 = expandRows(all_1518, "SAMPWEIGHT")

indx <- sapply(input2, is.factor)
input2[indx] <- lapply(input2[indx], function(x) as.numeric(as.character(x)))

summary(input2)

all_rearr = input2[,c(3,2,5,6,7,8,9,4,11,12,13,14,15:25)]

input3 = input[input$YEAR%in%c(2015:2018),]
summary(input3)


write.table(all_rearr,"impute_4080_1518_2mil.csv",sep=",",col.names=F,row.names=F)



data = read.table("input4080ever_2015.csv",sep=",",header=T)#read.table("input_full.csv",sep=",",header=T)


input = data

names(input) = c("SEX","AGE","EDU","BMI","COPD","PERSONHIST","FAMHIST","RACE","CURRSMK","CPD","SMKDUR","YRSQUIT","SMOKAGE","HYPER","CORONARY","ANGINA","ATTACK","OTHERH","STROKE","DIABETE","CHRBRON","KIDNEY","LIVER","EQUIP","SAMPWEIGHT")

input = input[,-c(13)]


input=input[complete.cases(input),]

summary(input)

input$SAMPWEIGHT = round(input$SAMPWEIGHT/10)

# #only ever-smokers
# input = input[input$SMKSTAT!=1,]

input2 = expandRows(input, "SAMPWEIGHT")

summary(input2)

write.table(input2,"impute_2015_2mil.csv",sep=",",col.names=F,row.names=F)
