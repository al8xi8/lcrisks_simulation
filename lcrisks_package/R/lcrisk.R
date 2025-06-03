# INFO:
# "Life-expectancy without CT lung screens" and 
# "Life-expectancy with 3 rounds of CT lung screens" are  
# meant to be residual life-expectancy (remaining life-years).


risk.kovalchik <- function (begin, end, newdata, coxph1, ...) 
{
  c.coxph.risk <- function (coxph, ...) 
  {
    models <- list(coxph)
    if (!missing(..1)) 
      models <- c(models, list(...))
    models
  }  

  coxph.relrisk.uncentered <- function (coxph.object, newdata) 
  {
    center <- coxph.object$means %*% coef(coxph.object)
    if (!missing(newdata)) 
      lp <- predict(coxph.object, newdata, type = "lp")
    else lp <- coxph.object$linear.predictor
    exp(lp + rep(center,length(lp)))
  }  
  
  projection.relrisk <- function (object, data) 
  {
    if (is.numeric(object)) 
      return(object)
    else if (class(object) == "coxph") 
      if (missing(data)) 
        return(coxph.relrisk.uncentered(object))
    else return(coxph.relrisk.uncentered(object, data))
    else stop(cat("No method for class", class(object)))
  }  
    
    #calculate absolute risk given hazards/survival and relative risks
    risk.fixed.interval <- function(H, RR) {

        if (!is.list(H)) 
            0
        else {
            absrisk <- H[[1]]$surv^RR[1] * H[[1]]$haz * RR[1]
            for (i in 2:length(H)) {
                absrisk <- absrisk * (H[[i]]$surv^RR[i])
            }
            sum(absrisk)
        }
    }
    
    models <- c.coxph.risk(coxph1, ...)
    
    #check for missing
    which.kept <- complete.cases(newdata)
    if (!all(which.kept)) {
        warning("Missing cases excluded.")
        if (length(begin) > 1) {
            begin <- begin[which.kept]
            end <- end[which.kept]
        }
        newdata <- newdata[which.kept, , drop=FALSE]
        if (!nrow(newdata)) stop("ALL rows removed")
    }

    #calculate relative risk for each subject and store as list
    rr <- sapply(models, projection.relrisk, data = newdata)

    if (is.matrix(rr)) { rr.list <- lapply(1:nrow(rr), function(x) rr[x, ])
    } else { rr.list <- list(rr)}

    #estimate risk
    if (length(begin) == 1) {    
       AllVars <- unique(unlist(sapply(models, function(x) all.vars(x$formula))))
        in.interval <- function(x, begin, end) x >= begin & x <= end
        if ("lung.cancer.death" %in% AllVars){
          H <- list(models[[1]]$basehaz[in.interval(models[[1]]$basehaz$time, begin, end),], 
          models[[2]]$basehaz_LCDRAT[in.interval(models[[1]]$basehaz$time, begin, end),])
        } else if ("case" %in% AllVars){
          H <- list(models[[1]]$basehaz[in.interval(models[[1]]$basehaz$time, begin, end),], 
          models[[2]]$basehaz_LCRAT[in.interval(models[[1]]$basehaz$time, begin, end),])        
        }

        #calculate absolute risk given hazards/survival for average covariate values and relative risks
        risks <- mapply(risk.fixed.interval, RR = rr.list, MoreArgs = list(H = H))
    } else {
        stop("No code for length(begin) != 1")
        #risks <- mapply(risk, begin = begin, end = end, RR = rr.list, MoreArgs = list(models = models))
    }

    list(risks=risks, rows=which.kept)
}

addValues <- function(covar, var, begin, end, covarCols, newdat, coxph1) {

  covar[, var] <- NA
  tmp          <- try(risk.kovalchik(begin, end, covar[, covarCols, drop=FALSE], newdat, coxph1), silent=TRUE)
  if (!("try-error" %in% class(tmp))) {
    covar[tmp$rows, var] <- tmp$risks
  }
  covar
}

getRowsFromRnames <- function(vec) {
  ret <- gsub("r", "", vec, fixed=TRUE)
  ret <- as.numeric(ret)
  ret
}

#' Wrapper to the cut() function
#'
#' A wrapper to the cut() function, so that you can automatically break into quantiles as
#' the default behavior, otherwise if the breakpoints are included, then just break on those.
#' In all cases, include.lowest is set to True      
#' @export
cuts <- function(data,npieces,simple.labels=TRUE,...) {

  if (length(npieces)==0 | any(is.na(npieces)) | any(!is.numeric(npieces)))
    stop("npieces must be a numeric scalar or a vector of breakpoints")
  
  if (length(npieces)==1)
    # Just break into quantiles.  Use quantile labelling: Q1,Q2,Q3,etc. if you want
    if (simple.labels)
      cutdata <- cut(data,breaks=quantile(data,seq(0,1,1/npieces),na.rm=T,...),
                     include.lowest=T,labels=paste("Q",1:npieces,sep=""),...)
    else
      cutdata <- cut(data,breaks=quantile(data,seq(0,1,1/npieces),na.rm=T,...),
                     include.lowest=T,...)
  else
    # Break into pieces as specified by the breakpoints
    if (simple.labels)
      cutdata <- cut(data,breaks=npieces,include.lowest=T,
                     labels=paste("Q",1:(length(npieces)-1),sep=""),...)
    else
      cutdata <- cut(data,breaks=npieces,include.lowest=T,...)
  
  return(cutdata)
}



#' Lung Cancer Death Risk Predictor
#'
#' In both the absence and presence of screening, the R package calculates individual risks 
#' of lung cancer and lung cancer death based on covariates: age, education, sex, race, 
#' smoking intensity/duration/quit-years, Body Mass Index, family history of lung-cancer, 
#' and self-reported emphysema.  In the presence of CT screening akin to the NLST 
#' (3 yearly screens, 5 years of follow-up), it uses the covariates to estimate risk of 
#' false-positive CT screen as well as the reduction in risk of lung cancer death and 
#' increase in risk of lung cancer screening.  
#' This package also estimates the Life Years Gained From Screening-CT (LYFS-CT) 
#' as per Cheung et al., 2019. A basic mortality model can be fitted using only age, gender,
#' race, and smoking variables. To use the full mortality model, comorbidities and the year 
#' of patient assessment should be included.
#'
#' @section Warning:
#'  VGAM and survival are required dependencies of this package.
#'  VGAM and survival may automatically be installed the first time this package is used. 
#'
#' @section Model Objects in Package:
#'  \itemize{
#'  \item LCDRAT - model for lung cancer death in absence of screening;
#'  \item LCRAT - model for lung cancer incidence in absence of screening;
#'  \item cox.death - model for deaths from causes other than lung cancer;
#'  \item morat - model for overall mortality;
#'  \item polytmod - polytomous model for false positive CT lung screens.
#'  }
#' 
#' @param x A numeric matrix containing individuals' covariates for the model.  
#'  Columns 11-23 are needed only to estimate life gained from undergoing CT screening
#'  using the full mortality model.
#'  Covariates should be in the following column and format:
#'
#'  \itemize{
#'  \item column 1 - current age (numeric);
#'  \item column 2 - gender (1=Female, 0=Male);
#'  \item column 3 - years smoked (numeric);
#'  \item column 4 - years quit (numeric or NA);
#'  \item column 5 - cigarettes per day (numeric);
#'  \item column 6 - race (0=Non-hispanic white,
#'                   1=Non-hispanic Black/African American, 
#'                   2=Hispanic, 
#'                   3=Other Ethnicity);
#'  \item column 7 - lung disease (1=COPD or Emphysema, 0=No COPD or Emphysema);
#'  \item column 8 - number of parents with lung cancer (0,1,2);
#'  \item column 9 - bmi;
#'  \item column 10 - highest education level (1=<12 grade, 
#'                                       2=HS graduate, 
#'                                       3=post hs, no college, 
#'                                       4=associate degree/some college, 
#'                                       5=bachelors degree, 
#'                                       6=graduate school);
#'  \item column 11 - prior history of cancer (1=Yes,0=No);
#'  \item column 12 - Hypertension (1=Yes,0=No);
#'  \item column 13 - Coronary Heart Disease (1=Yes,0=No);
#'  \item column 14 - Angina pectoris (1=Yes,0=No);
#'  \item column 15 - Heart Attack (1=Yes,0=No);
#'  \item column 16 - Other heart disease (1=Yes,0=No);
#'  \item column 17 - Stroke (1=Yes,0=No);
#'  \item column 18 - Diabetes (1=Yes,0=No);
#'  \item column 19 - Chronic bronchitis in past year (1=Yes,0=No);
#'  \item column 20 - Weak/failing kidneys in past year (1=Yes,0=No);
#'  \item column 21 - Liver condition in past year (1=Yes,0=No);
#'  \item column 22 - Health problem requiring special eqiupment (1=Yes,0=No);
#'  \item column 23 - Year of assessment.                                       
#'  }
#' @param y Number of years to calculate risks for (numeric, max of 10).
#' @param impute.missing Option to impute missing variables using the 
#'        NHIS 2015 survey (default=TRUE). 
#' @param counterfactual.race 0-3 to compute countefactual estimates,
#'            where (0=Non-hispanic white,
#'                   1=Non-hispanic Black/African American, 
#'                   2=Hispanic, 
#'                   3=Other Ethnicity).
#'           The default is 0.
#'
#' @return A numeric matrix containing individuals' predictions:
#'
#'  \itemize{
#'  \item column 1 - An indicator variable for whether the individual is eligible 
#'              for CT lung screening according to 
#'              US Preventive Services Task Force (USPSTF) recommendations.
#'  \item column 2 - Number of years predictions are for.
#'  \item column 3 - Among 1000 people in the US with this risk-factor profile, 
#'              this is the number who will die from lung cancer 
#'              if they do not attend screening.
#'  \item column 4 - In the NLST, those who underwent 3 rounds of annual CT 
#'              screening had their risk reduced by 20 percent.  
#'              Therefore, among those who would have died from lung cancer,
#'              this is the number who will not die from lung cancer death, 
#;              if they undergo 3 yearly CT lung screens as in the NLST.
#'  \item column 5 - Among 1000 people in the US with this risk-factor profile, 
#'              this is the number who will be diagnosed with lung cancer 
#'              if they do not attend screening (LCRAT).
#'  \item column 6 - In the NLST, those who underwent CT screening had 12.4 percent 
#'              more lung cancer diagnosed, all of which require treatment.  
#'              Therefore, among 1000 people with this risk-factor profile, 
#'              this is the number of extra lung cancer that would be 
#'              diagnosed, if they undergo 3 yearly CT lung screens as in the NLST.
#'  \item column 7 - Out of 1000 NLST participants with this risk profile, 
#'              this is the number who had at least one false-positive 
#'              CT screen out of 3 screens.
#'  \item column 8 - Days of life expectancy gained from undergoing 3 rounds of CT screening.
#'  \item column 9 - Life expectancy without CT screening.
#'  \item column 10 - Life expectancy with CT screening.
#'  \item column 11 - Years of life gained if lung cancer is found early due to screening.
#'  \item column 12 - Years of life gained if lung cancer death is averted due to screening
#' 
#'  }
#'
#' @author Li C. Cheung, \email{li.cheung@nih.gov}, Stephanie A. Kovalchik, Hormuzd A. Katki
#'
#' @references 
#' \itemize{
#'  \item Katki HA, Kovalchik SA, Berg CD, Cheung LC, Chaturvedi AK.
#'             Development and validation of risk models to select ever-smokers
#'             for CT lung cancer screening. JAMA. 2016; 315(21):2300-2311.
#'             doi: 10.1001/jama.2016.6255.
#'  \item Cheung LC, Berg CD, Castle PE, Katki HA, Chaturvedi AK.
#'        Life-gained-based versus risk-based selection of smokers for lung cancer screening.
#'        Ann Intern Med.;171:623-632. doi:107326/M19-1263.  
#'
#' }
#' 
#' @export
#' @examples
#' age <- c(66,58,75,72,56)
#' bmi <- c(23,28,26,27,24)
#' cpd <- c(36,36,40,24,40)
#' emp <- c(0,1,1,0,1)
#' fam.lung.trend <- c(0,2,0,2,0)
#' female <- c(0,1,0,1,0)
#' smkyears <- c(43,37,45,42,29)
#' qtyears <- c(NA,NA,9,6,6)
#' race <- c(0,1,2,2,3)
#' edu6 <- c(3,5,4,5,5)
#' prior.cancer <- c(0,0,0,0,0)
#' hypertension <- c(0,0,1,0,1)
#' chd <- c(0,0,0,0,0)
#' angina <- c(0,0,0,0,0)
#' heartattack <- c(0,0,0,0,1)
#' heartdisease <- c(0,0,0,0,0)
#' stroke <- c(0,0,0,0,0)
#' diab <- c(1,0,0,0,0)
#' bron <- c(0,1,0,0,1)
#' kidney <- c(0,0,0,0,0)
#' liver <- c(0,0,0,0,0)
#' speceq <- c(0,1,0,0,0)
#' yearassessed <- rep(2019,5)
#' years <- 5
#' 
#' persons <- cbind(age,
#'                  female,
#'                  smkyears,
#'                  qtyears,
#'                  cpd,
#'                  race,
#'                  emp,
#'                  fam.lung.trend,
#'                  bmi,
#'                  edu6,
#'                  prior.cancer,
#'                  hypertension,
#'                  chd,
#'                  angina,
#'                  heartattack,
#'                  heartdisease,
#'                  stroke,
#'                  diab,
#'                  bron,
#'                  kidney,
#'                  liver,
#'                  speceq,
#'                  yearassessed)
#'                      
#' persons_predictions <- lcrisk(persons,years)
#' persons_predictions

lcrisk <- function(x, nyears, impute.missing=TRUE, counterfactual.race=0,
                   nyears.mortality=5) {
  
   ##########################################################################################################
   # History
   # 2021-08-13 Make changes to line: survx <- (exp(-1*haz[which(haz$time>age[x]),1])/exp(-1*haz[which(haz$time==age[x]),1]))^rs[x]
   # Email: "We should change that code to use the time just preceding age[x] if 
   #  the hazard at age[x] is not defined.  Was age 41 the only integer age affected from ages 40-84 
   #  (for 40, we should remove the denominator altogether)? Best, Li
   # 2021-09-03 Make changes to age[x] based on email:
   #   We encountered some errors with the LYG calculations.  
   #   Currently, z subsets to non-missing rows, and we sapply the lyg function to z.  
   #   However, within the lyg function, we index age[x], but the age vector contains both non-missing and missing rows.  
   #   Therefore non-missing rows after the missing rows are reading the incorrect age[x].
   # 2021-10-13 Add option for counterfactual race. Email:
   #   For lcrisks and lcmodels, we want to create a counterfactual set of estimates for the life expectancy and LYG calculations.
   #   These counterfactual estimates will use:
   #   1) the original lung cancer death risk predictions with the individual’s original race
   #   2) comorbidity imputations under the individual’s counterfactual race (set default to white)
   #   3) the mortality model predictions under the individual’s counterfactual race (set default to white)
   # 2023-06-29 Add option nyears.mortality to compute overall mortality in nyears
   #   Email: Thinking about it more for age 100, what do you think instead of calculating mortality 
   #          through age 100, but no further (i.e. mortality does not suddenly jump to 1).  
   #          Then in the manual for the all-cause mortality nyears option, it returns risk through 
   #          nyears or age 100, whichever is achieved earlier.  
   ##########################################################################################################

   checkNumeric(nyears) 
   checkLogical(impute.missing)  
   checkCounterfactual(counterfactual.race) 
   x <- checkInputData(x)
   checkNumeric(nyears.mortality, maxval=98) 

   # First call using no counterfactual race
   obj0 <- lcrisk.main(x, nyears, impute.missing=impute.missing, counterfactual.race=NULL,
                       nyears.mortality=nyears.mortality)

   # Second call using counterfactual race
   obj1 <- lcrisk.main(x, nyears, impute.missing=impute.missing, counterfactual.race=counterfactual.race,
                       nyears.mortality=nyears.mortality)

   ret  <- mergeObj.counterfactual(obj0, obj1) 
   ret
}
  
lcrisk.main <- function(x, nyears, impute.missing=TRUE, counterfactual.race=NULL,
                        nyears.mortality=5) {

   flag1          <- ncol(x) >= 23
   counterFlag    <- !is.null(counterfactual.race)

   age            <- x[,1]
   female         <- x[,2]
   smkyears       <- x[,3]
   qtyears        <- x[,4]
   qtyears        <- ifelse(is.na(qtyears),0,qtyears)
   cpd            <- x[,5]
   race           <- as.factor(x[,6])
   emp            <- x[,7]
   fam.lung.trend <- x[,8]
   bmi            <- x[,9]
   edu6           <- x[,10]
   pkyr.cat       <- smkyears*cpd/20

   race.orig      <- race
   if (counterFlag) {
     race.counter <- as.factor(rep(counterfactual.race, length(race)))
   } else {
     race.counter <- race 
   }
   
   # Imputation uses the counterfactual race or the original race (if counterfactual.race = NULL)
   race <- race.counter
   if (impute.missing==TRUE){
     bmi[is.na(bmi)==1] <- 29.994-0.1115*age[is.na(bmi)==1]+1.7528*I(race[is.na(bmi)==1]==1)+
       0.9577*I(race[is.na(bmi)==1]==2)-1.6921*I(race[is.na(bmi)==1]==3)+0.1091*female[is.na(bmi)==1]+
       0.0531*cpd[is.na(bmi)==1]-0.1608*log(pkyr.cat[is.na(bmi)==1])+0.9115*log(smkyears[is.na(bmi)==1])+
       0.9508*log(qtyears[is.na(bmi)==1]+1)
     bmi <- checkImputed(bmi, "bmi", warn=1)
     edu6[is.na(edu6)==1] <- 3.6862+0.1192*log(age[is.na(edu6)==1])-0.4907*I(race[is.na(edu6)==1]==1)-
       0.9958*I(race[is.na(edu6)==1]==2)-0.0708*I(race[is.na(edu6)==1]==3)+0.0823*female[is.na(edu6)==1]-
       0.2484*I(cpd[is.na(edu6)==1]>=20)-0.0434*log(pkyr.cat[is.na(edu6)==1])-0.0207*smkyears[is.na(edu6)==1]+
       0.0964*log(qtyears[is.na(edu6)==1]+1)
     edu6 <- checkImputed(edu6, "education", warn=1)
     lp <- -5.2038+1.1218*log(age[is.na(fam.lung.trend)==1])-1.022*I(race[is.na(fam.lung.trend)==1]==1)-
           1.4558*I(race[is.na(fam.lung.trend)==1]==2)-1.3052*I(race[is.na(fam.lung.trend)==1]==3)+
           0.0917*female[is.na(fam.lung.trend)==1]-0.3626*log(cpd[is.na(fam.lung.trend)==1])+
           0.4853*log(pkyr.cat[is.na(fam.lung.trend)==1])-0.3273*sqrt(smkyears[is.na(fam.lung.trend)==1])-
           0.0184*qtyears[is.na(fam.lung.trend)==1]
     lp <- checkImputed(lp, "number of parents with lung cancer", warn=1)
     fam.lung.trend[is.na(fam.lung.trend)==1] <- exp(lp)/(1+exp(lp))
     lp <- -9.2854+0.1946*sqrt(age[is.na(emp)==1])-0.4025*I(race[is.na(emp)==1]==1)-1.139*I(race[is.na(emp)==1]==2)-
           1.0095*I(race[is.na(emp)==1]==3)-0.0142*female[is.na(emp)==1]+0.1818*I(cpd[is.na(emp)==1] >= 20)+
           0.0125*pkyr.cat[is.na(emp)==1]+0.7645*sqrt(smkyears[is.na(emp)==1])+0.0198*sqrt(qtyears[is.na(emp)==1])
     lp <- checkImputed(lp, "lung disease", warn=1)
     emp[is.na(emp)==1] <- exp(lp)/(1+exp(lp))
   }
 
   if (flag1) {   
     prior.cancer <- ifelse(is.na(x[,11])==0 & x[,11] %in% c(0,1),x[,11],NA)
     hypertension <- ifelse(is.na(x[,12])==0 & x[,12] %in% c(0,1),x[,12],NA)
     chd <- ifelse(is.na(x[,13])==0 & x[,13] %in% c(0,1),x[,13],NA)
     angina <- ifelse(is.na(x[,14])==0 & x[,14] %in% c(0,1),x[,14],NA)
     heartattack <- ifelse(is.na(x[,15])==0 & x[,15] %in% c(0,1),x[,15],NA)
     heartdisease <- ifelse(is.na(x[,16])==0 & x[,16] %in% c(0,1),x[,16],NA)
     stroke <- ifelse(is.na(x[,17])==0 & x[,17] %in% c(0,1),x[,17],NA)
     diab <- ifelse(is.na(x[,18])==0 & x[,18] %in% c(0,1),x[,18],NA)
     bron <- ifelse(is.na(x[,19])==0 & x[,19] %in% c(0,1),x[,19],NA)
     kidney <- ifelse(is.na(x[,20])==0 & x[,20] %in% c(0,1),x[,20],NA)
     liver <- ifelse(is.na(x[,21])==0 & x[,21] %in% c(0,1),x[,21],NA)
     speceq <- ifelse(is.na(x[,22])==0 & x[,22] %in% c(0,1),x[,22],NA)
     year <- ifelse(is.na(x[,23])==0 & x[,23]>=0,x[,23],NA)

     if (impute.missing==TRUE){
       lp <- -10.3915+1.6949*log(age[is.na(speceq)==1])+0.6288*I(race[is.na(speceq)==1]==1)+
             0.1398*I(race[is.na(speceq)==1]==2)+0.0565*I(race[is.na(speceq)==1]==3)+0.1878*female[is.na(speceq)==1]-
             0.1024*sqrt(cpd[is.na(speceq)==1])+0.015*pkyr.cat[is.na(speceq)==1]+0.2012*sqrt(smkyears[is.na(speceq)==1])+
             0.0178*qtyears[is.na(speceq)==1]
       lp <- checkImputed(lp, "health problem requiring special equipment", warn=1)
       speceq[is.na(speceq)==1] <- exp(lp)/(1+exp(lp))
       lp <- -3.0702-0.029*age[is.na(liver)==1]-0.3303*I(race[is.na(liver)==1]==1)+0.0984*I(race[is.na(liver)==1]==2)-
             0.606*I(race[is.na(liver)==1]==3)-0.0058*female[is.na(liver)==1]+0.1473*sqrt(cpd[is.na(liver)==1])-
             0.2633*log(pkyr.cat[is.na(liver)==1])+0.0434*smkyears[is.na(liver)==1]+0.115*sqrt(qtyears[is.na(liver)==1])
       lp <- checkImputed(lp, "liver condition in past year", warn=1)
       liver[is.na(liver)==1] <- exp(lp)/(1+exp(lp))
       lp <- -10.5384+1.8572*log(age[is.na(diab)==1])+0.7984*I(race[is.na(diab)==1]==1)+0.7837*I(race[is.na(diab)==1]==2)+
             0.5*I(race[is.na(diab)==1]==3)-0.2197*female[is.na(diab)==1]+0.1815*I(cpd[is.na(diab)==1] >= 20)+
             0.007*pkyr.cat[is.na(diab)==1]+0.2594*log(smkyears[is.na(diab)==1])+0.0076*qtyears[is.na(diab)==1]
       lp <- checkImputed(lp, "diabetes", warn=1)
       diab[is.na(diab)==1] <- exp(lp)/(1+exp(lp))
       lp <- -8.3008+0.0349*age[is.na(kidney)==1]+0.343*I(race[is.na(kidney)==1]==1)+0.1427*I(race[is.na(kidney)==1]==2)+
             0.3635*I(race[is.na(kidney)==1]==3)-0.2092*female[is.na(kidney)==1]+0.0281*cpd[is.na(kidney)==1]-
             0.2223*log(pkyr.cat[is.na(kidney)==1])+0.8292*log(smkyears[is.na(kidney)==1])+0.0176*qtyears[is.na(kidney)==1]
       lp <- checkImputed(lp, "weak/failing kidneys in past year", warn=1)
       kidney[is.na(kidney)==1] <- exp(lp)/(1+exp(lp))
       lp <- -17.818+3.6777*log(age[is.na(prior.cancer)==1])-0.5575*I(race[is.na(prior.cancer)==1]==1)-
             0.8102*I(race[is.na(prior.cancer)==1]==2)-1.164*I(race[is.na(prior.cancer)==1]==3)+0.2055*female[is.na(prior.cancer)==1]+
             0.016*cpd[is.na(prior.cancer)==1]-0.0053*pkyr.cat[is.na(prior.cancer)==1]+0.1098*sqrt(smkyears[is.na(prior.cancer)==1])+
             0.0877*sqrt(qtyears[is.na(prior.cancer)==1])
       lp <- checkImputed(lp, "prior history of cancer", warn=1)
       prior.cancer[is.na(prior.cancer)==1] <- exp(lp)/(1+exp(lp))
       lp <- -7.0694+0.0596*age[is.na(stroke)==1]+0.3312*I(race[is.na(stroke)==1]==1)+0.2439*I(race[is.na(stroke)==1]==2)-
             0.3382*I(race[is.na(stroke)==1]==3)-0.2466*female[is.na(stroke)==1]-0.3095*I(cpd[is.na(stroke)==1] >= 20)+
             0.0166*pkyr.cat[is.na(stroke)==1]+0.0828*log(smkyears[is.na(stroke)==1])-0.0759*log(qtyears[is.na(stroke)==1]+1)
       lp <- checkImputed(lp, "stroke", warn=1)
       stroke[is.na(stroke)==1] <- exp(lp)/(1+exp(lp))
       lp <- -14.5767+2.7591*log(age[is.na(chd)==1])+0.1135*I(race[is.na(chd)==1]==1)+0.1116*I(race[is.na(chd)==1]==2)-
             0.0271*I(race[is.na(chd)==1]==3)-0.6922*female[is.na(chd)==1]-0.1556*sqrt(cpd[is.na(chd)==1])+
             0.249*sqrt(pkyr.cat[is.na(chd)==1])+0.0112*smkyears[is.na(chd)==1]+0.0202*qtyears[is.na(chd)==1]
       lp <- checkImputed(lp, "coronary heart disease", warn=1)
       chd[is.na(chd)==1] <- exp(lp)/(1+exp(lp))
       lp <- -17.9734+3.5297*log(age[is.na(heartattack)==1])+0.0232*I(race[is.na(heartattack)==1]==1)-
             0.1583*I(race[is.na(heartattack)==1]==2)+0.1133*I(race[is.na(heartattack)==1]==3)-0.6572*female[is.na(heartattack)==1]-
             0.0056*cpd[is.na(heartattack)==1]+0.1762*sqrt(pkyr.cat[is.na(heartattack)==1])+0.1129*log(smkyears[is.na(heartattack)==1])-
             0.0003*qtyears[is.na(heartattack)==1]
       lp <- checkImputed(lp, "heart attack", warn=1)
       heartattack[is.na(heartattack)==1] <- exp(lp)/(1+exp(lp))
       lp <- -8.2199+1.1237*log(age[is.na(heartdisease)==1])-0.1497*I(race[is.na(heartdisease)==1]==1)-
             0.592*I(race[is.na(heartdisease)==1]==2)+0.081*I(race[is.na(heartdisease)==1]==3)-0.0199*female[is.na(heartdisease)==1]+
             0.221*sqrt(cpd[is.na(heartdisease)==1])-0.232*log(pkyr.cat[is.na(heartdisease)==1])+0.0362*smkyears[is.na(heartdisease)==1]+
             0.1547*sqrt(qtyears[is.na(heartdisease)==1])
       lp <- checkImputed(lp, "other heart disease", warn=1)
       heartdisease[is.na(heartdisease)==1] <- exp(lp)/(1+exp(lp))
       lp <- -3.4777-0.0145*age[is.na(bron)==1]+0.2342*I(race[is.na(bron)==1]==1)-0.4939*I(race[is.na(bron)==1]==2)-
             0.8404*I(race[is.na(bron)==1]==3)+0.8134*female[is.na(bron)==1]-0.085*sqrt(cpd[is.na(bron)==1])+
             0.0199*pkyr.cat[is.na(bron)==1]+0.0315*smkyears[is.na(bron)==1]+0.0097*qtyears[is.na(bron)==1]
       lp <- checkImputed(lp, "chronic bronchitis in past year", warn=1)
       bron[is.na(bron)==1] <- exp(lp)/(1+exp(lp))
       lp <- -12.5734+2.8198*log(age[is.na(hypertension)==1])+0.9723*I(race[is.na(hypertension)==1]==1)+
            0.0862*I(race[is.na(hypertension)==1]==2)-0.0721*I(race[is.na(hypertension)==1]==3)-0.2521*female[is.na(hypertension)==1]+
            0.1108*log(cpd[is.na(hypertension)==1])-0.0019*pkyr.cat[is.na(hypertension)==1]+0.2135*log(smkyears[is.na(hypertension)==1])+
            0.0512*sqrt(qtyears[is.na(hypertension)==1])
       lp <- checkImputed(lp, "hypertension", warn=1)
       hypertension[is.na(hypertension)==1] <- exp(lp)/(1+exp(lp))
       lp <- -10.1981+1.5728*log(age[is.na(angina)==1])-0.0783*I(race[is.na(angina)==1]==1)-0.1044*I(race[is.na(angina)==1]==2)-
             0.8567*I(race[is.na(angina)==1]==3)-0.3057*female[is.na(angina)==1]-0.3474*log(cpd[is.na(angina)==1])+
             0.3091*sqrt(pkyr.cat[is.na(angina)==1])-0.0012*smkyears[is.na(angina)==1]+0.0172*qtyears[is.na(angina)==1]
       lp <- checkImputed(lp, "angina pectoris", warn=1)
       angina[is.na(angina)==1] <- exp(lp)/(1+exp(lp))
     }
       covar <- data.frame(age=age,
                         bmi=bmi,
                         cpd=cpd,
                         emp=emp,
                         fam.lung.trend=fam.lung.trend,
                         female=female,
                         qtyears=qtyears,
                         smkyears=smkyears,
                         race=race,
                         edu6=edu6,
                         pkyr.cat=pkyr.cat,
                         prior.cancer=prior.cancer,
                         hypertension = hypertension,
                         chd = chd,
                         angina = angina,
                         heartattack = heartattack,
                         heartdisease = heartdisease,
                         stroke = stroke,
                         diab = diab,
                         bron = bron,
                         kidney = kidney,
                         liver = liver,
                         speceq = speceq,
                         year = year)
   } else {
     covar <- data.frame(age=age,
                         bmi=bmi,
                         cpd=cpd,
                         emp=emp,
                         fam.lung.trend=fam.lung.trend,
                         female=female,
                         qtyears=qtyears,
                         smkyears=smkyears,
                         race=race,
                         edu6=edu6,
                         pkyr.cat=pkyr.cat)
   }

   # Incidence calculations use the original race
   covar$race <- race.orig

   tmp <- c("LCDRAT", "cxLCDRAT", "LCRAT", "cxLCRAT", 
            "prob_0falsepos", "prob_1falsepos", "prob_2falsepos", "prob_3falsepos",
            "expected_falsepos", paste0("LCDRAT", 1:5))
   for (v in tmp) covar[, v] <- NA
   rownames(covar) <- paste0("r", 1:nrow(covar))

   # Changed below 2021-07-16, from email: 
   # 2)	The USPSTF recently changed their criteria to age 50-80, 20+ pack-years, and no more than 15 years quit
   # uspstf_elig <- ifelse(age>=50 & age<=80 & pkyr.cat>=30 & qtyears <= 15,1,0)
   uspstf_elig <- ifelse(age>=50 & age<=80 & pkyr.cat>=20 & qtyears <= 15,1,0)

   #calculate predicted risks of lung cancer death within nyears years based on LCDRAT with competing risk of death
   covar <- addValues(covar, "LCDRAT", 0, nyears, 1:11, LCDRAT, cox.death)
   #covar$LCDRAT <- risk.kovalchik(0, nyears, covar[,1:11], LCDRAT, cox.death)

   #calculate predicted risks of lung cancer death using chest xray
   covar$cxLCDRAT <- 0.796*covar$LCDRAT
 
   #calculate predicted risks of lung cancer incidence within nyears years based on LCRAT with competing risk of death
   covar <- addValues(covar, "LCRAT", 0, nyears, 1:11, LCRAT, cox.death)
   #covar$LCRAT <- risk.kovalchik(0, nyears, covar[,1:11], LCRAT, cox.death)
   covar$LCRAT <- pmax(covar$LCRAT,covar$LCDRAT)
   
   #calculate predicted risks of lung cancer incidence using chest xray
   covar$cxLCRAT <- 1.124*covar$LCRAT
   
   #calculate probability of Z, number of false positives, taking values of 0, 1, 2, or 3 
   #responses are reported as log(P(Z=1)/P(Z=0)), log(P(Z=2)/P(Z=0)), and log(P(Z=3)/P(Z=0))
   prob_numfalsepos <- try(predict(polytmod,type="response",newdata=covar), silent=TRUE)
   if (!("try-error" %in% class(prob_numfalsepos))) {
     tmp <- getRowsFromRnames(rownames(prob_numfalsepos))

     #solving for P(Z=0), P(Z=1), P(Z=2), and P(Z=3), we have:
     for (i in 0:3) covar[tmp, paste0("prob_", i, "falsepos")] <- prob_numfalsepos[, i+1]
     covar$expected_falsepos <- covar$prob_1falsepos + 2*covar$prob_2falsepos + 3*covar$prob_3falsepos
   }

   for (i in 1:5) covar <- addValues(covar, paste0("LCDRAT", i), 0, i, 1:11, LCDRAT, cox.death)   

   # Life expectancy and time gained use the counterfactual race
   covar$race <- race.counter

   # Flag for warning message if age+nyears.mortality is too large
   flag.mortality   <- FALSE
   maxAge.mortality <- 98

   if (flag1) {   

     covar$missval <- ifelse(!is.na(covar$age) & !is.na(covar$female) & !is.na(covar$smkyears) & !is.na(covar$qtyears) &
                             !is.na(covar$cpd) & !is.na(covar$race) & !is.na(covar$emp) & 
                             !is.na(covar$fam.lung.trend) & !is.na(covar$bmi) & !is.na(covar$edu6) &
                             !is.na(covar$year) & !is.na(covar$hypertension) & !is.na(covar$chd) &
                             !is.na(covar$angina) & !is.na(covar$stroke) & !is.na(covar$heartattack) &
                             !is.na(covar$heartdisease) & !is.na(covar$prior.cancer) & !is.na(covar$speceq) &
                             !is.na(covar$diab) & !is.na(covar$liver) & !is.na(covar$bron) & !is.na(covar$kidney),0,1)
   
     lgindex         <- which(covar$missval==0)
     covar$ely       <- NA
     covar$lg        <- NA
     covar$ely_ct    <- NA
     covar$mortality <- NA
     rs <- predict(morat, covar, type = "risk",reference="sample")
     if (sum(1-covar$missval)>0){

       z              <- covar[lgindex,]
       flag.mortality <- any(z[, 1, drop=TRUE] + nyears.mortality > maxAge.mortality)  

       lyg <- function(x){
         # age vector needs to change based on subsetted z
         #agex <- age[x]
         agex <- z[x, 1]  
 
         if ((agex < 40) || (agex >= 99)) {
           msg <- paste0("Age = ", agex, " is outside the range for estimation, results may not be accurate.")
           warning(msg)
         }

         numer <- exp(-1*haz[which(haz$time>agex),1])
         ival  <- getMinLogVecLE(haz$time, agex)
         if (is.finite(ival)) {
           denom <- exp(-1*haz[ival,1])
         } else {
           denom <- NA
         }         
         if (agex > 40) {
           survx <- (numer/denom)^rs[x]
         } else {
           survx <- numer
         }
         ages <- haz[which(haz$time>agex),2]
         ely <- sum(diff(c(z$age[x],ages))*survx)     

         agexp5 <- agex + 5
         numer  <- exp(-1*haz[which(haz$time>agexp5),1])
         ival   <- getMinLogVecLE(haz$time, agexp5)
         if (is.finite(ival)) {
           denom <- exp(-1*haz[ival,1])
         } else {
           denom <- NA
         }  
         condsurvx <- (numer/denom)^rs[x]
         #condsurvx <- (exp(-1*haz[which(haz$time>age[x]+5),1])/exp(-1*haz[which(haz$time==age[x]+5),1]))^rs[x]
         condages <- haz[which(haz$time>agexp5),2]           
         condely <- sum(diff(c(z$age[x]+5,condages))*condsurvx)

         lyg <- .204*z$LCDRAT1[x] + .204*z$LCDRAT2[x] + 
                .204*z$LCDRAT3[x] + .204*z$LCDRAT4[x] +
                .204*z$LCDRAT5[x]*(1+condely)

         #######################################################
         # For overall mortality in m years
         # Update agexp5 , numer, denom 
         # mortality will be new condsurvx
         ival   <- getMinLogVecLE(haz$time, agex)
         if (is.finite(ival)) {
           denom <- exp(-1*haz[ival,1]) # this is baseline survival at their current age (timeline in mortality model begins at age 40)
         } else {
           denom <- NA
         } 
         agexp.mor <- agex + nyears.mortality
         if (agexp.mor > maxAge.mortality) agexp.mor <- maxAge.mortality
         numer.mor     <- exp(-1*haz[which(haz$time>=agexp.mor),1])
         condsurvx.mor <- 1 - (numer.mor[1]/denom)^rs[x]
         if (is.finite(condsurvx.mor) && (condsurvx.mor < 0)) condsurvx.mor <- 0
         ####################################################### 

         res <- c(ely,ely+lyg,365.25*lyg, condsurvx.mor)

         return(res)
       } 
       lyg_res                  <- sapply(1:nrow(z),lyg)
       lyg_res                  <- t(lyg_res)
       covar$ely[lgindex]       <- lyg_res[,1]
       covar$ely_ct[lgindex]    <- lyg_res[,2]
       covar$lg[lgindex]        <- lyg_res[,3]
       covar$mortality[lgindex] <- lyg_res[,4]
     } 
   } else {
     covar$missval <- ifelse(!is.na(covar$age) & !is.na(covar$female) & !is.na(covar$smkyears) & !is.na(covar$qtyears) &
                               !is.na(covar$cpd) & !is.na(covar$race) & !is.na(pkyr.cat),0,1)
     lgindex         <- which(covar$missval==0)
     covar$ely       <- NA
     covar$lg        <- NA
     covar$ely_ct    <- NA
     covar$mortality <- NA
     rs <- predict(morat2, covar, type = "risk",reference="sample")
     if (sum(1-covar$missval)>0){
       z              <- covar[lgindex,]
       flag.mortality <- any(z[, 1, drop=TRUE] + nyears.mortality > maxAge.mortality) 

       lyg <- function(x){
         #print(x)
         #agex <- age[x]
         agex  <- z[x, 1] 
         if ((agex < 40) || (agex >= 99)) {
           msg <- paste0("Age = ", agex, " is outside the range for estimation, results may not be accurate")
           warning(msg)
         }
         numer <- exp(-1*haz2[which(haz2$time>agex),1]) 
         ival  <- getMinLogVecLE(haz2$time, agex)
         if (is.finite(ival)) {
           denom <- exp(-1*haz2[ival,1])
         } else {
           denom <- NA
         }         
         if (agex > 40) {
           survx <- (numer/denom)^rs[x]
         } else {
           survx <- numer
         }
         #survx <- (exp(-1*haz2[which(haz2$time>age[x]),1])/exp(-1*haz2[which(haz2$time==age[x]),1]))^rs[x]

         ages <- haz2[which(haz2$time>agex),2]
         ely <- sum(diff(c(z$age[x],ages))*survx)     
 
         agexp5 <- agex + 5
         numer  <- exp(-1*haz2[which(haz2$time>agexp5),1])
         ival   <- getMinLogVecLE(haz2$time, agexp5)
         if (is.finite(ival)) {
           denom <- exp(-1*haz2[ival,1])
         } else {
           denom <- NA
         }        
         condsurvx <- (numer/denom)^rs[x]
         #condsurvx <- (exp(-1*haz2[which(haz2$time>age[x]+5),1])/exp(-1*haz2[which(haz2$time==age[x]+5),1]))^rs[x]

         #######################################################
         # For overall mortality in m years
         # Update agexp5 and numer, denom 
         # mortality will be new condsurvx
         ival   <- getMinLogVecLE(haz2$time, agex)
         if (is.finite(ival)) {
           denom <- exp(-1*haz2[ival,1]) # this is baseline survival at their current age (timeline in mortality model begins at age 40)
         } else {
           denom <- NA
         }  
         agexp.mor <- agex + nyears.mortality
         if (agexp.mor > maxAge.mortality) agexp.mor <- maxAge.mortality
         numer.mor     <- exp(-1*haz2[which(haz2$time>=agexp.mor),1])
         condsurvx.mor <- 1 - (numer.mor[1]/denom)^rs[x]
         if (is.finite(condsurvx.mor) && (condsurvx.mor < 0)) condsurvx.mor <- 0
         ####################################################### 

         condages <- haz2[which(haz2$time>agexp5),2]           
         condely <- sum(diff(c(z$age[x]+5,condages))*condsurvx)
         lyg <- .204*z$LCDRAT1[x] + .204*z$LCDRAT2[x] + 
                .204*z$LCDRAT3[x] + .204*z$LCDRAT4[x] +
                .204*z$LCDRAT5[x]*(1+condely)
         res <- c(ely,ely+lyg,365.25*lyg, condsurvx.mor)
         return(res)
       } 

       lyg_res                  <- sapply(1:nrow(z),lyg)
       lyg_res                  <- t(lyg_res)
       covar$ely[lgindex]       <- lyg_res[,1]
       covar$ely_ct[lgindex]    <- lyg_res[,2]
       covar$lg[lgindex]        <- lyg_res[,3]
       covar$mortality[lgindex] <- lyg_res[,4]
     }      
   }   

   if (flag.mortality && !length(counterfactual.race)) {
     msg <- paste0("The all-cause mortality risk was computed using a maximum age of ",
                    maxAge.mortality)
     warning(msg)
   }

   predicted <- data.frame(uspstf_elig,
                           rep(nyears,length(uspstf_elig)),
                           1000*covar$LCDRAT,
                           1000*(covar$LCDRAT - covar$cxLCDRAT),
                           1000*covar$LCRAT,
                           1000*(covar$cxLCRAT - covar$LCRAT),
                           1000*(1-covar$prob_0falsepos),
                           covar$lg,
                           covar$ely,
                           covar$ely_ct,
                           (covar$lg/365.25)/covar$cxLCRAT,
                           (covar$lg/365.25)/(covar$LCDRAT - covar$cxLCDRAT),
                            covar$mortality)
                           
   colnames(predicted) <- c("USPSTF eligible",
                            "Number of years predictions are for",
                            "Number of lung cancer deaths per 1000 (LCDRAT)",
                            "Screening reduced lung cancer deaths per 1000",
                            "Number with lung cancer diagnosed per 1000 (LCRAT)",
                            "Screening increase lung cancer diagnosis per 1000",
                            "False-positive CT lung screens per 1000",
                            "Days of life gained from undergoing 3 rounds of CT lung screens",
                            "Life-expectancy without CT lung screens",
                            "Life-expectancy with 3 rounds of CT lung screens",
                            "Years of life gained if lung cancer is found early due to screening",
                            "Years of life gained if lung cancer death is averted due to screening",
                            paste0("All-cause mortality risk in ", nyears.mortality, " years") )

   predicted                   
}


