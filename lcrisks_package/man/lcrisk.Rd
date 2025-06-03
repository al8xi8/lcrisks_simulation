
\name{lcrisk}
\alias{lcrisk}
\title{Lung Cancer Death Risk Predictor}
\usage{
lcrisk(x, nyears, impute.missing=TRUE, counterfactual.race=0,
       nyears.mortality=5)
}
\arguments{
\item{x}{A numeric matrix containing individuals' covariates for the model.
 Columns 11-23 are needed only to estimate life gained from undergoing CT screening
 using the full mortality model.
 Covariates should be in the following column and format:

 \itemize{
 \item column 1 - current age (numeric);
 \item column 2 - gender (1=Female, 0=Male);
 \item column 3 - years smoked (numeric);
 \item column 4 - years quit (numeric or NA);
 \item column 5 - cigarettes per day (numeric);
 \item column 6 - race (0=Non-hispanic white,
                  1=Non-hispanic Black/African American, 
                  2=Hispanic, 
                  3=Other Ethnicity);
 \item column 7 - lung disease (1=COPD or Emphysema, 0=No COPD or Emphysema);
 \item column 8 - number of parents with lung cancer (0,1,2);
 \item column 9 - bmi;
 \item column 10 - highest education level (1=<12 grade, 
                                      2=HS graduate, 
                                      3=post hs, no college, 
                                      4=associate degree/some college, 
                                      5=bachelors degree, 
                                      6=graduate school);
 \item column 11 - prior history of cancer (1=Yes,0=No);
 \item column 12 - Hypertension (1=Yes,0=No);
 \item column 13 - Coronary Heart Disease (1=Yes,0=No);
 \item column 14 - Angina pectoris (1=Yes,0=No);
 \item column 15 - Heart Attack (1=Yes,0=No);
 \item column 16 - Other heart disease (1=Yes,0=No);
 \item column 17 - Stroke (1=Yes,0=No);
 \item column 18 - Diabetes (1=Yes,0=No);
 \item column 19 - Chronic bronchitis in past year (1=Yes,0=No);
 \item column 20 - Weak/failing kidneys in past year (1=Yes,0=No);
 \item column 21 - Liver condition in past year (1=Yes,0=No);
 \item column 22 - Health problem requiring special eqiupment (1=Yes,0=No);
 \item column 23 - Year of assessment. Calendar year assessed is used in the 
                   overall mortality model for life-expectancy to account for 
                   calendar time effects in overall mortality.                                     
 }}

\item{nyears}{Number of years to calculate risks for (numeric, max of 10).}
\item{impute.missing}{TRUE or FALSE to impute missing variables using the 
                      NHIS 2015 survey (default=TRUE). 
                      The columns that can be imputed are 7-22. 
                      The imputation assumes there is complete data for columns 1-6.}
\item{counterfactual.race}{0-3 to compute counterfactual estimates based on a different race,
                           where (0=Non-hispanic white,
                                  1=Non-hispanic Black/African American, 
                                  2=Hispanic, 
                                  3=Other Ethnicity). See details for more information. The default is 0.}
\item{nyears.mortality}{Number of years to calculate risks for all-cause mortality. 
                        A maximum age of 98 is used for this calculation. If age is
                        greater than 98, then the calculation is performed at age 98
                        and a warning is produced.}
}


\value{
A numeric matrix containing individuals' predictions:

 \itemize{
 \item column 1 - An indicator variable for whether the individual is eligible 
             for CT lung screening according to 
             US Preventive Services Task Force (USPSTF) recommendations.
 \item column 2 - Number of years predictions are for.
 \item column 3 - Among 1000 people in the US with this risk-factor profile, 
             this is the number who will die from lung cancer 
             if they do not attend screening.
 \item column 4 - In the NLST, those who underwent 3 rounds of annual CT 
             screening had their risk reduced by 20 percent.  
             Therefore, among those who would have died from lung cancer,
             this is the number who will not die from lung cancer death, 
 \item column 5 - Among 1000 people in the US with this risk-factor profile, 
             this is the number who will be diagnosed with lung cancer 
             if they do not attend screening (LCRAT).
 \item column 6 - In the NLST, those who underwent CT screening had 12.4 percent 
             more lung cancer diagnosed, all of which require treatment.  
             Therefore, among 1000 people with this risk-factor profile, 
             this is the number of extra lung cancer that would be 
             diagnosed, if they undergo 3 yearly CT lung screens as in the NLST.
 \item column 7 - Out of 1000 NLST participants with this risk profile, 
             this is the number who had at least one false-positive 
             CT screen out of 3 screens.
 \item column 8 - Days of life expectancy gained from undergoing 3 rounds of CT screening.
 \item column 9 - Life expectancy without CT screening.
 \item column 10 - Life expectancy with CT screening.
 \item column 11 - Years of life gained if lung cancer is found early due to screening.
 \item column 12 - Years of life gained if lung cancer death is averted due to screening
 \item column 13 - All-cause mortality risk
 \item column 14 - Counterfactual days of life expectancy gained from undergoing 3 rounds of CT screening.
 \item column 15 - Counterfactual life expectancy without CT screening.
 \item column 16 - Counterfactual life expectancy with CT screening.
 \item column 17 - Counterfactual years of life gained if lung cancer is found early due to screening.
 \item column 18 - Counterfactual years of life gained if lung cancer death is averted due to screening
 \item column 19 - Counterfactual all-cause mortality risk
 }
 Columns 11 and 12 are meant to be residual life-expectancy (remaining life-years).
}
\description{
In both the absence and presence of screening, the R package calculates individual risks 
of lung cancer and lung cancer death based on covariates: age, education, sex, race, 
smoking intensity/duration/quit-years, Body Mass Index, family history of lung-cancer, 
and self-reported emphysema.  In the presence of CT screening akin to the NLST 
(3 yearly screens, 5 years of follow-up), it uses the covariates to estimate risk of 
false-positive CT screen as well as the reduction in risk of lung cancer death and 
increase in risk of lung cancer screening.  
This package also estimates the Life Years Gained From Screening-CT (LYFS-CT) 
as per Cheung et al., 2019. A basic mortality model can be fitted using only age, gender,
race, and smoking variables. To use the full mortality model, comorbidities and the year 
of patient assessment should be included.
}

\section{Model Objects in Package}{

 \itemize{
   \item LCDRAT - model for lung cancer death in absence of screening;
   \item LCRAT - model for lung cancer incidence in absence of screening;
   \item cox.death - model for deaths from causes other than lung cancer;
   \item morat - model for overall mortality;
   \item polytmod - polytomous model for false positive CT lung screens.
  }
}

\details{
 The age range for estimation is from 40-98; results may not be accurate for ages outside of this range. 
 The \code{counterfactual.race} option is used to compute additional life expectancy and life gained
 estimates. These estimates will appear as columns with the prefix \code{Counterfactual} in the 
 returned object.
}



\examples{
age <- c(66,58,75,72,56)
bmi <- c(23,28,26,27,24)
cpd <- c(36,36,40,24,40)
emp <- c(0,1,1,0,1)
fam.lung.trend <- c(0,2,0,2,0)
female <- c(0,1,0,1,0)
smkyears <- c(43,37,45,42,29)
qtyears <- c(NA,NA,9,6,6)
race <- c(0,1,2,2,3)
edu6 <- c(3,5,4,5,5)
prior.cancer <- c(0,0,0,0,0)
hypertension <- c(0,0,1,0,1)
chd <- c(0,0,0,0,0)
angina <- c(0,0,0,0,0)
heartattack <- c(0,0,0,0,1)
heartdisease <- c(0,0,0,0,0)
stroke <- c(0,0,0,0,0)
diab <- c(1,0,0,0,0)
bron <- c(0,1,0,0,1)
kidney <- c(0,0,0,0,0)
liver <- c(0,0,0,0,0)
speceq <- c(0,1,0,0,0)
yearassessed <- rep(2019,5)
years <- 5

persons <- cbind(age,
                 female,
                 smkyears,
                 qtyears,
                 cpd,
                 race,
                 emp,
                 fam.lung.trend,
                 bmi,
                 edu6,
                 prior.cancer,
                 hypertension,
                 chd,
                 angina,
                 heartattack,
                 heartdisease,
                 stroke,
                 diab,
                 bron,
                 kidney,
                 liver,
                 speceq,
                 yearassessed)
                     
persons_predictions <- lcrisk(persons,years)
persons_predictions
}
\references{
\itemize{
 \item Katki HA, Kovalchik SA, Berg CD, Cheung LC, Chaturvedi AK.
            Development and validation of risk models to select ever-smokers
            for CT lung cancer screening. JAMA. 2016; 315(21):2300-2311.
            doi: 10.1001/jama.2016.6255.
 \item Cheung LC, Berg CD, Castle PE, Katki HA, Chaturvedi AK.
       Life-gained-based versus risk-based selection of smokers for lung cancer screening.
       Ann Intern Med.;171:623-632. doi:107326/M19-1263.  

}
}
\author{
Li C. Cheung, \email{li.cheung@nih.gov}, Stephanie A. Kovalchik, Hormuzd A. Katki
}
