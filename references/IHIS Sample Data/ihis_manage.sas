LIBNAME IPUMS "C:\Users\Pianpian Cao\Documents\OldDesktopWin7\Documents\Documents\LCS model\IHIS";

DATA IPUMS.recode;
	set IPUMS.NHIS_00083;
	/*Age*/
	currentage = AGE;
	/*Education*/ 
	IF EDUC IN (000,504,996,997,998,999) THEN education = .; *504-other degree;
	ELSE DO;
		IF EDUC IN (500,501,502,503,505) THEN education = 6; *professional degree;
		ELSE IF EDUC = 400 THEN education = 5; *college graduate;
		ELSE IF EDUC IN (300,301) THEN education = 4; *some college;
		ELSE IF EDUC IN (302,303) THEN education = 3; *some training after high school;
		ELSE IF EDUC IN (200,201,202) THEN education = 2; *high school graduate or GED;
		ELSE education=1; *less than high school;
	END;
	/*BMI*/
	IF BMI >= 96 THEN BMI = .;
	/*Recode race*/ *modify this;
	/*1-white, 2-black, 3-hispanic, 4-asian, 5-AIAN, 6-NHPI*/
	IF HISPETH IN (70,90,91,92,93,99) THEN race=.;
	ELSE IF HISPETH = 10 then do;
		if racea = 100 then race = 1;
		else if racea = 200 then race = 2;
		else if racea in (411,412,416,434) then race=4; *Asian;
		else if racea = 310 then race = 5; *AIAN;
		else racea = .;
	end;
	else race = 3; *hispanic;
	/*COPD History*/
	if(CRONBRONYR in (7,8,9) and EMPHYSEMEV in (7,8,9)) then copd=.;
	else do;
		if(CRONBRONYR = 2 or EMPHYSEMEV = 2) THEN copd = 1; *YES-2;
		else copd=0;
	end;
	/*Personal Cancer History*/
	if(CANCEREV in (7,8,9)) then personalhistory=.;
	else do;
		if(CANCEREV = 2) THEN personalhistory = 1;
		else personalhistory = 0;
	end;
	/*Personal lung cancer history*/
	if(CNLUNG = 2) THEN lchistory = 1;
	else if (CNLUNG = 1) THEN lchistory = 0;
	else lchistory = .;
	/*Family Lung Cancer History*/
	IF(YEAR IN (2000,2005,2010,2015)) THEN DO;
		ARRAY FAMHIST(6) BFLGCAN BMLGCAN BSLGCAN BDLGCAN FSLGCAN FBLGCAN;
		SUM=0;
		fammiss=0;
		DO i = 1 to 6;
			IF FAMHIST(i) = 2 THEN SUM = SUM+1; *HAD LUNG CANCER;
			ELSE IF FAMHIST(i) = 1 THEN SUM = SUM+0; *NO LUNG CANCER;
			ElSE fammiss=fammiss+1;
		END;
		IF SUM=1 THEN FMHIST=1;
		ELSE IF SUM>1 THEN FMHIST=2;
		ELSE IF SUM=0 THEN FMHIST=0;
		IF FAMMISS=6 THEN FMHIST=.; *SET THOSE WITHOUT ANY CANCER INFORMATION AS MISSING;
		DROP SUM i;							
	END;
	ELSE FMHIST = .;
	/*Smoking status*/
	if(SMOKESTATUS2 IN (11,12)) then do;
		currentsmokingstatus = 1;
		currentyearsquit = 0;
	end;
	else if(SMOKESTATUS2 = 20) then currentsmokingstatus = 0; *former;
	else currentsmokingstatus = .;
	smkstat=.;
	IF smkstat = . THEN DO;
		IF smokestatus2=30 THEN smkstat=1;
		ELSE IF smokestatus2 in (11:13) THEN smkstat=2; 
		ELSE IF smokestatus2=20 THEN smkstat=3;
		*ELSE IF smokestatus2 in (00,90,40) THEN smkstat=.;
	END;
	/*CPD FOR FORMER SMOKERS*/
	IF YEAR IN (2000,2005, 2010, 2015) THEN DO;
		IF CIGSDAYFS IN (01:94) THEN CPDformer = CIGSDAYFS;
		ELSE IF CIGSLONGFS IN (01:95) THEN CPDformer = CIGSDAYFS;
		ELSE CPDformer=.;
	END;
	ELSE CPDformer=.;
	/*Cigarettes per day FOR current smokers*/
	if(CIGSDAY>=1 and CIGSDAY<96) then CPDcurrent = CIGSDAY;
	else if(CIGSDAY1>=1 and CIGSDAY1<96) then CPDcurrent = CIGSDAY1;
	else if(CIGSDAY2>=1 and CIGSDAY2<96) then CPDcurrent = CIGSDAY2;
	else CPDcurrent = .;
	/*CPD for all*/
	if smkstat=2  then averageCPD = CPDcurrent;
	else if smkstat=3  then averageCPD = CPDformer;
	else if smkstat=1 then averageCPD = 0;
	else averageCPD = .;
	/*Smoking quit time*/
	if(QUITYRS>=0 and QUITYRS<=70) then currentyearsquit = QUITYRS; *quityrs=0: quit less than a year;
	else currentyearsquit = .;
	/*reassign smoking status for quit year less than 1 individuals*/
	IF currentyearsquit = 0 and currentsmokingstatus = 0 THEN currentsmokingstatus = 1;
	/*reassign quit years for current smokers*/
	IF currentyearsquit = . and currentsmokingstatus = 1 THEN currentyearsquit = 0;
	/*Smoking duration*/
	if(SMOKAGEREG>0 and SMOKAGEREG<=85)then do;
		smokeage = smokagereg;
		if currentyearsquit>0 then currentsmokingduration = AGE - SMOKAGEREG - currentyearsquit;
		else do;
			if currentsmokingstatus=1 then currentsmokingduration = AGE - SMOKAGEREG;
			else currentsmokingduration = .;
		end;
	end;
	else do;
		smokeage = .;
		currentsmokingduration = .;
	end;
	/*Pack year*/
	pky = averageCPD*currentsmokingduration/20;
	/*Comorbidity*/
	/*	column 12 - Hypertension (1=Yes,0=No);

		column 13 - Coronary Heart Disease (1=Yes,0=No);

		column 14 - Angina pectoris (1=Yes,0=No);

		column 15 - Heart Attack (1=Yes,0=No);

		column 16 - Other heart disease (1=Yes,0=No);

		column 17 - Stroke (1=Yes,0=No);

		column 18 - Diabetes (1=Yes,0=No);

		column 19 - Chronic bronchitis in past year (1=Yes,0=No);

		column 20 - Weak/failing kidneys in past year (1=Yes,0=No);

		column 21 - Liver condition in past year (1=Yes,0=No);

		column 22 - Health problem requiring special eqiupment (1=Yes,0=No);

		column 23 - Year of assessment.*/
	IF HYPERTENEV = 1 then HYPER=0;
	ELSE IF HYPERTENEV = 2 THEN HYPER=1;
	ELSE HYPER=.;
	
	IF CHEARTDIEV = 1 THEN CORONARY=0;
	ELSE IF CHEARTDIEV = 2 THEN CORONARY=1;
	ELSE CORONARY=.;

	IF ANGIPECEV = 1 THEN ANGINA=0;
	ELSE IF ANGIPECEV = 2 THEN ANGINA=1;
	ELSE ANGINA=.;

	IF HEARTATTEV = 1 THEN ATTACK=0;
	ELSE IF HEARTATTEV = 2 THEN ATTACK=1;
	ELSE ATTACK=.;

	IF HEARTCONEV = 1 THEN OTHERH=0;
	ELSE IF HEARTCONEV = 2 THEN OTHERH=1;
	ELSE OTHERH=.;
	
	IF STROKEV = 1 THEN STROKE=0;
	ELSE IF STROKEV = 2 THEN STROKE=1;
	ELSE STROKE=.;

	IF  DIABETICEV = 1 THEN DIABETE=0;
	ELSE IF DIABETICEV = 2 THEN DIABETE=1;
	ELSE DIABETE=.;
	
	IF CRONBRONYR = 1 THEN CHRBRON=0;
	ELSE IF CRONBRONYR = 2 THEN CHRBRON=1;
	ELSE CHRBRON=.;

	IF KIDNEYWKYR = 1 THEN KIDNEY=0;
	ELSE IF KIDNEYWKYR = 2 THEN KIDNEY=1;
	ELSE KIDNEY=.;

	IF LIVERCONYR = 1 THEN LIVER=0;
	ELSE IF LIVERCONYR = 2 THEN LIVER=1;
	ELSE LIVER=.;

	IF EQUIPMENT = 1 THEN EQUIP=0;
	ELSE IF EQUIPMENT = 2 THEN EQUIP=1;
	ELSE EQUIP=.;
RUN;

proc surveyfreq data=IPUMS.input4080ever;
table race;
weight SAMPWEIGHT;
where year=2015;
run;

PROC SURVEYMEANS DATA=IPUMS.recode;
VAR CPDformer;
WEIGHT SAMPWEIGHT;
RUN;


PROC MEANS DATA=IPUMS.recode;
VAR CPDformer;
where year=2015;
RUN;

proc freq data=ipums.recode;
table smkstat;
run;

*exclude never smoker, persons with lung cancer history, persons with missing smoking information: cigarette per day, smoking duration;

PROC DATASETS lib=IPUMS memtype=data;
	MODIFY recode;
	attrib _all_ label=' '; 
    attrib _all_ format=;
	attrib _all_ informat=;
RUN;
QUIT;

*50-80 EVER SMOKERS 2010-2015;
DATA IPUMS.input5080ever; *not excluding persons without CPD info;
	SET IPUMS.recode; 
	IF currentage >=50 AND currentage <= 80;
	IF YEAR>=2010 AND YEAR<=2015;
	IF currentsmokingstatus NE . AND lchistory NE 1;
	IF smkstat = 2 or smkstat = 3;
	KEEP SEX currentage education bmi copd personalhistory FMHIST race currentsmokingstatus smkstat
		averageCPD currentsmokingduration currentyearsquit PKY HHX FMX PX PERWEIGHT SAMPWEIGHT FWEIGHT HHWEIGHT YEAR smokeage 
		HYPER CORONARY ANGINA ATTACK OTHERH STROKE DIABETE CHRBRON KIDNEY LIVER EQUIP;
RUN; 

*40-80 EVER SMOKERS 2010-2018;

DATA IPUMS.input4080ever; *not excluding persons without CPD info;
	SET IPUMS.recode; 
	IF currentage >=40 AND currentage <= 80;
	IF YEAR>=2010; *AND YEAR<=2015;
	IF currentsmokingstatus NE . AND lchistory NE 1;
	IF smkstat = 2 or smkstat = 3;
	KEEP SEX currentage education bmi copd personalhistory FMHIST race currentsmokingstatus smkstat
		averageCPD currentsmokingduration currentyearsquit PKY HHX FMX PX PERWEIGHT SAMPWEIGHT FWEIGHT HHWEIGHT YEAR smokeage 
		HYPER CORONARY ANGINA ATTACK OTHERH STROKE DIABETE CHRBRON KIDNEY LIVER EQUIP;
RUN; 

*2015 data;

DATA IPUMS.input4080ever_2015; *not excluding persons without CPD info;
	SET IPUMS.recode; 
	array variable{10} HYPER--LIVER;
	IF currentage >=40 AND currentage <= 80;
	IF YEAR=2015; *AND YEAR<=2015;
	IF currentsmokingstatus NE . AND lchistory NE 1 AND averageCPD NE . AND FMHIST NE .;
	IF smkstat = 2 or smkstat = 3;
	
	do i=1 to 10;
	if variable{i} ne .;
	end;

	KEEP SEX currentage education bmi copd personalhistory FMHIST race currentsmokingstatus smkstat
		averageCPD currentsmokingduration currentyearsquit PKY HHX FMX PX PERWEIGHT SAMPWEIGHT FWEIGHT HHWEIGHT YEAR smokeage 
		HYPER CORONARY ANGINA ATTACK OTHERH STROKE DIABETE CHRBRON KIDNEY LIVER EQUIP;
RUN; 

DATA IPUMS.input4080ever_2015;
retain SEX currentage education bmi copd personalhistory FMHIST race currentsmokingstatus 
		averageCPD currentsmokingduration currentyearsquit smokeage HYPER CORONARY ANGINA ATTACK OTHERH STROKE DIABETE CHRBRON KIDNEY LIVER EQUIP SAMPWEIGHT;
set Ipums.input4080ever_2015;

KEEP SEX currentage education bmi copd personalhistory FMHIST race currentsmokingstatus 
		averageCPD currentsmokingduration currentyearsquit smokeage HYPER CORONARY ANGINA ATTACK OTHERH STROKE DIABETE CHRBRON KIDNEY LIVER EQUIP SAMPWEIGHT;

RUN;


PROC SORT DATA=IPUMS.input5080ever; 
by smkstat year;
run;

PROC MEANS DATA=IPUMS.input5080ever; 
VAR averageCPD;
by smkstat year;
where year in (2010, 2015);
run;

PROC FREQ DATA=IPUMS.input4080ever;
TABLE personalhistory FMHIST race SMKSTAT HYPER CORONARY ANGINA ATTACK OTHERH STROKE DIABETE CHRBRON KIDNEY LIVER EQUIP education;
RUN;

PROC FREQ DATA = IPUMS.RECODE;
TABLE OTHERH;
RUN;
PROC MEANS DATA = IPUMS.input;
	VAR currentsmokingduration;
RUN;

PROC PRINT DATA = IPUMS.input_elig;
	where currentyearsquit = . ;
RUN;
DATA IPUMS.INPUT_ELIG; *pack year>=30 or pack year=.;
	RETAIN SEX currentage education bmi copd personalhistory familyhistory race currentsmokingstatus 
		averageCPD currentsmokingduration currentyearsquit smokeage WEIGHT YEAR HHX FMX PX PERWEIGHT SAMPWEIGHT FWEIGHT HHWEIGHT;
	SET IPUMS.INPUT;
	IF currentage >= 55 AND currentage <= 74 AND currentyearsquit <=15;
	*IF smokeage > 0;
	IF year in (2010:2014);
	WEIGHT=SAMPWEIGHT/5;
	KEEP SEX currentage education bmi copd personalhistory familyhistory race currentsmokingstatus 
		averageCPD currentsmokingduration currentyearsquit smokeage WEIGHT YEAR HHX FMX PX PERWEIGHT SAMPWEIGHT FWEIGHT HHWEIGHT;
RUN; *13323; *13229;

DATA IPUMS.INPUT_ELIG80; *pack year>=30 or pack year=.;
	RETAIN SEX currentage education bmi copd personalhistory familyhistory race currentsmokingstatus 
		averageCPD currentsmokingduration currentyearsquit smokeage WEIGHT YEAR HHX FMX PX PERWEIGHT SAMPWEIGHT FWEIGHT HHWEIGHT;
	SET IPUMS.INPUT;
	IF currentage >= 55 AND currentage <= 80 AND currentyearsquit <=15; *remove pack year constrain;
	IF smokeage > 0;
	IF year in (2010:2014);
	WEIGHT=SAMPWEIGHT/5;
	KEEP SEX currentage education bmi copd personalhistory familyhistory race currentsmokingstatus 
		averageCPD currentsmokingduration currentyearsquit smokeage WEIGHT YEAR HHX FMX PX PERWEIGHT SAMPWEIGHT FWEIGHT HHWEIGHT;
RUN; *14532; *14422;

DATA IPUMS.INPUT_ELIG80; *pack year>=30 or pack year=.;
	RETAIN SEX currentage education bmi copd personalhistory familyhistory race currentsmokingstatus 
		averageCPD currentsmokingduration currentyearsquit smokeage WEIGHT YEAR HHX FMX PX PERWEIGHT SAMPWEIGHT FWEIGHT HHWEIGHT;
	SET IPUMS.INPUT;
	IF currentsmokingduration ne 0;
	IF currentage >= 55 AND currentage <= 80 AND currentyearsquit <=15; *remove pack year constrain;
	IF smokeage > 0;
	IF year in (2010:2014);
	WEIGHT=SAMPWEIGHT/5;
	KEEP SEX currentage education bmi copd personalhistory familyhistory race currentsmokingstatus 
		averageCPD currentsmokingduration currentyearsquit smokeage WEIGHT YEAR HHX FMX PX PERWEIGHT SAMPWEIGHT FWEIGHT HHWEIGHT;
RUN; *14532; *14422;
proc freq data = IPUMS.INPUT_ELIG80;
	TABLE copd personalhistory;
run;

proc print data=IPUMS.INPUT_ELIG80;
where currentsmokingduration<0 and currentsmokingduration ne .;
run;

DATA IPUMS.INPUT_ELIG80_table2; *pack year>=30 or pack year=.;
	RETAIN SEX currentage education bmi copd personalhistory familyhistory race currentsmokingstatus 
		averageCPD currentsmokingduration currentyearsquit smokeage WEIGHT YEAR HHX FMX PX PERWEIGHT SAMPWEIGHT FWEIGHT HHWEIGHT;
	SET IPUMS.INPUT;
	IF currentage >= 55 AND currentage <= 80 AND currentyearsquit <=15 AND PKY>=30; *remove pack year constrain;
	IF smokeage > 0;
	IF year in (2010);
	WEIGHT=SAMPWEIGHT/5;
	KEEP SEX currentage education bmi copd personalhistory familyhistory race currentsmokingstatus 
		averageCPD currentsmokingduration currentyearsquit smokeage WEIGHT YEAR HHX FMX PX PERWEIGHT SAMPWEIGHT FWEIGHT HHWEIGHT AGEGROUP;
	IF currentage>=55 AND currentage<=59 THEN AGEGROUP=1;
	ELSE IF currentage<=64 THEN AGEGROUP=2;
	ELSE IF currentage<=69 THEN AGEGROUP=3;
	ELSE IF currentage<=74 THEN AGEGROUP=4;
	ELSE AGEGROUP=5;
RUN; 

PROC SURVEYFREQ DATA = IPUMS.INPUT_ELIG80_table2;
	TABLE AGEGROUP sex race currentsmokingstatus ;
	WEIGHT WEIGHT;
RUN;

PROC MEANS DATA = IPUMS.input_elig mean nmiss n;
	VAR bmi averageCPD currentsmokingduration currentyearsquit WEIGHT;
RUN;

PROC FREQ DATA = IPUMS.input_elig;
	TABLE SEX COPD personalhistory familyhistory currentsmokingstatus;
RUN;
DATA IPUMS.INPUT_ELIG0510;
	RETAIN SEX currentage education bmi copd personalhistory familyhistory race currentsmokingstatus 
		averageCPD currentsmokingduration currentyearsquit WEIGHT YEAR HHX FMX PX PERWEIGHT SAMPWEIGHT FWEIGHT HHWEIGHT;
	SET IPUMS.INPUT;
	IF currentage >= 55 AND currentage <= 74 AND Pky >= 30 AND currentyearsquit <=15;
	IF year in (2005,2010);
	WEIGHT=SAMPWEIGHT/2;
	KEEP SEX currentage education bmi copd personalhistory FMHIST race currentsmokingstatus 
		averageCPD currentsmokingduration currentyearsquit WEIGHT YEAR HHX FMX PX PERWEIGHT SAMPWEIGHT FWEIGHT HHWEIGHT;
RUN;

PROC freq DATA=IPUMS.input;
	table currentsmokingstatus;
RUN;

PROC MEANS DATA=IPUMS.input;
	VAR averageCPD;
	WHERE currentsmokingstatus = 0;
RUN;

PROC SORT DATA = IPUMS.recode; BY YEAR;
PROC FREQ DATA = IPUMS.recode;
	TABLE currentsmokingstatus*CIGSDAYFS;
	BY YEAR;
RUN;

PROC FREQ DATA = IPUMS.INPUT_ELIG0510;
TABLE personalhistory;
RUN;
PROC FREQ DATA = IPUMS.IHIS_00009;
TABLE CNLUNG;
RUN;
proc contents data = ipums.recode;
run;
