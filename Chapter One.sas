/******************Program 1.1****************/
data SP500FIN;
input  Date MMDDYY10. SPS EPS DPR PE;
format Date MMDDYY10. SPS Dollar10. EPS Dollar10.;
label SPS = 'Sales Per Share' EPS ='Earnings Per Share' DPR= 'Dividend Payout Ratio' PE  = 'Price-to-Earnings Ratio';
datalines;
12/31/2015 1106.96 89.73 53.38 18.73
12/30/2016 1128.45 98.90 52.35 20.44
12/29/2017 1210.13 109.99 52.11 21.48
12/31/2018 1313.58 133.01 46.10 16.64
12/31/2019 1391.09 140.42 52.45 20.86
12/31/2020 1342.44 97.00 72.10 30.37
12/31/2021 1541.77 200.35 37.18 24.71
12/30/2022 1708.91 188.41 39.34 18.61
;
run;

/******************Program 1.2****************/
proc sort data=SP500FIN ;
by Date;
run;

proc print data=SP500FIN ;
run;

/******************Program 1.3****************/
proc sort data=SP500FIN OUT=DSorted_SP500FIN;
By descending Date;
run;

proc print data=DSorted_SP500FIN;
run;

/******************Program 1.4****************/
title 'Annual Sales Per Share and Earnings Per Share for the S&P 500 Index';
proc sgplot data= SP500FIN;
	series x=Date y=SPS ;
	series x=Date y=EPS ;
	xaxis grid;
	yaxis grid;
run;
title;
/*************************************************/
/*****WARNING**********************WARNING********/
/*This code will delete your local SASGit Repository
Use only if you are sure that you don't need any of the files
in it. You should first try to manually clear your directory
before using this program*/

/*data _null_;
   rc=git_delete_repo("&datapath.");
   put rc=;
run;
*/

/******************Program 1.5****************/
options dlcreatedir;
%let datapath = %sysfunc(getoption(WORK))/finsasdata;
libname findata "&datapath.";
run;

data _null_;
 rc = git_clone("https://github.com/finsasdata/Bookdata/",
   "&datapath.");
    %put rc=;
run;


/******************Program 1.6****************/
Proc datasets library=findata; 
run;
quit;

/******************Program 1.7****************/
filename stocks "%sysfunc(getoption(WORK))/stocks.sas7bdat";
proc http url="https://github.com/finsasdata/Bookdata/raw/main/stocks.sas7bdat"
   out=stocks
   method ="get";
run;

proc print Data=Stocks (where= (Stock='AMZN' and Date> '31Dec2021'D));
format volume comma13.;
Run;

/******************Program 1.8A****************/
Data NSP500FIN;
Set SP500FIN;
SPSG=LOG(SPS/LAG(SPS));
EPSG=LOG(EPS/LAG(EPS));
DPRG = LOG(DPR/LAG(DPR));
PEG = PE/(EPSG*100);
Label SPSG = 'Sales Growth Rate' EPSG ='Earnings Growth Rate' DPRG='Dividend Payout Ratio Growth Rate'
PEG = 'Price-to-Earning Growth Ratio';
format SPSG percent8.2 EPSG percent8.2 DPRG percent8.2 PEG bestd6.;
run;

proc print data=NSP500FIN;
run;

proc means data=NSP500FIN mean stddev min median max Nolabels;
var SPSG EPSG DPRG PEG;
run;

/******************Program 1.8B****************/
proc tabulate data = NSP500FIN;
var SPSG EPSG DPRG PEG;
table SPSG*F=percent8.2  EPSG*F=percent8.2 DPRG*F=percent8.2 PEG  , 	mean stddev median max;
run;

/******************Program 1.8C****************/
title 'Annual EPS Growth Rate and PEG Ratios for the S&P 500 Index';
proc sgplot data= NSP500FIN (where=(date>'31Dec2015'D));
	series x=Date y=EPSG ;
	series x=Date  y=PEG /Y2AXIS;
	xaxis values=('31Dec2016'd to '31Dec2022'd by year) ;
	yaxis grid;
run;
title;

/******************Program 1.9A****************/
filename SPX "%sysfunc(getoption(WORK))/SPX_Members.xlsx";
proc http
   url="https://github.com/finsasdata/Bookdata/raw/main/SPX_Members.xlsx"
   out=SPX
   method ="get";
run;


/******************Program 1.9B****************/
/*PROC IMPORT statement below is used to import files from various 
systems into SAS. The set of code below imports the SPX_Members.xlsx 
file from it current location on the computer into the SPX_Members SAS
datafile in the WORK library*/

proc import out=SPX_Members
datafile= spx
dbms= xlsx
replace;
getnames= YES;
sheet= Sheet1;
run;


/******************Program 1.9C****************/
ods graphics on;
proc anova Data= SPX_Members;
title 'Anova Test of Equality of Industry Performance';
class Sector;
model ROAG5Y = SECTOR;
/*Lets also test for equality of variance by including the code below. If the p-value of the Levene test rejects the null of equal variance, then the Welch Anova Test of the Equality of means will be used*/
means SECTOR/hovtest=levene welch;
run;
title;
ods graphics off;

/******************Program 1.9C****************/
%let smean=0.0067658; %let ssd = 0.04465726;Data FSPX;
set ustats;
do iter=1 to 100; /*number of replication*/
            do time = 1 to 457; /*Simulation window*/
         	fmret =rand("normal",&smean,&ssd);
	   		mcount+1; 
		    output;
         end;
         mcount=0;
	/*sumret is cumulative sum of returns over the sample period*/
     end;
label fmret='Forecasted Monthly Returns';
run;

/* To Extract the Descriptive Statistic for the Simulated Returns*/
proc tabulate data=FSPX;
class iter;
var  fmret;
table iter*fmret,mean*F=percent8.2 stddev*F=percent8.2;
table fmret,mean*F=percent8.2 stddev*F=percent8.2;
run;
