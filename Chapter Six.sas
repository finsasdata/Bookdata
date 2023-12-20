/*********Program 6.1**************************************
/*SAS Codes to show different parametric method of estimation*/
%datapull(industr,industr.sas7bdat);

proc model data=industr;
	finance = constant +beta*mrp;
	fit finance / ols; /*OLS*/
	fit finance / fiml; /*MLE*/
	fit finance / gmm;/*GMM*/
run;


/*********Program 6.2**************************************
/*Estimating Kernel Density Using PROC UNIVARIATE*/
title 'Distribution of Financial Services Industry Returns';

proc univariate data=industr noprint;
	histogram  finance/ 
		kernel(c = 0.5 1 2 
		noprint  
		k=normal)  
		odstitle = title;
run;


/*********Program 6.3**************************************
/*Estimating Univariate Bivariate Kernel Densities Using PROC KDE*/
title 'Distribution of Financial Services Industry and Market Returns';
ods graphics on;

proc KDE data=industr;
	univar finance(bwm=1) mrp(bwm=1) /plots=densityoverlay;
	bivar finance mrp/ plots=all;
run;


/*********Program 6.4**************************************
/*Using PROC LOESS to Fit the Index Model*/
ods graphics on;

%datapull(finscore,finscore.sas7bdat);

proc loess data=industr plots=all;
	model finance=mrp/ all details  degree=1 
		smooth=0.1;
	score data=finsas.finscore /clm print(var=finance);
run;


/*********Program 6.5**************************************/
/*Estimating Index Model Using Entropy Procedure*/
proc entropy data= industr maxiter=50 outp=entprob plots=all;
	priors mrp  0.5(0.5) 1.5(0.5);
	model finance = mrp /esupports= (-1.0(0.5) 1.0(0.5));
run;

proc print data=entprob;
run;



/*********Program 6.6**************************************/
proc product_status;
run;



/*********Program 6.7**************************************/
/* Using PROC PROBIT to Model the Determinants of Long-term Stock Return*/
%datapull(profit,spx_profit.sas7bdat);
ods graphics on;

proc probit data=spx_profit plot=all;
	class ceoduality sector;
	model pret5(event='0') =  ceo_tenure logmcap logemp 
		Instshrout pe esg wacc age ceoduality sector;
	effectplot contour( x=Instshrout y=logemp);
run;



/*********Program 6.8**************************************/
/* Using PROC LOGISTIC to Model the Determinants of Long-term Stock Return*/
ods graphics on;

proc logistic  data=spx_profit plot=all;
	class ceoduality sector;
	model pret5(event='0') = CEO_Tenure logmcap logemp 
		Instshrout pe esg wacc age ceoduality sector;
	effectplot contour(x=Instshrout y=logemp);
run;



/*********Program 6.9A**************************************/
/*Determinant of Long-Term Stock Returns*/
%datapull(profit,spx_profit.sas7bdat);

proc stepdisc data=spx_profit include=0 short;
	class pret5;
	var ceo_tenure logmcap logemp 
		Instshrout pe esg wacc age;
run;


/*********Program 6.9B**************************************/
/*Determinant of Long-Term Stock Returns*/
proc discrim data=spx_profit method=normal crossvalidate 
outstat=distat outd=discore  outcross=cvalid posterr distance anova manova pool=yes canonical ncan=2;
	class pret5;
	var logmcap logemp Instshrout wacc;
	priors proportional;
run;


/*********Program 6.9C**************************************/
/*Determinants of Long-Term Stock Returns*/
proc discrim data=spx_profit method=npar kernel=normal r=.1 crossvalidate 
	outstat=distat outd=discore outcross=cvalid posterr distance anova manova pool=yes;
	class pret5;
	var logmcap logemp Instshrout wacc;
	priors  equal;
run;


/*********Program 6.10**************************************/
/*Estimating Statistical Factor Model of Bank Stock Return*/
%datapull(banks,banks.sas7bdat);
ods graphics on;

proc factor data= banks method=ml 
	score nfact=4 priors=smc heywood  
	rotate=quartimin plots=all  
	outstat = factstats out=factfactors;
	var rbac rwfc rpnc rjpm rusb rc rtfc rhban rmtb rrf rzion rfitb rcfg rkey rcma;
	pathdiagram  fuzz=0.3 arrange=grip scale=0.85 title='Quartimin-Rotated Path Diagram' ;
run;


/*********Program 6.11**************************************/
/*Principal Component Analysis of Determinants of Long-Term Profitability*/
%datapull(profit,spx_profit.sas7bdat);
ods graphics on;

proc princomp data  = spx_profit standard
out=pcscores(label="original data and principal components scores for work.spx_profit")
outstat=pcstats(label="principal components statistics for work.spx_profit")
   	prefix='comp#'n		vardef=df
	plots(only)=scree
	plots(only)=matrix
	plots(only)=patternprofile
	plots(only)=pattern ;
	var logrev logmcap logemp logshrout age beta esg pct_wboard pe ceo_tenure logvol;
run;

proc princomp data  = spx_profit standard
out=pcscores(label="original data and principal components scores for work.spx_profit")
outstat=pcstats(label="principal components statistics for work.spx_profit")
   	prefix='comp#'n		vardef=df
	plots=all;
	var logrev logmcap logemp logshrout age beta esg pct_wboard pe ceo_tenure logvol;
run;

/*********Program 6.12A**************************************/
/*Estimating Survival Function Credit Obligors Using PROC LIFETEST*/
%datapull(loans,lcloans.sas7bdat);
ods graphics on;

proc lifetest data=lcloans plots=(s(atrisk),ls,lls,h(kernel=epanechnikov) cif)
	method=pl nelson  intervals=0 to 50 by 1  outsurv=sdata;
	time timetd*status(0)/;
run;


/*********Program 6.12B**************************************/
/*Comparing Survival Function Credit Obligors Using PROC LIFETEST*/
ods graphics on;

proc lifetest data=lcloans method=pl  plots=(s h ls lls) notable outtest=sest intervals=0 to 50 by 1  outsurv=sdata;;
	time timetd*status(0);
	strata term/order=internal test=(lr logrank);
	test dti int_rate;
run;


/*********Program 6.13**************************************/
/*Estimating Proportional Hazard Models of Credit Obligors Using PROC PHREG*/
ods graphics on;

proc phreg data=lcloans plots=(survival cumhaz roc auc) 
	rocoptions(at=0 to 40 by 10) simple;
	class application_type grade term purpose home_ownership verification_status emp_length;
	model timetd*status(0)= loan_amnt int_rate annual_inc dti pub_rec application_type total_acc revol_util grade term purpose home_ownership
		verification_status emp_length / selection=stepwise slentry=0.25 slstay=0.15 details;
run;



/*********Program 6.14**************************************/
/*Using PROC Syslin to Model the Determinant of Stock return and Volume*/
%datapull (market,market_data.sas7bdat);
ods graphics on;

proc syslin data=market_data 2sls out=predicted;
	endogenous dret vret;
	instruments rbond roil rexr rvix rgvix rovix ldret lvret;
return:
	model dret= vret lvret rbond roil rexr;
	output predicted=pdret ;
volume:
	model vret= dret ldret rvix rgvix rovix;
	output predicted=pvret;
run;

proc sgplot data=predicted;
series x=Date y=pdret/y2axis;
series x=Date y=dret;
xaxis label= 'Daily Returns' values=('01jan2009'D to '20dec2022'd) interval=year valuesformat=YY.;
run;


/*********Program 6.15**************************************/
/*Modeling the Relationship between Unemployment and Consumption*/
%datapull (conswork,conswork.sas7bdat);
ods graphics on;

proc varmax data=conswork plots=all;
	id date interval=month;
	model  pce unrate = / p=1 method=ml print=(impulse(12) decompose(12));
	causal group1=(pce) group2=(unrate);
	causal group1=(unrate)  group2=(pce);
	output lead= 12 out=forecast;
run;


/*********Program 6.16**************************************/
/*Forecasting Monthly Trading Volume on the S&P 500 using Exponsential Smoothing Model*/
%datapull(spx,rspx_monthly.sas7bdat);
ods graphics on;

proc esm data=rspx_monthly back=12 lead=24 plot=forecastsonly
outest=sesmparms outstat=sesmstats print=all;
	id date interval=monthly;
	forecast volume /model=simple;
run;

proc esm data=rspx_monthly back=12 lead=24 plot=forecastsonly
outest=desmparms outstat=desmstats print=all;
	id date interval=monthly;
	forecast volume /model=linear;
run;

proc esm data=rspx_monthly back=12 lead=24 plot=forecastsonly
outest=tesmparms outstat=tesmstats print=all;
	id date interval=monthly;
	forecast volume /model=multwinters;
run;
title 'Forecasting S&P 500 Volume Using Simple ESM';
proc sgrender data=sesmparms template=ETS.ESM.ParameterEstimates;
run;
title 'Forecasting S&P 500 Volume Using Holts Double ESM';
proc print data=desmparms;
run;
title 'Forecasting S&P 500 Volume Using Additive Holt-Winters Triple ESM';
proc print data=tesmparms;
run;
title;


title 'Fit Statistic from Forecasting S&P 500 Volume Using ESM';
proc print data=sesmstats;
proc print data=desmstats;
proc print data=tesmstats;
run;
title;


/*********Program 6.17**************************************/
/*Generating Monthly Forecast of the Wilshire 5000 Index Returns Using UCM*/
%datapull (conswork,conswork.sas7bdat);
ods graphics on;

proc ucm data=conswork;
	id date interval=monthly;
	model will5000 = t10yr unrate pce jby;
	irregular;
	level;
	cycle;
	season type=dummy length=12;
	estimate  outest=ucmparms;
	forecast back=24 lead=36 plot=(decomp decompvar forecasts );
	nloptions tech=dbldog maxiter=200;
run;

proc print data=ucmparms;
run;
