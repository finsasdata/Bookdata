*libname finsas 'C:\Users\Owner\OneDrive - Widener University\Financial Data Science\Data\Bookdata';

/*********Program 4.01**************************************
/*SAS Codes to show different parametric method of estimation*/
%datapull(industr,industr.sas7bdat);
proc model data=industr;
finance = constant +beta*mrp;
fit finance / ols; /*OLS*/
fit finance / fiml; /*MLE*/
fit finance / gmm;/*GMM*/
run;


/*********Program 4.02**************************************
/*Estimating Kernel Density Using PROC UNIVARIATE*/
title 'Distribution of Financial Services Industry Returns';
proc univariate data=industr noprint;
	histogram  finan/ 
		kernel(c = 0.5 1 2 
		noprint  
		k=normal)
		odstitle = title;
run;



/*********Program 4.03**************************************
/*Estimating Univariate Bivariate Kernel Densities Using PROC KDE*/
title 'Distribution of Financial Services Industry and Market Returns';
ods graphics on;
proc KDE data=industr;
	univar finance(bwm=1) mrp(bwm=1) /plots=densityoverlay;
	bivar finance mrp/ plots=all;
run;


/*********Program 4.04**************************************
/*Using PROC LOESS to Fit the Index Model*/
proc loess data=industr plots=all;
	model finance=mrp/ clm details  degree=1 
		dfmethod=exact smooth=0.1;
	score data=finscore /clm print(var=finance);
run;



/*********Program 4.05**************************************/
/*Estimating Index Model Using Entropy Procedure*/
proc entropy data= industr maxiter=50 outp=entprop plots=all;
	priors mrp  0.5(0.5) 1.5(0.5);
	model finan = mrp /esupports= (-1.0(0.5) 1.0(0.5));
run;

proc print data=entprop;
run;



/*********Program 4.06**************************************/
proc product_status;
run;



/*********Program 4.07**************************************/
/* Using PROC PROBIT to Model the Determinants of Long-term Stock Return*/
%datapull(profit,spx_profit.sas7bdat);
ods graphics on;

proc probit data=spx_profit plot=all;
	class ceoduality sector;
	model pret5(event='0') =  ceo_tenure logmcap logemp 
		Instshrout pe esg wacc age ceoduality sector;
	effectplot contour( x=Instshrout y=logemp);
run;



/*********Program 4.08**************************************/
/* Using PROC LOGISTIC to Model the Determinants of Long-term Stock Return*/
ods graphics on;

proc logistic  data=spx_profit plot=all;
	class ceoduality sector;
	model pret5(event='0') = CEO_Tenure logmcap logemp 
		Instshrout pe esg wacc age ceoduality sector;
	effectplot contour(x=Instshrout y=logemp);
run;



/*********Program 4.09A**************************************/
/*Determinant of Long-Term Stock Returns*/
%datapull(profit,spx_profit.sas7bdat);

proc stepdisc data=spx_profit include=0 short;
	class pret5;
	var ceo_tenure logmcap logemp 
		Instshrout pe esg wacc age;
run;


/*********Program 4.09B**************************************/
/*Determinant of Long-Term Stock Returns*/
proc discrim data=spx_profit method=normal crossvalidate 
outstat=distat outd=discore posterr distance anova manova pool=test;
	class pret5;
	var logmcap logemp Instshrout wacc;
	priors equal;
run;


/*********Program 4.09C**************************************/
/*Determinants of Long-Term Stock Returns*/
proc discrim data=spx_profit method=npar kernel=normal r=.1 crossvalidate 
	outstat=distat outd=discore;
	class pret5;
	var logmcap logemp Instshrout wacc;
	priors equal;
run;



/*********Program 4.10**************************************/
/*Estimating Statistical Factor Model of Bank Stock Return*/
%datapull(banks,banks.sas7bdat);
ods graphics on;

proc factor data= banks method=ml 
	score nfact=4 priors=smc heywood 
	rotate=quartimin plots=all
	outstat = factstats out=work.factfactors;
	var rbac rwfc rpnc rjpm rusb rc rtfc rhban rmtb rrf rzion rfitb rcfg rkey rcma;
run;




/*********Program 4.11**************************************/
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



/*********Program 4.12A**************************************/
/*Estimating Survival Function Credit Obligors Using PROC LIFETEST*/
%datapull(loans,lcloans.sas7bdat);
ods graphics on;

proc lifetest data=lcloans plots=(s(atrisk),ls,lls,h(kernel=epanechnikov) cif)
	method=pl nelson  intervals=0 to 50 by 1  outsurv=sdata;
	time timetd*status(0)/;
run;


/*********Program 4.12B**************************************/
/*Comparing Survival Function Credit Obligors Using PROC LIFETEST*/
ods graphics on;

proc lifetest data=lcloans method=pl  plots=(s h ls lls) notable outtest=sest intervals=0 to 50 by 1  outsurv=sdata;;
	time timetd*status(0);
	strata term/order=internal test=(lr logrank);
	test dti int_rate;
run;



/*********Program 4.13**************************************/
/*Estimating Proportional Hazard Models of Credit Obligors Using PROC PHREG*/
ods graphics on;

proc phreg data=lcloans plots=(survival cumhaz roc auc) 
rocoptions(at=0 to 40 by 10) simple;
class application_type grade term purpose home_ownership verification_status emp_length;
model timetd*status(0)= loan_amnt int_rate annual_inc dti pub_rec application_type total_acc revol_util grade term purpose home_ownership
verification_status emp_length / selection=stepwise slentry=0.25 slstay=0.15 details;
run;



/*********Program 4.14**************************************/
/*Using PROC Syslin to Model the Determinant of Stock return and Volume*/
%datapull (market,market_data.sas7bdat);
ods graphics on;

proc syslin data=market_data 2sls;
	endogenous dret vret;
	instruments rbond roil rexer rvix rgvix rovix ldret lvret;
return:
	model dret= vret lvret rbond roil rexr;
volume:
	model vret= dret ldret rvix rgvix rovix;
run;



/*********Program 4.15**************************************/
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




/*********Program 4.16**************************************/
/*Forecasting Monthly Trading Volume on the S&P 500 using Exponsential Smoothing Model*/
%datapull(spx,rspx_monthly7bdat);
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



/*********Program 4.17**************************************/
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



/*********Program 4.18**************************************/
/*Importing dataset into your SAS Environment*/
%datapull(spxraw,spxraw.sas7bdat);
%datapull(spxscore,spxscore.sas7bdat);
data emsas.spxraw ;set work.spxraw ;run;
data emsas.spxscore ;set work.spxscore;run;



/*********Program 4.19A**************************************/
/*Classifying Stock Market Direction Using High Performance Logistic Regression */
/*Use Macro Variable to create list of predictor variable*/
%let var_list=CCSIBBB	CFDTR	CFXRATE	CHIST_CALL_IMP_VOL	CHIST_PUT_IMP_VOL CINJCJC	CLEI	CMF_NET_BLCK CMF_NET_NON_BLCK	CMOV_AVG_10D CMOV_AVG_30D	CMOV_AVG_5D COPEN_INT_TOTAL_CALL	COPEN_INT_TOTAL_PUT CPE_RATIO CPX_LAST	CPX_OPEN;	

ods graphics on;
proc hplogistic data=EMSAS.SPXRAWP;
	id Dates Partition;
	class Target;
	model Target=&var_list. /cutpoint=0.5 link=logit;
	partition rolevar=partition(train='1' validate='2');
	selection method=backward(slstay=0.1)  details=all;
	code file ="%sysfunc(pathname(work))/logscore.sas" group=HPLOG;
	output out=plogout /allstats;
	ods output PartFitStats=logstats;
run;
	 

/*********Program 4.19A**************************************/
/*Comparing Prediction to Actual Outcomes*/
proc sort data=plogout; by dates; run;
proc sort data=emsas.spxrawp; by dates;run;
data logout;
	merge plogout emsas.spxrawp;
	by dates;
/*************************************************
The codes below repeated in other Models.
**************************************************/

	if (Pred < 0.5) then
		P_Target = 0;
	else P_Target = 1;
   format Classification $10. Role $9. Model $15.;
    if (Target = P_Target) then Classification = 'Correct';
 	else Classification = 'Incorrect';
	if Partition = '1' then
		Role ='Train';
	else Role ='Validate';
  if P_Target = 1 and Target=1 then TP=1; else TP='';
  if P_Target = 1 and Target=0 then FP=1; else FP='';
  if P_Target = 0 and Target=1 then FN=1; else FN='';
  if P_Target = 0 and Target=0 then TN=1; else TN='';
  label TP='True Positive' FP='False Positive' FN='False Negative' TN='True Negative';

/*************************************************
End of repeated code
**************************************************/

  Model='HP Logistics';
run;



/*********Program 4.20**************************************/
/*Classifying Stock Market Direction Using High Performance GLM */
proc hpgenselect data=EMSAS.SPXRAWP;
	class Target;
	id dates partition;
	partition role=Partition (validate='2');
	model Target (event='1')= &var_list./dist=binary link=probit;
/* Macrovariable list*/
		selection method=backward(slstay=0.1) details=all;
		output out=pglmout role=partrole / allstats;
	  code file="%sysfunc(pathname(work))/glmscore.sas" group=HPGLM ;
	  ods output fitstatistics=glmstats(where=(Step is missing));
run;

/*Comparing Prediction to Actual Outcomes*/
proc sort data=pglmout; by dates; run;
proc sort data=emsas.spxrawp; by dates;run;
data glmout;
merge pglmout emsas.spxraw;

/*********************************
Same code as Program 4.19B
**********************************/

	if (Pred < 0.5) then
		P_Target = 0;
	else P_Target = 1;
   format Classification $10. Role $9. Model $15.;
    if (Target = P_Target) then Classification = 'Correct';
 	else Classification = 'Incorrect';
	if Partition = '1' then
		Role ='Train';
	else Role ='Validate';
  if P_Target = 1 and Target=1 then TP=1; else TP='';
  if P_Target = 1 and Target=0 then FP=1; else FP='';
  if P_Target = 0 and Target=1 then FN=1; else FN='';
  if P_Target = 0 and Target=0 then TN=1; else TN='';
  label TP='True Positive' FP='False Positive' FN='False Negative' TN='True Negative';

  Model='HP GLM';
run;



/*********Program 4.21**************************************/
/*Classifying Stock Market Direction Using HP Neural Network*/
/*Creating numeric partition variable*/
data emsas.spxrawn;
set emsas.spxrawp;
npartition = input(partition,2.);
run;

ods graphics on;
proc hpneural data=emsas.spxrawn;
	architecture mlp;
	input &var_list.;/* Macrovariable  list*/
	id dates partition;
	target target/level=nom;
	hidden 3/act=tanh;
	train numtries=3 outmodel=model_spxwrap  maxiter=1000;
	weight  _inverse_priors_;
	partition rolevar=npartition(validate=2);
	code file ="%sysfunc(pathname(work))/neuralscore.sas";
	score out=pneuralout;
	ods output fitstatistics=neuralstats;
run;

/*Comparing Prediction to Actual Outcomes*/
proc sort data=pneuralout;by dates;
proc sort data=emsas.spxrawn;by dates;
run; 
data neuralout;
merge pneuralout emsas.spxrawn;
by dates;
/*********************************
Same code as Program 4.19B
**********************************/

	if (Pred < 0.5) then
		P_Target = 0;
	else P_Target = 1;
   format Classification $10. Role $9. Model $15.;
    if (Target = P_Target) then Classification = 'Correct';
 	else Classification = 'Incorrect';
	if Partition = '1' then
		Role ='Train';
	else Role ='Validate';
  if P_Target = 1 and Target=1 then TP=1; else TP='';
  if P_Target = 1 and Target=0 then FP=1; else FP='';
  if P_Target = 0 and Target=1 then FN=1; else FN='';
  if P_Target = 0 and Target=0 then TN=1; else TN='';
  label TP='True Positive' FP='False Positive' FN='False Negative' TN='True Negative';

Model='HP Neural';
run;



/*********Program 4.22**************************************/
/*Classifying Stock Market Direction Using HP Decision Tree*/
ods graphics on;
proc hpsplit data=emsas.spxrawp  maxbranch=2 splitonce
	intervalbins=100 maxdepth=10 mincatsize=1 mindist=0.01 alpha=0.2 leafsize=1 nsurrogates=0
	assignmissing=popular;
	id dates partition;
	class target;
	model target = &var_list.;
	grow entropy;
/*Prune based on misclassification and select subtree with lowest misclassification*/
	prune  misc / min;
	partition rolevar=partition(train='1' validate='2');
	code file ="%sysfunc(pathname(work))/splitscore.sas";
	rules file="%sysfunc(pathname(work))/rules.txt";
	output out=ptreeout;
	ods output treePerformance=treestats;
run;

/*Comparing Predictions with Actual Outcome*/
proc sort data=ptreeout; by dates; run;

data treeout;
merge ptreeout emsas.spxraw;
by dates;

/**********************************
Same code as Program 4.19B
**********************************/  

	if (Pred < 0.5) then
		P_Target = 0;
	else P_Target = 1;
   format Classification $10. Role $9. Model $15.;
    if (Target = P_Target) then Classification = 'Correct';
 	else Classification = 'Incorrect';
	if Partition = '1' then
		Role ='Train';
	else Role ='Validate';
  if P_Target = 1 and Target=1 then TP=1; else TP='';
  if P_Target = 1 and Target=0 then FP=1; else FP='';
  if P_Target = 0 and Target=1 then FN=1; else FN='';
  if P_Target = 0 and Target=0 then TN=1; else TN='';
  label TP='True Positive' FP='False Positive' FN='False Negative' TN='True Negative';

  Model='HP Tree';
run;



/*********Program 4.23**************************************/

/*Classifying Stock Market Direction Using HP SVM*/
ods graphics on;
proc hpsvm data=emsas.spxrawp method=ipoint;
	id dates partition;
	input &var_list. /level=interval;
	kernel polynom/ degree=3;
	target target/;
	penalty C=20.0;
	partition  rolevar=partition(validate='2');
	code file ="%sysfunc(pathname(work))/svmscore.sas";
	output out=psvmout;
	ods output fitstatistics=svmstats;
run;

/*Comparing Prediction to Actual Outcomes*/
proc sort data=psvmout; by dates; run;
data svmout;
merge psvmout emsas.spxraw;
by dates;
/*****************************
Same code as previous algorithms
**********************************/ 

	if (Pred < 0.5) then
		P_Target = 0;
	else P_Target = 1;
   format Classification $10. Role $9. Model $15.;
    if (Target = P_Target) then Classification = 'Correct';
 	else Classification = 'Incorrect';
	if Partition = '1' then
		Role ='Train';
	else Role ='Validate';
  if P_Target = 1 and Target=1 then TP=1; else TP='';
  if P_Target = 1 and Target=0 then FP=1; else FP='';
  if P_Target = 0 and Target=1 then FN=1; else FN='';
  if P_Target = 0 and Target=0 then TN=1; else TN='';
  label TP='True Positive' FP='False Positive' FN='False Negative' TN='True Negative';
 
Model='HP SVM';
run;



/*********Program 4.24**************************************/

/*Classifying Stock Market Direction Using HP Random Forest*/
ods graphics on;

proc hpforest data=emsas.spxrawp 
	maxtrees=100 vars_to_try=10 seed=12345 inbagfraction=0.3
	maxdepth=10 leafsize=1 alpha=0.05 scoreprole=oob;
	id dates partition;
	target target/level=binary;
	input &var_list. /level=interval; /* Macrovariable list*/
	partition rolevar=partition(train='1' validate='2');
	save file ="%sysfunc(pathname(work))/forestscore.sas";
	score out=pforestout;
	ods output fitstatistics=pforeststats modelinfo=forestinfo;
run;

/*SAS Code to the obtain the fit statistics for the selected number of trees*/
data _null;
	set forestinfo(where=(parameter='Actual Trees'));
	call symput("treenum",setting);
run;

data foreststats;
	set pforeststats(where=(Ntrees=&treenum));
run;

/*Comparing Prediction to Actual Outcomes*/
proc sort data=pforestout;	by dates;run;
proc sort data=emsas.spxrawp; by dates;run;

data forestout;
	merge pforestout emsas.spxrawp;
	by dates;

/*****************************
Same code as previous algorithms
**********************************/

	if (Pred < 0.5) then
		P_Target = 0;
	else P_Target = 1;
   format Classification $10. Role $9. Model $15.;
    if (Target = P_Target) then Classification = 'Correct';
 	else Classification = 'Incorrect';
	if Partition = '1' then
		Role ='Train';
	else Role ='Validate';
  if P_Target = 1 and Target=1 then TP=1; else TP='';
  if P_Target = 1 and Target=0 then FP=1; else FP='';
  if P_Target = 0 and Target=1 then FN=1; else FN='';
  if P_Target = 0 and Target=0 then TN=1; else TN='';
  label TP='True Positive' FP='False Positive' FN='False Negative' TN='True Negative';
 
	Model='HP Forest';
run;

	




/*********Program 4.25**************************************/

/*Classifying Stock Market Direction Using Ensemble Model*/
%datapull(Ensemble,Ensemble.sas);/*Pull Ensemble Codes from GitHub*/
data PEnsemble;
set emsas.spxrawp;
%include "%sysfunc(pathname(work))/Ensemble.sas";
/*Ensemble Code includes scoring from previous models 
HPLOGISTICS, HPGENSELECT, HPNEURAL, HPTREE, HPSVM*/
/***Average of Posterior probabilities from each model is calculated**
 **Classification is then performed using the 50% threshold***/
run;

/*Comparing Prediction to Actual Outcomes*/
Data Ensembleout;
set PEnsemble;
/**********************************
Same code as Program 4.19B
**********************************/ 
   Model='Ensmbl';
   
	if (Pred < 0.5) then
		P_Target = 0;
	else P_Target = 1;
   format Classification $10. Role $9. Model $15.;
    if (Target = P_Target) then Classification = 'Correct';
 	else Classification = 'Incorrect';
	if Partition = '1' then
		Role ='Train';
	else Role ='Validate';
  if P_Target = 1 and Target=1 then TP=1; else TP='';
  if P_Target = 1 and Target=0 then FP=1; else FP='';
  if P_Target = 0 and Target=1 then FN=1; else FN='';
  if P_Target = 0 and Target=0 then TN=1; else TN='';
  label TP='True Positive' FP='False Positive' FN='False Negative' TN='True Negative';

run;



/*********Program 4.26**************************************/
/*Print Summary of Estimation Fit Statistics*/
proc print data=logstats;
	title 'Logistic Regression Statistics';
proc print data=glmstats;
	title 'GLM Regression Statistics';
proc sgrender data=neuralstats template=HPDM.HPNEURAL.FitStatistics;
	title 'Neural Network Statistics';
proc print data=treestats;
	title 'Decision Tree Statistics';
proc sgrender data=svmstats template=HPDM.HPSvm.FitStatistics;
	title 'Support Vector Machine Statistics';
proc print data=foreststats;
	title 'Random Forest Statistics';
run;
title;



/*********Program 4.27**************************************/
/*Merging Predictions and Classifications for Further Analysis*/
data Modelcomp1;
set logout(keep=dates model target role p_target classification TP FP FN TN)
glmout(keep=dates model target role p_target classification TP FP FN TN)
neuralout(keep=dates model target role p_target classification TP FP FN TN) 
treeout(keep=dates model target role p_target classification TP FP FN TN)
svmout(keep=dates model target role p_target classification TP FP FN TN)
forestout(keep=dates model target role p_target classification TP FP FN TN)
Ensembleout(keep=dates model target role p_target classification TP FP FN TN);
run;

/*Using Proc Tabulate to Compute the Classfication Accuracy*/
proc tabulate data=Modelcomp1;
	class model classification role;
	table  classification*(pctn<classification>=''),Model='Comparing Model Classification Accuracy'*role ;
run;



/*********Program 4.28**************************************/

/*Plotting Classification Table for All Model*/
proc sort data=Modelcomp1 out=Plotdata;
	by Model Role;
run;

/*Using PROC SGPANEL to Produce Cluster Performance Graphs*/
ods graphics / reset width=6.4in height=4.8in imagemap;
title 'Classification Chart for Machine Learning Algorithms';

proc sgpanel data=Plotdata pctlevel=cell ;
	panelby role model/ onepanel rows=2 noheaderborder novarname 
		colheaderpos=bottom spacing=2;
vbar Target / group=Classification groupdisplay=stack stat=percent transparency=0.10 ;
	colaxis label='Target';
	rowaxis label='Prediction Percent';
run;
title;


/*********Program 4.29**************************************/
/*Using Proc Tabulate to Compute the Classfication Matrix*/
proc tabulate data=Modelcomp1;
	class model target p_target role;
	table  role*Target='Target', 
	Model='Comparing Model Classification Matrix'*(pctn<P_Target>='Predicted')*P_Target='' ;
run;


/*********Program 4.30**************************************/
/*Using Proc SQL to Calculate Classification Measures*/
proc sql;
	create table modelcomp2 as select Role as Role, Model as  Model, 
	(sum(TP)+sum(TN))/(sum(TP)+sum(TN)+sum(FP)+sum(TN)) as Accuracy , 
	1-(sum(TP)+sum(TN))/(sum(TP)+sum(TN)+sum(FP)+sum(TN)) 
label="Misclassfication Rate" as Misclass , sum(TP)/(sum(TP)+sum(FP)) as 
	Precision, sum(TP)/(sum(TP)+sum(FN)) as Sensitivity, 
sum(TN)/(sum(TN)+sum(FP)) as Specificity, 1-sum(TN)/(sum(TN)+sum(FP)) 
label="1-Specificity" as MSpecificity from plotdata group by Role, Model.
quit;

title 'Summary of Fit Statistics from Estimated Models';
proc print data=modelcomp2 label style(header)=[width=0.8 in textalign=center]noobs;
	var Role model;
var Accuracy Misclass Precision Sensitivity Specificity Mspecificity/style(data)=[textalign=center];
format Accuracy best4.2 Misclass best4.2 Precision best4.2 Sensitivity best4.2 Specificity best4.2 Mspecificity best4.2;
run;
title;



/*********Program 4.31A**************************************/
/*Merging Data for ROC Curves*/
data Modelcomp3;
	merge 
		logout(rename=(Pred=Log_Pred P_Target=Log_Target Classification=Log_class))
		glmout(rename=(Pred=Glm_Pred P_Target=Glm_Target Classification=Glm_class)) treeout(rename=(P_Target1=Tree_Pred P_Target=Tree_Target Classification=Tree_class))	
		svmout(rename=(P_Target1=SVM_Pred P_Target=SVM_Target Classification=SVM_class)) 
		forestout(rename=(P_Target1=Forest_Pred P_Target=Forest_Target Classification=Forest_class)) 
		neuralout(rename=(P_Target1=Neural_Pred P_Target=Neural_Target Classification=Neural_class))
		Ensembleout(rename=(P_Target1=Ens_Pred P_Target=Ens_Target Classification=Ens_class));
	by dates;
run;



/*********Program 4.31B**************************************/
/*Using PROC LOGISTICS to Compare ROC Curves*/
%let _ROCOVERLAY_ENTRYTITLE = Comparing ROC Curves (Data Role=Validate);

proc logistic data=modelcomp3 (where=(role='Validate'));
	model target(event='1')=Log_Pred GLM_Pred Neural_Pred 
		Tree_Pred SVM_Pred 	Forest_Pred Ens_Pred / nofit;
	roc 'HPReg' pred=Log_Pred;
	roc 'HPGLM' pred=GLM_Pred;
	roc 'HPNNA' pred=Neural_Pred;
	roc 'HPTree' pred=Tree_Pred;
	roc 'HPSVM' pred=SVM_Pred;
	roc 'HPForest' pred=Forest_Pred;
	roc 'Ensmbl' pred=Ens_Pred;
	ods select ROCOverlay;
run;
%symdel _ROC_ENTRYTITLE;


/*********Program 4.32**************************************/
/*Backtesting Champion Model Using Scoring Code*/
/*Pull Scoring Code for Ensemble Model from GitHub*/
%datapull(Ensemble,Ensemble.sas);

/*Scoring Holdout Data Using DATA Step*/
data pscorespx;
	set emsas.spxscore;
	%include "%sysfunc(pathname(work))/Ensemble.sas";
run;

data scorespx;
	set pscorespx;
	format Classification $10. CMatrix $8.;
	if (I_Target=Index_out) then do;
		Classification='Correct';
		Algo_ret=abs(OTO_FRet);/*Open-to-Open Futures Return*/
	end;
	else do;
		Classification='Incorrect';
		Algo_Ret=-1*abs(OTO_FRet);
	end;
	EXAlgo_Ret=Algo_Ret-RF; /*Excess Algorithmic Return*/
	EXIndex_Ret=Index_Ret-RF;/*Excess Index Return*/
	if I_Target=1 and  Index_out=1 then	CMatrix='TP';
	if I_Target=1 and  Index_out=0 then CMatrix='FP';
	if I_Target=0 and  Index_out=1 then	CMatrix='FN';
	if I_Target=0 and  Index_out=0 then	CMatrix='TN';
label CMatrix='Prediction Condition' 
Algo_Ret='Algorithmic Returns';

	/*Cumulative Returns*/
	Sumiret+Index_Ret; /*Passive Index Buy and Hold*/
	Sumaret+algo_Ret;  /*Algorithmic Based*/
	IndexVal =1000000*exp(sumiret); /* Index Portfolio Value*/
	AlgoVal = 1000000*exp(sumaret);/* Algorithmic Portfolio Value*/
	format IndexVal dollar16.2 AlgoVal dollar16.2;
	label IndexVal= 'Index Portfolio Values' AlgoVal ='Algorithmic Portfolio Values';
run;



/*********Program 4.33**************************************/
/*Graphing Classification Accuracy Using PROC SGPLOT*/
ods graphics / reset width=6.4in height=4.8in imagemap;

proc sgplot data=Scorespx;
	title height=12pt "Prediction Accuracy (Backtesting Period)";
vbar Index_Out / group=Classification groupdisplay=stack stat=percent dataskin=crisp;
	yaxis grid label="Prediction Percent";
run;

ods graphics / reset;
title;



/*********Program 4.34**************************************/
/*Comparing Portfolio Performance Using PROC REPORT*/
proc report data=scorespx nowd ;
columns ('Backtesting the Performance of Algorithmic Portfolio Relative to Passive Index Strategy'(CMatrix
(('Algorithmic Portfolio'(Algo_Ret=Asum Algo_Ret=Amean Algo_Ret=Astd AAmean AAstd ExAlgo_Ret=EXAmean ExAlgo_Ret=EXAstd  ASharpe))
('Index Portfolio' (Index_Ret=Isum Index_Ret=Imean Index_Ret=Istd AImean AIstd ExIndex_Ret=EXImean ExIndex_Ret=EXIstd ISharpe))) ));
	
	define Cmatrix/group style(header)=[textalign=left]   left;
/*****************Algorithmic Portfolio Calculations****************/
 	define Asum/analysis sum format=percent8.2 'Total Return' center;
define Amean/analysis mean format=percent8.2 'Mean Daily Return' center noprint;
define Astd/analysis std format=percent8.2 'Standard Deviation' center noprint;
define AAmean/computed format=percent8.2 'Annualized Mean' center;
 	compute AAmean;
 	AAmean =amean*252;
 	endcomp;
define AAstd/computed format=percent8.2 'Annualized Standard Deviation' center;
 	compute AAstd;
 	AAStd =Astd*sqrt(252);
 	endcomp;
 	
 	/*Calculating Mean Excess Return and Standard of Excess Return*/
	define ExAmean/analysis mean format=percent8.2  noprint ;
	define EXAstd/analysis std format=percent8.2 noprint ;

 	define ASharpe/computed format=best6.2 'Sharpe Ratio' center;
 	compute ASharpe;
 	 if Amean>0 then ASharpe=EXAmean/ExAstd;
 	endcomp;

	
/*****************Passive Portfolio Calculations******************/
 	define Isum/analysis sum format=percent8.2 'Total Return' center;
define Imean/analysis mean format=percent8.2 'Mean Daily Return' center noprint;
define Istd/analysis std format=percent8.2 'Standard Deviation' center noprint;
define AImean/computed format=percent8.2 'Annualized Mean' center;
 	compute AImean;
 	AImean =Imean*252;
 	endcomp;
define AIstd/computed format=percent8.2 'Annualized Standard Deviation' center;
 	compute AIstd;
 	AIstd =Istd*sqrt(252);
 	endcomp;
 	
 	 /*Calculating Mean Excess Return and Standard of Excess Return*/
 	define EXImean/analysis mean  format=percent8.2  noprint;
	define ExIstd/analysis std  format=percent8.2 noprint;

 	define ISharpe/computed format=best6.2  'Sharpe Ratio' center;
 	compute ISharpe;
 	if CMatrix='Total' then ISharpe=EXImean/EXIstd;
 	endcomp;
 	rbreak after / summarize style=[font_weight=bold  ] ;
 	 compute Cmatrix ; 
 	 if CMatrix='' then
		CMatrix='Total'  ;
	endcomp;
run;




/*********Program 4.35**************************************/
/* Using SGPLOT to Graph the Relationship Between Posterior Probabilities and Actual Index Return*/
Proc sgplot data=scorespx;
	title height=12pt "Predicted Probabilities of Positive Market Movement";
scatter x = index_ret y=P_Target1/group=I_Target markeroutlineattrs=( thickness=1) markerattrs=(symbol=circlefilled size=10)  ;
reg x = index_ret y=P_Target1/  clm ;
yaxis label='Predicted Probabilities';
xaxis valuesformat=percent8.2 values=(-0.04 to 0.03 by 0.01);
run;



/*********Program 4.36**************************************/
/*Comparing Portfolio Values Using PROC SGPLOT*/
proc sgplot data=scorespx;
	title height=14pt "Daily Portfolio Values";
	series x=Dates y=IndexVal/;
	series x=  Dates y=AlgoVal;
	yaxis label='Daily Portfolio Values';
run;
title;


