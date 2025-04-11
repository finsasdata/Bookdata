/*********Program 7.9B**************************************/
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

/*
data Modelcomp1;
set
treeout(keep=dates model target role p_target classification)
svmout(keep=dates model target role p_target classification)
logout(keep=dates model target role p_target classification)
neuralout(keep=dates model target role p_target classification)
treeout(keep=dates model target role p_target classification)
glmout(keep=dates model target role p_target classification)
forestout(keep=dates model target role p_target classification)
Ensembleout(keep=dates model target role p_target classification);
run;
*/
/*********Program 7.10**************************************/
/*Plotting Classification Table for All Models*/
proc tabulate data=Modelcomp1;
	class model classification role;
	table classification*(pctn<classification>=''), 
		Model='Comparing Model Classification Accuracy'*role;
run;

/*Plotting Classification Table for All Model*/
proc sort data=Modelcomp1 out=Plotdata;
	by Model Role;
run;

/*Using PROC SGPANEL to Produce Cluster Performance Graphs*/
ods graphics / reset width=6.4in height=4.8in imagemap;
title 'Classification Table for Machine Learning Algorithms';

proc sgpanel data=Plotdata pctlevel=cell;
	panelby role model/ onepanel rows=2 noheaderborder novarname 
		colheaderpos=bottom spacing=2;
	vbar Target / group=Classification groupdisplay=stack stat=percent 
		transparency=0.10;
	colaxis label='Target';
	rowaxis label='Prediction Percent';
run;

title;

/*********Program 7.11**************************************/
/*Using Proc Tabulate to Compute the Classification Matrix*/
proc tabulate data=Modelcomp1;
	class model target p_target role;
	table role*Target='Target', Model='Comparing Model Classification Matrix'*(pctn<P_Target>='Predicted')*P_Target='';
run;

/*********Program 7.12**************************************/
/*Using Proc SQL to Calculate Classification Measures*/
proc sql;
	create table modelcomp2 as select Role as Role, Model as Model, 
		(sum(TP)+sum(TN))/(sum(TP)+sum(TN)+sum(FP)+sum(TN)) as Accuracy , 
		1-(sum(TP)+sum(TN))/(sum(TP)+sum(TN)+sum(FP)+sum(TN)) 
		label="Misclassfication Rate" as Misclass , sum(TP)/(sum(TP)+sum(FP)) as 
		Precision, sum(TP)/(sum(TP)+sum(FN)) as Sensitivity, 
		sum(TN)/(sum(TN)+sum(FP)) as Specificity, 1-sum(TN)/(sum(TN)+sum(FP)) 
		label="1-Specificity" as MSpecificity from plotdata group by Role, Model;
quit;

title 'Summary of Fit Statistics from Estimated Models';

proc print data=modelcomp2 label style(header)=[width=0.8 in textalign=center] 
		noobs;
	var Role model;
	var Accuracy Misclass Precision Sensitivity Specificity Mspecificity/ 
		style(data)=[textalign=center];
	format Accuracy best4.2 Misclass best4.2 Precision best4.2 Sensitivity best4.2 
		Specificity best4.2 Mspecificity best4.2;
run;

title;

/*********Program 7.13A**************************************/
/*Merging Data for ROC Curves*/
data Modelcomp3;
	merge logout(rename=(Pred=Log_Pred P_Target=Log_Target 
		Classification=Log_class))glmout(rename=(Pred=Glm_Pred P_Target=Glm_Target 
		Classification=Glm_class)) treeout(rename=(P_Target1=Tree_Pred 
		P_Target=Tree_Target Classification=Tree_class)) 
		svmout(rename=(P_Target1=SVM_Pred P_Target=SVM_Target 
		Classification=SVM_class)) forestout(rename=(P_Target1=Forest_Pred 
		P_Target=Forest_Target Classification=Forest_class)) 
		neuralout(rename=(P_Target1=Neural_Pred P_Target=Neural_Target 
		Classification=Neural_class)) Ensembleout(rename=(P_Target1=Ens_Pred 
		P_Target=Ens_Target Classification=Ens_class));
	by dates;
run;

/*********Program 7.131B**************************************/
/*Using PROC LOGISTICS to Compare ROC Curves*/
%let _ROCOVERLAY_ENTRYTITLE = Comparing ROC Curves (Data Role=Train);

proc logistic data=modelcomp3 (where=(role='Train'));
	model target(event='1')=Log_Pred GLM_Pred Neural_Pred Tree_Pred SVM_Pred 
		Forest_Pred Ens_Pred / nofit;
	roc 'HPReg' pred=Log_Pred;
	roc 'HPGLM' pred=GLM_Pred;
	roc 'HPNNA' pred=Neural_Pred;
	roc 'HPTree' pred=Tree_Pred;
	roc 'HPSVM' pred=SVM_Pred;
	roc 'HPForest' pred=Forest_Pred;
	roc 'Ensmbl' pred=Ens_Pred;
	ods select ROCOverlay;

	/* roccontrast reference ('HPReg') /Estimate E;*/
run;

%let _ROCOVERLAY_ENTRYTITLE = Comparing ROC Curves (Data Role=Validate);

proc logistic data=modelcomp3 (where=(role='Validate'));
	model target(event='1')=Log_Pred GLM_Pred Neural_Pred Tree_Pred SVM_Pred 
		Forest_Pred Ens_Pred / nofit;
	roc 'HPReg' pred=Log_Pred;
	roc 'HPGLM' pred=GLM_Pred;
	roc 'HPNNA' pred=Neural_Pred;
	roc 'HPTree' pred=Tree_Pred;
	roc 'HPSVM' pred=SVM_Pred;
	roc 'HPForest' pred=Forest_Pred;
	roc 'Ensmbl' pred=Ens_Pred;
	ods select ROCOverlay;

	/* roccontrast reference ('HPReg') /Estimate E;*/
run;

%symdel _ROC_ENTRYTITLE;

/*********Program 7.14A**************************************/
/* Label: Score */
/*Backtesting Champion Model Using Scoring Code*/
/*Pull Scoring Code for Ensemble Model from GitHub*/
%datapull(Ensemble, Ensemble.sas);

/*Scoring Holdout Data Using DATA Step*/
data pscorespx;
	set emsas.spxscore;
	%include "%sysfunc(pathname(work))/Ensemble.sas";
run;

data scorespx;
	set pscorespx;
	format Classification $10. CMatrix $8.;

	if (I_Target=Index_out) then
		do;
			Classification='Correct';
			Algo_Ret=abs(OTO_FRet);

			/*Open-to-Open Futures Return*/
		end;
	else
		do;
			Classification='Incorrect';
			Algo_Ret=-1*abs(OTO_FRet);
		end;
	EXAlgo_Ret=Algo_Ret-RF;

	/*Excess Algorithmic Return*/
	EXIndex_Ret=Index_Ret-RF;

	/*Excess Index Return*/
	if I_Target=1 and Index_Out=1 then
		CMatrix='TP';

	if I_Target=1 and Index_Out=0 then
		CMatrix='FP';

	if I_Target=0 and Index_Out=1 then
		CMatrix='FN';

	if I_Target=0 and Index_Out=0 then
		CMatrix='TN';
	label CMatrix='Prediction Condition' Algo_Ret='Algorithmic Returns';

	/*Cumulative Returns*/
	Sumiret+Index_Ret;

	/*Passive Index Buy and Hold*/
	Sumaret+Algo_Ret;

	/*Algorithmic Based*/
	IndexVal=1000000*exp(sumiret);

	/* Index Portfolio Value*/
	AlgoVal=1000000*exp(sumaret);

	/* Algorithmic Portfolio Value*/
	format IndexVal dollar16.2 AlgoVal dollar16.2;
	label IndexVal='Passive Portfolio Values' 
		AlgoVal='Algorithmic Portfolio Values';
run;

/*********Program 7.14B**************************************/
/*Graphing Classification Accuracy Using PROC SGPLOT*/
ods graphics / reset width=6.4in height=4.8in imagemap;

proc sgplot data=Scorespx;
	title height=12pt "Classification Accuracy (Backtesting Period)";
	vbar Index_Out / group=Classification groupdisplay=stack stat=percent 
		dataskin=crisp;
	yaxis grid label="Prediction Percent";
run;

ods graphics / reset;
title;

/*********Program 7.15**************************************/
/*Comparing Portfolio Performance Using PROC REPORT*/
proc report data=scorespx nowd;
	columns ('Backtesting the Performance of Algorithmic Portfolio Relative to Passive Index Strategy'(CMatrix
	(('Algorithmic Portfolio'(Algo_Ret=Asum Algo_Ret=Amean Algo_Ret=Astd AAmean 
		AAstd ExAlgo_Ret=EXAmean ExAlgo_Ret=EXAstd ASharpe)) 
		('Passive Portfolio' (Index_Ret=Isum Index_Ret=Imean Index_Ret=Istd AImean 
		AIstd ExIndex_Ret=EXImean ExIndex_Ret=EXIstd ISharpe))) ));
	define Cmatrix/group style(header)=[textalign=left] left;

	/***********************Algorithmic Portfolio Calculations******************************/
	define Asum/ analysis sum format=percent8.2 'Total Return' center;
	define Amean/analysis mean format=percent8.2 'Mean Daily Return' center 
		noprint;
	define Astd/analysis std format=percent8.2 'Standard Deviation' center noprint;
	define AAmean/computed format=percent8.2 'Annualized Mean' center;
	compute AAmean;
		AAmean=amean*252;
	endcomp;
	define AAstd/computed format=percent8.2 'Annualized Standard Deviation' center;
	compute AAstd;
		AAStd=Astd*sqrt(252);
	endcomp;

	/*Calculating Mean Excess Return and Standard of Excess Return*/
	define ExAmean/analysis mean format=percent8.2 noprint;
	define EXAstd/analysis std format=percent8.2 noprint;
	define ASharpe/computed format=best6.2 'Sharpe Ratio' center;
	compute ASharpe;

		if Amean>0 then
			ASharpe=EXAmean/ExAstd;
	endcomp;

	/***********************Passive Portfolio Calculations******************************/
	define Isum/ analysis sum format=percent8.2 'Total Return' center;
	define Imean/analysis mean format=percent8.2 'Mean Daily Return' center 
		noprint;
	define Istd/analysis std format=percent8.2 'Standard Deviation' center noprint;
	define AImean/computed format=percent8.2 'Annualized Mean' center;
	compute AImean;
		AImean=Imean*252;
	endcomp;
	define AIstd/computed format=percent8.2 'Annualized Standard Deviation' center;
	compute AIstd;
		AIstd=Istd*sqrt(252);
	endcomp;

	/*Calculating Mean Excess Return and Standard of Excess Return*/
	define EXImean/analysis mean format=percent8.2 noprint;
	define ExIstd/analysis std format=percent8.2 noprint;
	define ISharpe/computed format=best6.2 'Sharpe Ratio' center;
	compute ISharpe;

		if CMatrix='Total' then
			ISharpe=EXImean/EXIstd;
	endcomp;
	rbreak after / summarize style=[font_weight=bold];
	compute Cmatrix;

		if CMatrix='' then
			CMatrix='Total';
	endcomp;
run;

/*********Program 7.16**************************************/
/* Using SGPLOT to Graph the Relationship Between Posterior Probabilities and Actual Index Return*/
proc sgplot data=scorespx;
	title height=12pt "Posterior Probabilities of Positive Market Movements";
	scatter x=index_ret y=P_Target1/group=I_Target 
		markeroutlineattrs=(thickness=1) markerattrs=(symbol=circlefilled size=10);
	reg x=index_ret y=P_Target1/ clm;
	yaxis label='Predicted Probabilities';
	xaxis valuesformat=percent8.2 values=(-0.04 to 0.03 by 0.01);
run;

/*********Program 7.17**************************************/
/*Comparing Portfolio Values Using PROC SGPLOT*/
proc sgplot data=scorespx;
	title height=14pt "Daily Portfolio Values";
	series x=Dates y=IndexVal/;
	series x=Dates y=AlgoVal;
	yaxis label='Daily Portfolio Values';
run;

title;




