/*****************************************/
/* Financial Data Science with SAS       */
/* SAS Codes for Chapter Five Examples   */
/*****************************************/

/*Run the the datapull macro (if you have not already done so) below before running the remaining programs.


/**********Datapull Macro********************/
%macro datapull(fref,pname);
	filename  &fref "%sysfunc(getoption(WORK))/&pname";

	proc http
		url="https://github.com/finsasdata/Bookdata/raw/main/&pname"
		out=&fref
		method ="get";
	run;

%mend datapull;

/**********************************************/


/**************Program 5.1****************/
/*Simulating Maximum and Minimum values of Index returns*/
%let mu=0.0067658;
%let ssd =0.04465726;

data simul13;
	call streaminit(4321);
	array xnum{1000} x1-x1000;

	do j = 1 to 1000;
		do i = 1 to 1000;
			xnum[i]=rand('normal',&mu, &ssd);
		end;
	
		xmax = max(of x1-x1000);
		xmin =min(of x1-x1000);
		output;
	end;

*	keep  xmin xmax;
run;

proc univariate data=simul13;
	var xmin xmax;
	histogram xmin/gumbel kernel;
	histogram xmax/gumbel kernel;
run;

proc sgplot data=simul13;
	inset "Simulated Maximum and Minimum Stock Returns"/title=" Distribution of 1-Month S&P 500 Index Stock Return" position=top 
		textattrs=(family="Times New Roman" color=darkblue size=12 ) valuealign=center 
		titleattrs=(family="Times New Roman" color=darkblue size=12 weight=bold) labelalign=center;
	histogram xmin/ legendlabel='Minimum Values' dataskin=matte transparency=0.3;
	histogram xmax / legendlabel='Maximum Values' dataskin=matte transparency=0.3;
	xaxis label= 'Simulated Values';
	yaxis values=(0 to 25 by 5);
run;

/******************Program 5.2****************/
/*Simulating Extreme Negative Returns from Gumbel Distribution*/
%let n=1000;
%let mun    = -0.14594;  /* location parameter */
%let sigman = 0.018013; /*Shape Parameter*/
%let mean=0.0067658; /*Mean of Normal distribution*/
%let ssd =0.04465726;/*STDEV of Normal distribution*/

data simul14;
	call streaminit(4321);

	do k=1 to 1000;
		xgumb = rand('gumbel',&mun,&sigman);
		xgev= rand('extrvalue',&mun,&sigman,0);
		xnorm=rand('normal',&mean, &ssd);
		output;
	end;

	label 
		xgumb ='Simulated Minimum Gumbel Returns'
		xgev ='Simulated Minimum GEV Returns'
		xnorm ='Simulated Normal Returns';
run;

proc sgplot data=simul14;
	inset "Minimum Stock Returns From Gumbel Distribution"/title=" Simulating Extreme 1-Month S&P 500 Index Stock Returns" position=top 
		textattrs=(family="Times New Roman" color=darkblue size=12 ) valuealign=center 
		titleattrs=(family="Times New Roman" color=darkblue size=12 weight=bold) labelalign=center;
	histogram xgumb/dataskin=matte fill transparency=0.3 legendlabel='Extreme Negative Monthly Returns';
	histogram xnorm/dataskin=matte fill transparency=0.3 legendlabel='Normal Monthly Returns';
	xaxis label= 'Simulated Returns Values' values=(-0.2 to 0.2 by 0.05);
	yaxis values=(0 to 35 by 5);
run;

proc means data=simul14 mean min median max;
	var xgumb xgev xnorm;
run;

/******************Program 5.3****************/
/*Calculating Value at Risk Using the GEV Distribution*/
%let n=1000;

/*From Program 3.17 Output*/
%let mun    = 0.143661;  /* Upper location parameter */
%let sigman = 0.0129773; /*Upper Shape Parameter*/

data gevvar;
	VaR90=&mun-&sigman*log(-&n*log(1-0.1));
	VaR95=&mun-&sigman*log(-&n*log(1-0.05));
	VaR99=&mun-&sigman*log(-&n*log(1-0.01));
	label
		var90 ='90% VaR'
		var95 ='95% VaR'
		var99 ='99% VaR';
	format var90 percent8.2  var95 percent8.2  var99 percent8.2;
run;

title ' One-Month Value-at-Risk for the S&P 500 Index';

proc print data=gevvar noobs label;
run;

title;

/******************Program 5.4****************/
/*Simulating Data from Special Cases Of GPD*/
%let n = 1000;
%let sigma =0.03153; 
%let xi = 0.0000000105367;
%let xuni=-1;
%let psigma=%sysevalf(&sigma/&xi);
%let pxi=%sysevalf(1/&xi);

data GPD;
call streaminit(4321);
	do i = 1 to &n;
		/* Generalized Pareto Parameters(scale=sigma, shape=xi) */
		u = rand('uniform');	
		xuni = u;	
		xexp=rand('exponential', &sigma);
		xpd=rand('pareto',&pxi,&psigma)-&psigma;		
		xguni=&sigma/&xuni*(u**(-&xuni)-1);
		xgexp=-&sigma*log(u);
		xgpd = &sigma/&xi *(u**(-&xi)-1);
		output;
	end;

	drop i;
	label 
		xuni =' Uniformly Distributed Returns'
		xexp = 'Exponentially Distributed Returns'
		xpd= ='Pareto Distributed Returns'
		xguni= 'Uniform Generalized Pareto Distributed Returns'
		xgexp ='Exponential Generalized Pareto Distributed Returns'
		xgpd = 'Generalized Pareto Distributed Returns';
run;

ods graphics on;
proc univariate data=gpd;
	var  xuni xexp xpd xguni xgexp xgpd ;
	histogram xuni/pareto;
	histogram xexp/pareto;
	histogram xpd/pareto(sigma=&sigma alpha=&xi);
	histogram xguni/ pareto (sigma=&sigma alpha=&xuni);
	histogram xgexp/pareto(sigma=&sigma alpha=&xi);
	histogram xgpd/pareto(sigma=&sigma alpha=&xi);
	ods select Histogram ParameterEstimates;
run;

/******************Program 5.5****************/
%datapull(spx_ret,spx_ret.sas7bdat);

proc means data=spx_ret p10;
	var mret;
	output out=threshold P10= / autoname;
run;

/*Creating macro variable with data step*/
data _null_;
	set threshold;
	call symput('mu',mret_p10);
	call symput('thresh',-mret_p10);
run;

/*Create excess distributions of returns*/
data exspx;
	set spx_ret;
	where mret<&mu;
	exmret=abs(mret-&mu);/*absolute values*/
run;

/******************Program 5.6****************/

/*Estimating parameters of distribution using PROC UNIVARIATE*/
proc univariate data=exspx outtable=PET ;
var exmret;
histogram/pareto;
ods output ParameterEstimates=gpdest FitQuantiles=FQ;
ods select Histogram ParameterEstimates ;
run;

/*Estimating parameters of distribution using PROC NLMIXED*/
proc nlmixed tech=trureg  data=exspx MAXFUNC=900 MAXITER=900;
parms nxi 0.01 theta=0.010 ;
title 'Generalized pareto distribution ';
bounds 0 < theta nxi;
xx=(1/theta)*(1+(nxi*(exmret))/theta)**(-(1+(1/nxi)));
ll=log(xx);
model exmret~general(ll);
run;

/*Estimating parameters of distribution using PROC SEVERITY*/
proc severity data=exspx crit=aicc outest=sevest(where=( _type_='EST')) 
	print=all plots (histogram)= all;
	loss exmret;
	dist gpd;
	nloptions tech=quanew maxiter=200;
run;

/*Creating macro variable with data step*/
data _null_;
	set sevest;
	call symput('ssigma',theta);
	call symput('sxi',xi);
run;

/******************Program 5.7****************/
/*Calculating GPD VaR*/
data gpdvar;
	VaR90= &mu-&ssigma*log((37*0.1/187));
	VaR95= &mu-&ssigma*log((37*0.05/187));
	VaR99= &mu-&ssigma*log((37*0.01/187));
	ES90 = Var90/(1-&sxi)+(&ssigma-&sxi*&thresh)/(1-&sxi);
	ES95 = Var95/(1-&sxi)+(&ssigma-&sxi*&thresh)/(1-&sxi);
	ES99 = Var99/(1-&sxi)+(&ssigma-&sxi*&thresh)/(1-&sxi);
	label
		var90 ='90% VaR' var95 ='95% VaR' var99 ='99% VaR'
		es90 ='90% Expected Shortfall' es95 ='95% Expected Shortfall' es99 ='99% Expected Shortfall';
	format var90 percent8.2 var95 percent8.2 var99 percent8.2
		es90 percent8.2 es95 percent8.2  es99 percent8.2;
run;

title ' One-Month Value-at-Risk for the S&P 500 Index';

proc print data=gpdvar noobs label;
run;

title;

/******************Program 5.8****************/
/*Estimating and Simulating Returns from Student-T Copula*/
%datapull(finport,finport.sas7bdat);

proc copula data = finport;
	var raxp rbac rc rgs rjpm;

	/* fit T-copula to Monthly stock returns*/
	fit T /
		marginals = empirical
		method    = MLE
		/*Generate Plots (Chi - tail dependence plot)*/
	plots (chi)  = (datatype = both);

	/* simulate 10000 observations*/
	simulate /
		ndraws = 10000
		seed   = 4321
		out    = simulR
		plots(chi) = (datatype = original);
run;

/******************Program 5.9****************/
/*SAS Code to Construct Portfolio and Estimate VaR*/
data Port_ret(drop=i);
	set simulR;
	array sret{5} raxp rbac rc rgs rjpm;

	/*Calculating Equally-Weighted Portfolio Returns*/
	PortR=0;

	do i=1 to 5;
		PortR=PortR+0.2*(exp(sret[i])-1);
	end;

	/*Transforming Log Returns in Simple Returns*/
	rnaxp=exp(raxp)-1;
	rnbac=exp(rbac)-1;
	rnc=exp(rc)-1;
	rngs=exp(rgs)-1;
	rnjpm=exp(rjpm)-1;
	label rnaxp ='American Express' rnbac ='Bank of America'
		rnc = 'Citi Group' rngs ='Goldman Sachs' rnjpm ='JPMorgan Chase'
		PortR= 'Equally-Weighted Portfolio';
run;

title 'Value at Risk for Portfolio of Financial Stocks';

proc tabulate data=Port_Ret;
	var rnaxp rnbac rnc rngs rnjpm PortR;

	table (rnaxp rnbac rnc rngs rnjpm PortR), (p1 p5 p10)*{format=percent8.2};
		keylabel p1='99% VaR' p5='95% VaR' p10='90% VaR';
run;

title;


/******************Program 5.10****************/
/*Simple Bootstrapping using SURVEY SELECT*/
%datapull(spx_ret,spx_ret.sas7bdat);

proc surveyselect data=spx_ret out=bootsamp1 
	method=urs sampsize=12 reps=1000 seed=12345;
run;

ods exclude all;/*Supppress printing of listing outputs*/

Proc means data=bootsamp1 mean std;
	by replicate;
	var mret;
	ods output summary=bootstats1;
run;

ods exclude none; /*Reactivates print of listing outputs*/

proc means data=bootstats1 mean stderr;
	var mret_Mean;
run;

proc sgplot data=bootstats1;
	inset "Basic Bootstrapping (URS)"/title="Distribution of the Sample Means of Bootstrapped S&P 500 Index Monthly Returns
" position=top 
		textattrs=(family="Times New Roman" color=darkblue size=12 ) valuealign=center 
		titleattrs=(family="Times New Roman" color=darkblue size=12 weight=bold) labelalign=center;
	histogram mret_mean/ nbins=20 dataskin=matte fill transparency=0.8 fillattrs=(color=blue );
	density mret_mean/ legendlabel= "(Normal Density Plot for Sample Means)";
	yaxis values=(0 to 25 by 5);
run;


/******************Program 5.11A****************/
/*Block Bootstrapping Using SAS*/
data aspx_ret;
	set spx_ret;
	block =ceil(_n_/12);
run;

proc surveyselect data=aspx_ret out=bootsamp2
	method=urs sampsize=10 reps=1000 seed=12345;
	cluster block;
run;

proc print data=bootsamp2 (obs=13);
run;

ods exclude all;

Proc means data=bootsamp2 mean std;
	by replicate;
	var mret;
	ods output summary=bootstats2;
run;

ods exclude none;

proc means data=bootstats2 mean stderr;
	var mret_Mean;
run;
proc sgplot data=bootstats2;
	inset "Block Bootstrapping (RS)"/title="Distribution of the Sample Means of Bootstrapped S&P 500 Index Monthly Returns" position=top 
		textattrs=(family="Times New Roman" color=darkblue size=12 ) valuealign=center 
		titleattrs=(family="Times New Roman" color=darkblue size=12 weight=bold) labelalign=center;
	histogram mret_mean/ nbins=20 dataskin=matte fill transparency=0.8 fillattrs=(color=blue );
	density mret_mean/ legendlabel= "(Normal Density Plot for Sample Means)";
	yaxis values=(0 to 25 by 5);
run;



/******************Program 5.11B****************/
/*Block Bootstrapping Using IML*/
%let ss = 12; /*sample size*/
%let nrp =1000; /*Number of replications*/

proc iml;
	call randseed(12345);
	use aspx_ret;/*Same dataset as SURVEY SELECT*/
	read all var {'Date' 'Price' 'mret' 'Block'};
	close;
	n=nrow(mret);
	ss=&ss;
	m=n/ss;
	ys=shape(mret,m,ss);
	sid = sample(1:nrow(ys), &nrp);
	yb=sid`||ys[sid,];/*include sid to show sampleID*/
	ybc=ys[sid,];
	smean=(mean(ybc`))`;/*Calculate mean of each sample*/
	ssd =((std(smean)))/sqrt(&nrp);/*Calculate stdev of each sample*/
	bsmean=mean(smean); /*Mean of means calculation*/
	create bootsamp3 from yb;
	append from yb;
	close bootsamp;
	create bootstats3 from smean;
	append from smean;
	close bootstats3;
	print ("Block Bootstrapping Using PROC IML");
	colnames = "obs1":"obs12";
	print (yb[1:2,1])[label='Block ID']  (yb[1:2, 2:13])[format=percent8.2 colname=colnames]; /*Print first 13 obs*/
	print ("Descriptive Statistics");
	print (bsmean)[label='Mean'] (ssd) [label='Std Error']; /*Standard error of sample means*/
quit;

proc sgplot data=bootstats3;
	inset "Block Bootstrapping (RS)"/title="Distribution of the Sample Means of Bootstrapped S&P 500 Index Monthly Returns" position=top 
		textattrs=(family="Times New Roman" color=darkblue size=12 ) valuealign=center 
		titleattrs=(family="Times New Roman" color=darkblue size=12 weight=bold) labelalign=center;
	histogram col1/nbins=20 dataskin=matte fill transparency=0.8 fillattrs=(color=blue );
	density col1/ legendlabel= "(Normal Density Plot for Sample Means)";
	yaxis values=(0 to 25 by 5);
	xaxis  label= 'Mean';
run;

/******************Program 5.12****************/
/*SAS Program to Calculate Black-Schole Options Prices*/
data optionv;
	XP	=	3844.16;
	rf	=	0.04339;
	price	=	3844.16;
	vol	=	0.19031;
	time	=	0.087611225;
	c=BLKSHCLPRC(xp,time,price,rf,vol);
	p=BLKSHPTPRC(xp,time,price,rf,vol);
	label 
		XP	=	'Exercise Price'
		rf	=	'Risk free rate'
		price	=	'Current Price'
		vol	=	'Volatility'
		time	=	'Time'
		C ='Call Price'
		P ='Put Price'
		sc ='Synthetic Call'
		sp ='Synthetic Put';
	format rf percent8.2 vol percent8.2 time best8.2 c dollar8.2 p dollar8.2 sc dollar8.2 sp dollar8.2;
	sc=p+price-XP*exp(-rf*time);
	sp=c-price+XP*exp(-rf*time);
run;

proc print data=optionv noobs label;
	title 'Valuing Equity Options on the S&P 500 Index';
	var XP	rf	price vol time;
	var c p  /style(data)=[fontweight=bold  backgroundcolor=liggr];
run;

title;

/******************Program 5.13****************/
/*Bootstrapping Option Prices*/
data cspx_ret;
	set spx_ret(where=(date>'31dec17'd));
	block =ceil(_n_/6);
run;

proc surveyselect data=cspx_ret out=bootsamp4
	method=urs sampsize=3 reps=1000 seed=54321;
	cluster block;
run;

ods exclude all;

proc means data=bootsamp4 mean std;
	by Replicate;
	var  price Mret;
	ods output summary=bootstats4
		(keep= replicate price_mean price_stddev Mret_mean Mret_StdDev);
run;

ods exclude none;
%let time=32; /*Time to expiration in days*/
%let rf=0.04340;/*Annualized Risk-free rate*/
%let price = 3844.16; /*Current Stock and Exercise Price*/

data optionvalue;
	set bootstats4;
	by replicate;
	exp=&time/365.25;/*calculating expiration in years)*/
	Sigma = mret_stddev*sqrt(12); /*Annualized monthly volatility*/
	CallP =BLKSHCLPRC(&price,exp,&price,&rf,sigma);
	PutP  =BLKSHPTPRC(&price,exp,&price,&rf,sigma);
	Label CallP = 'Call Price' PutP='Put Price' Sigma ='Volatility';
	*format Sigma percent8.2;
run;

title 'Valuing Equity Options on the S&P 500 Index';
proc tabulate data=optionvalue;
	var Sigma CallP PutP;
	table  (mean stderr ),(Sigma*F=percent8.2  CallP*F=dollar8.2 PutP*F=dollar8.2);
		Label CallP = 'Call Price' PutP='Put Price' Sigma ='Volatility';
		Keylabel StdErr='SE';
run;

proc sgplot data=optionvalue;
	inset "Block Bootstrapped Stock Return Volatility"/title=" Distribution of One Month S&P 500 Index Option Prices" position=top 
		textattrs=(family="Times New Roman" color=darkblue size=12 ) valuealign=center 
		titleattrs=(family="Times New Roman" color=darkblue size=12 weight=bold) labelalign=center;
	histogram CallP/ nbins=20 dataskin=matte fill transparency=0.8 fillattrs=(color=blue );
	histogram PutP/ nbins=20 dataskin=matte fill transparency=0.8 fillattrs=(color=depk);
	density CallP/ legendlabel= "(Normal Density Plot for Call Prices)";
	density PutP/ legendlabel= "(Normal Density Plot for Put Prices)";
	yaxis values=(0 to 35 by 5) ;
	xaxis  label= 'Option Prices' valuesformat=dollar8.2;
run;
title;