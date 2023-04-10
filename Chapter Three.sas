


/******************Program 3.7A****************/

/*Simulating Random Walk Returns */
/************Data Step************/
%let N=100;%let sigma=0.01;%let mu=0;
data simul7A; 
    format Date datetime.; 
    keep Date ret;
    call streaminit(4321);
	InitDate = '2Jan2021:00:00'dt;
	dt=0;
	Ret=&mu;
    do i = 1 to &N;
		dt+1;
        Date=	intnx('minutes',InitDate,dt);/*Simulating Date variable*/
        err= (rand("normal",&mu,&sigma)); /*Simulating Gaussian Errors*/
		Ret = Ret+err; /*Cumulating Values Over time*/
         output;
      end;
run;
proc sgplot data=simul7A;
inset "DATA Step"/title="Series Plot of Random Walk with Drift" position=top 
textattrs=(family="Times New Roman" color=darkblue size=12 ) valuealign=center 
titleattrs=(family="Times New Roman" color=darkblue size=12 weight=bold) labelalign=center;
series x=date y=Ret;
xaxis label= "Date" valuesformat= datetime.;
yaxis label= "FX Returns" valuesformat= percent8.2 values=(-0.1 to 0.1 by .02);
run;



/******************Program 3.7B****************/
/*Simulating Random Walk Returns */
/************IML Procedure************/
%let N=100;%let sigma=0.01;%let mu=0;
proc iml;
Rt =j(&n,2,.);
xj= j(&n,1,.);
InitDate = '2Jan2021:00:00'dt; /*Initial Date*/
call randseed(4321);
call randgen(xj,"Normal",&mu,&sigma);
Rt[,2]=xj;
Rt[,2]=cusum(Rt[,2]); /*Cumulating Values over time*/
dt=0;
do i = 1 to &n;
	dt=dt+1;
	Rt[i,1]=intnx('minutes',InitDate,dt);/*Simulating Date variable*/
end;
vname ={"Date" "Ret"}; /*specify column and row label*/
create simul7B from Rt[colname=vname] ;
append from Rt; close simul7B;
quit;

proc sgplot data=simul7B;
inset "PROC IML"/title="Series Plot of Random Walk with Drift" position=top 
textattrs=(family="Times New Roman" color=darkblue size=12 ) valuealign=center 
titleattrs=(family="Times New Roman" color=darkblue size=12 weight=bold) labelalign=center;
series x=date y=Ret;
xaxis label= "Date" valuesformat= datetime.;
yaxis label= "Returns" valuesformat= percent8.2 values=(-0.1 to 0.01 by .02);
run;

proc univariate data=simul7b;
var Ret;
run; 




/******************Program 3.8****************/
/*Simulating the Sample Path for S&P 500 Index*/
%let smean=0.0067658; %let ssd = 0.04465726; %let inprice=3756.07;
data simul8;
format Date monyy.;
keep Date Fmret Sumret Price;
call streaminit(4321);
InitDate = '2Jan2021'd;
Sumret=&smean-&ssd *0.5;
do iter=1 to 24; /*number of replication*/
   Date=intnx('month',InitDate,dt,'end');/*Simulating Dates*/
   Fmret =rand("normal",&smean,&ssd);/*Simulating Monthly Returns*/
   Sumret =sumret+fmret; /*Cumulating the returns*/
   Price =&inprice*exp(sumret);/*Continuously compounded Prices*/
   dt+1;
   output;
end;
label
 Fmret='Simulated Monthly Returns'
 Price = 'Simulated Monthly Index Level'
 Sumret = 'Cumulative Monthly Returns';
run;

proc sgplot data=simul8;
inset "January 2021 to December 2022" 
/title="Simulated Values of S&P 500" position=top 
textattrs=(family="Times New Roman" color=darkblue size=12 ) valuealign=center 
titleattrs=(family="Times New Roman" color=darkblue size=12 weight=bold) labelalign=center;
series x=date y=Price;
series x=date y=fmret/ y2axis;
xaxis label= "Date" valuesformat= monyy. values=('31Jan21'D to '31Dec22'D by month);
yaxis  valuesformat= best8.2 values=(1000 to 6500 by 500);
y2axis valuesformat= percent8.2 values=(-0.1 to 0.4 by .1);
run;




/******************Program 3.9****************/
%datapull(vix,vix.sas7bdat);
ods graphics on;
proc arima data=VIX (where=(date>'31dec2017'd)) out=work.tarama 
 plots(unpack)=(series(acf pacf) residual(acf pacf normal) forecast(forecast)) ;
    identify var=vixcls; 
	/*calculates the first difference of the volume AR(1,1,1)*/
	estimate p=1 plot  ;
run ;

/******************Program 3.9A****************/
/*Simulating Daily CBOE VIX Index Ornstein-Uhlenbeck Process*
/*Numerical Solution*/
%let N=730;%let sigma=2.412801;%let alpha=13.8262; %let beta=0.97559;
proc iml;
X0=0;
theta=1-&beta; /*Theta is calculated from beta*/
mu=&alpha/theta; /*Mu is calculated from regression alpha called */
xj= j(&n,1,.); /*vector random variables*/
Volt=j(&n,1,0); /*Interest rate vector*/
Vol=j(&n,2,&mu); /*Vector to merge simulate rates and date*/
InitDate = '1jan2021'd; /*Initial Date*/
call randseed(4321);
call randgen(xj,"Normal");
Volt=X0*exp(-theta)+mu*(1-exp(-theta))+&sigma*exp(-2*theta)*xj;
Vol[,2]=Volt;
dt=0;
do i = 1 to &n;
	dt=dt+1;
	Vol[i,1]=intnx('day',InitDate,dt);/*Simulating Date variable*/
end;
run series(Vol[,1],Vol[,2]);
vname ={"Date" "VIX"}; /*specify column and row label*/
create simul10A from Vol[colname=vname] ;
append from Vol; close simul10;
quit;
proc sgplot data=simul10A;
inset "Ornstein-Uhlenbeck Process -Numerical Approach"/title="Simulating CBOE Volatility (VIX) Index" position=top 
textattrs=(family="Times New Roman" color=darkblue size=12 ) valuealign=center 
titleattrs=(family="Times New Roman" color=darkblue size=12 weight=bold) labelalign=center;
series x=date y=VIX;
xaxis label= "Date" valuesformat=mmddyy10. values=('2jan21'd to '31dec21'd by month) interval=day  ;
yaxis label= "Volatility" valuesformat= best8.2 values=(0 to 30 by 5);
run;

/******************Program 3.9B****************/
/*Simulating Daily CBOE Volatility (VIX) Index Using AR(1) Model*/
%let N=730;%let sigma=2.412801;%let mu=13.8262; %let beta=0.97559;
proc iml;
xj= j(&n,1,.); /*vector random variables*/
Volt=j(&n,1,0); /*Interest rate vector*/
Vol=j(&n,2,&mu); /*Vector to merge simulate rates and date*/
InitDate = '1jan2021'd; /*Initial Date*/
call randseed(4321);
call randgen(xj,"Normal");
Volt=&mu+&beta*lag(Volt)+&sigma*xj;
Vol[,2]=Volt;
dt=0;
do i = 1 to &n;
	dt=dt+1;
	Vol[i,1]=intnx('day',InitDate,dt);/*Simulating Date variable*/
end;
call series(vol[,1],vol[,2]);
vname ={"Date" "VIX"}; /*specify column and row label*/
create simul10B from Vol[colname=vname] ;
append from Vol; close simul10B;
quit;
proc sgplot data=simul10B;
inset "Discretized OU Process - AR(1)"/title="Simulating CBOE Volatility (VIX) Index" position=top 
textattrs=(family="Times New Roman" color=darkblue size=12 ) valuealign=center 
titleattrs=(family="Times New Roman" color=darkblue size=12 weight=bold) labelalign=center;
series x=date y=VIX;
xaxis label= "Date" valuesformat=mmddyy10. values=('2jan21'd to '31dec21'd by month) interval=day  ;
yaxis label= "Volatility" valuesformat= best8.2 values=(0 to 30 by 5);
run;




/******************Program 3.10****************/
/*Montecarlo Simulation of S&P 500 Levels*/
%let smean=0.0067658; %let ssd = 0.04465726; %let inprice=3756.07;
%let nreps=1000;/*number of repititions*/
data simul9;
format Date monyy.;
keep Date sampleID Fmret Sumret Price;
InitDate = '2Jan2021'd;
Sumret=&smean-&ssd *0.5;
do sampleID=1 to &nreps;
do iter=1 to 24; /*number of replication*/
   Date=intnx('month',InitDate,dt,'end');/*Simulating Dates*/
   Fmret =rand("normal",&smean,&ssd);/*Simulating Monthly Returns*/
   Sumret =sumret+fmret; /*Cumulating the returns*/
   Price =&inprice*exp(sumret);/*Continuously compounded Prices*/
   dt+1;
   output;
end;
dt=0;sumret=0;
end;
label
 Fmret='Simulated Monthly Returns'
 Price = 'Simulated Monthly Index Level'
 Sumret = 'Cumulative Monthly Returns'
 sampleID = 'Sample ID';
run;
 /*Graphing Simulated SP500 Levels*/
proc sgplot data=simul9;
inset "January 2021 to December 2022" 
/title="Monte Carlo Simulation of S&P 500 Index Levels" position=top 
textattrs=(family="Times New Roman" color=darkblue size=12 ) valuealign=center;
	series x=Date y=Price / group=SampleID;
	xaxis grid;
	yaxis grid values=(2000 to 9000 by 1000); ;
run;




/******************Program 3.11****************/
/*Calculating Average Path of the S&P 500 Index*/
proc sort data=simul9 out=ssimul9;
by Date;
run;
proc means data=ssimul9 mean noprint;
by date;
var price;
output out=out_simul9 mean=Price;
run;

proc sgplot data=out_simul9;
inset "Average Path from January 2021 to December 2022" 
/title="Monte Carlo Simulation of S&P 500 Index Levels" position=top 
textattrs=(family="Times New Roman" color=darkblue size=12 ) valuealign=center;

	series x=Date y=Price ;
	xaxis  grid valuesformat= monyy. values=('31Jan21'D to '31Dec22'D by month);
	yaxis grid values=(2000 to 6000 by 1000); 
run;

proc means data=simul9(where=(Date='31Dec22'D))  mean std min max median n  p5 
		p10 q1 q3 p95 p99 qmethod=os;
	var Price;
run;




/******************Program 3.12****************/
/*Calculating NPV and IRR Using SAS*/
data capbud1;
format CF0 nlmny12.2  CF1 nlmny12.2 CF2 nlmny12.2 
CF3 nlmny12.2 CF4 nlmny12.2 CF5 nlmny12.2 Rate percent8.2;
Rate=0.13;
CF0=-53000;
CF1=15800;
CF2=13100;
CF3=16700;
CF4=19700;
CF5=14300;
NPV=finance('npv',rate,cf1,cf2,cf3,cf4,cf5)+cf0;
IRR=finance('irr',cf0,cf1,cf2,cf3,cf4,cf5);
format NPV nlmny12.2 IRR percent8.2;
run;
title 'Capital Budget Worksheet';
proc print data=capbud1 noobs ;
var CF0 CF1 CF2 CF3 CF4 CF5 Rate;
var NPV IRR/style(data)={just=l fontweight=bold };
run;
title;




/******************Program 3.13****************/
/*Capital Budgeting Worksheet in SAS*/
/*Storing Key Project Info in Dataset*/
data pinfo;
IC=	5245000; TL	=	4; SPU=	35; CPU=	12;
FC=	550000; NWC=	132400; RR=	0.123	; TR=	0.21;
QTY	= 120000 ;
run;




/******************Program 3.14****************/
/*Creating Capital Budget Worksheet Using PROC COMPUTAB*/
title 'ABA Manufacturing Inc.';
title2 'Capital Budgeting Worksheet ';
proc computab data=pinfo out=capbud2   ;
cols YR0 YR1 YR2 YR3 YR4 /'Year' f=10.2;
col  YR0/ 'Zero' zero=' ' ;
col  YR1/ 'One';
col  YR2/ 'Two';
col  YR3/ 'Three';
col  YR4/ 'Four';
/*Defining Proforma Income Statement Items*/
row Rev /'Income Statement' 'Sales' format= nlmny16.2 ;
row	COGS /	'Cost of Sales'  format= nlmny16.2 ;
row GP  /'Gross Profit' dol ul format= nlmny16.2 ;
row DP /'Depreciation' format= nlmny16.2 ;
row	SGA	 /'General Expense' ul format= nlmny16.2 ;
row EBT /'Taxable Income' ul format= nlmny16.2;
row Tax	 / 'Taxes'  format= nlmny16.2;
row NI  /'Net Income' dol dul format= nlmny16.2 skip;

/*Defining Cash Flow Worksheet Items*/
row OCF /' '
	     'Cash Flow Estimation:'
		'Project Cash Flows' 'Operating Cash Flow' format= nlmny16.2 ;
row CAPEX /'Capital Expenditure'  format= nlmny16.2 zero=' ' ;
row WCR /'Working Capital Requirement'  format= nlmny16.2  zero=' '; 
row CF/ 'Net Project Cash Flow' dol dul format= nlmny16.2 skip ;

/*Defining Decision Criteria*/
row NPV /' '
		'NPV Calculation:'
		'Net Present Value' format= nlmny16.2 zero=' ';
row IRR/'Internal Rate of Return'  LJC format=percent8.2 zero=' ';

/*Populating Income Statement Items with Values*/
colcalc:
/*Revenue of 5% per year and cost increase of 3%*/
if REV then do;
YR1=QTY*SPU; YR2=YR1*1.05; YR3=YR2*1.05; YR4=YR3*1.05;
end;
if COGS then do;
YR1=QTY*CPU; YR2=YR1*1.03; YR3=YR2*1.03; YR4=YR3*1.03;
end;
rowcalc: 
if  _COL_>1  then do
GP=REV-COGS; /*Gross Profit*/
DP = IC/TL;  /*Annual Depreciation*/
SGA = FC;    /*Selling and General Administrative*/
EBT = GP-DP-SGA; /*Taxable Income*/
TAX =TR*EBT;		 /*Income Taxes*/
NI = EBT-TAX;	 /* Net Income*/
/*Cash Flow Begins Here*/
CashFlows:
OCF = EBT+DP-TAX; /*Can also be NI+DP*/
WCR = 0.02*Rev;
end;

/*Controls to enter only values for initial date*/
if _col_=1 then do; /*Initial CAPEX and WCR*/
CAPEX = ic;
WCR =NWC;
end;
CF = OCF-CAPEX-WCR;

/*Decision Critieria*/
Decisions:
npv=cf;
irr=cf;

colcalc2:
if npv then do;
temp1 =finance('npv',rr,yr1, yr2, yr3, yr4)+yr0;
yr0=temp1;
yr1=0; yr2=0; yr3=0; yr4=0; yr5=0;
end;
if IRR then do;
temp2 =finance('irr',yr0, yr1, yr2, yr3, yr4);
yr0=temp2;
yr1=0; yr2=0; yr3=0; yr4=0;
end;
run;
title;
title2;





/******************Program 3.15****************/
/*SAS Macro to Implement Monte Carlo Simulation of Capital Budgeting*/
%macro capsimul(nrep); /*spccify number of reps*/
proc datasets nodetails nolist;
delete simul11;
run;

%local  i nrep; /*local macro variable*/

%do i=1 %to &nrep;

/******Reused Same Code from Program 3.XXXXX****/
data pinfo;
IC	=	5245000	;
TL	=	4	;
SPU	=	35	;
CPU	=	12	;
FC	=	550000	;
NWC	=	132400	;
RR	=	0.123	;
TR	=	0.21	;
QTY	= 120000 *(1+rand("normal",0,0.1)) ;/*simulating quanty sold*/
run;


proc computab data=pinfo out=capbud2 noprint  ;
cols YR0 YR1 YR2 YR3 YR4 /'Year' f=10.2;
col  YR0/ 'Zero' zero=' ' ;
col  YR1/ 'One';
col  YR2/ 'Two';
col  YR3/ 'Three';
col  YR4/ 'Four';
/*Defining Proforma Income Statement Items*/
row Rev /'Income Statement' 'Sales' format= nlmny16.2 ;
row	COGS /	'Cost of Sales'  format= nlmny16.2 ;
row GP  /'Gross Profit' dol ul format= nlmny16.2 ;
row DP /'Depreciation' format= nlmny16.2 ;
row	SGA	 /'General Expense' ul format= nlmny16.2 ;
row EBT /'Taxable Income' ul format= nlmny16.2;
row Tax	 / 'Taxes'  format= nlmny16.2;
row NI  /'Net Income' dol dul format= nlmny16.2 skip;

/*Defining Cash Flow Worksheet Items*/
row OCF /' '
	     'Cash Flow Estimation:'
		'Project Cash Flows' 'Operating Cash Flow' format= nlmny16.2 ;
row CAPEX /'Capital Expenditure'  format= nlmny16.2 zero=' ' ;
row WCR / 'Working Capital Requirement'  format= nlmny16.2  zero=' '; 
row CF	/ 'Net Project Cash Flow' dol dul format= nlmny16.2 skip ;

/*Defining Decision Criteria*/
row NPV /' '
		'NPV Calculation:'
		'Net Present Value' format= nlmny16.2 zero=' ';
row IRR/'Internal Rate of Return'  LJC format=percent8.2 zero=' ';

/*Populating Income Statement Items with Values*/
colcalc:
/*Revenue of 5% per year and cost increase of 3%*/
if REV then do;
YR1=QTY*SPU; YR2=YR1*1.05; YR3=YR2*1.05; YR4=YR3*1.05;
end;
if COGS then do;
YR1=QTY*CPU; YR2=YR1*1.03; YR3=YR2*1.03; YR4=YR3*1.03;
end;
rowcalc: 
if  _COL_>1  then do
GP=REV-COGS; /*Gross Profit*/
DP = IC/TL;  /*Annual Depreciation*/
SGA = FC;    /*Selling and General Administrative*/
EBT = GP-DP-SGA; /*Taxable Income*/
TAX =TR*EBT;		 /*Income Taxes*/
NI = EBT-TAX;	 /* Net Income*/

/*Cash Flow Begins Here*/
CashFlows:
OCF = EBT+DP-TAX; /*Can also be NI+DP*/
WCR = 0.02*Rev;
end;

/*Controls to enter only values for initial date*/
if _col_=1 then do; /*Initial CAPEX and WCR*/
CAPEX = ic;
WCR =NWC;
end;
CF = OCF-CAPEX-WCR;


/*Decision Critieria*/

Decisions:
npv=cf;
irr=cf;

colcalc2:
if npv then do;
temp1 =finance('npv',rr,yr1, yr2, yr3, yr4)+yr0;
yr0=temp1;
yr1=0; yr2=0; yr3=0; yr4=0; yr5=0;
end;
if IRR then do;
temp2 =finance('irr',yr0, yr1, yr2, yr3, yr4);
yr0=temp2;
yr1=0; yr2=0; yr3=0; yr4=0;
end;
run;

/****Reused Codes Ends Here***?

/*Merge project info data with cap budgeting results*/
Data _simul_;
merge pinfo(keep=QTY) capbud2( obs=1 keep=NPV IRR);
sampleID=&i;
label QTY= 'Quantity Sold' NPV='Net Present Value' IRR='Internal Rate of Return';
format NPV nlmny16.2 QTY best10.2 IRR percent8.2;
run;

/*updated table with results from new interation*/
proc append base=simul11 data=_simul_ force;
run;
%end;
%mend;

/*Macro invoked to do 1000 repititions*/
%capsimul(1000);


proc tabulate data=simul11;
var qty npv irr;
table qty*F=bestn10.2 npv*F=dollar15.2 irr*F=percent8.2,(min median mean max  p5  p10 ) ;
run;




/******************Program 3.16****************/
/*SAS Code to Graph Projects NPV Distribution*/
proc sgplot data=simul11;
inset "NPV Distribution (Monte Carlo Simulation)"/title="ABA Manufacturing Inc." position=top 
textattrs=(family="Times New Roman" color=darkblue size=12 ) valuealign=center 
titleattrs=(family="Times New Roman" color=darkblue size=12 weight=bold) labelalign=center;
histogram npv / nbins=20 dataskin=matte fill transparency=0.8 fillattrs=(color=bipb );
density npv/ legendlabel= "(Normal Density Plot for Project's NPV)" ;
yaxis values=(0 to 20 by 2.5);
run;

proc sgplot data=simul11;
inset "Quantity Sold Distribution (Monte Carlo Simulation)"/title="ABA Manufacturing Inc." position=top 
textattrs=(family="Times New Roman" color=darkblue size=12 ) valuealign=center 
titleattrs=(family="Times New Roman" color=darkblue size=12 weight=bold) labelalign=center;
histogram qty / nbins=20 dataskin=matte fill transparency=0.8 fillattrs=(color=bipb );
density qty/ legendlabel= "(Normal Density Plot for Project's NPV)" ;
yaxis values=(0 to 20 by 2.5);
run;




/******************Program 3.17****************/
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
keep xmin xmax;
run;
proc univariate data=simul13;
var xmin xmax;
histogram xmin/gumbel ;
histogram xmax/gumbel  ;
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


/******************Program 3.18****************/
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



/******************Program 3.19****************/
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



/******************Program 3.20****************/
/*Simulating Data from Specials Cases Of GPD*/
%let n = 1000;
%let theta =0.08359; 
%let xi = 0.0036013;
data GPD;
do i = 1 to &n;
   /* Generalized Pareto(scale=theta, shape=xi) */   
   u = rand("Uniform");
   Xuni = &theta/&xi *(u**(-&xi)-1);
   Xexp =rand("exponential", &theta);
   output;
end;
drop i;
label 
xuni =' Uniformly Distributed Return'
Xexp = 'Exponentially Distributed Returns';
run;
proc univariate data=gpd;
var xuni xexp;
histogram xuni/ pareto;
histogram xexp/pareto;
ods select Histogram ParameterEstimates ;
run;



/******************Program 3.21****************/
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



/******************Program 3.22****************/
/*Estimating parameters of distribution using PROC SEVERITY*/
proc severity data=exspx crit=aicc outest=sevest(where=( _type_='EST')) 
       print=all plots (histogram)= all ;
   loss exmret ;
  dist gpd  ;
  nloptions tech=quanew maxiter=200;
run;
/*Creating macro variable with data step*/
data _null_;
set sevest;
call symput('stheta',theta);
call symput('sxi',xi);
run;



/******************Program 3.23****************/
/*Calculating GPD VaR*/
data  gpdvar;
VaR90= &mu-&stheta*log((37*0.1/187));
VaR95= &mu-&stheta*log((37*0.05/187));
VaR99= &mu-&stheta*log((37*0.01/187));
ES90 = Var90/(1-&sxi)+(&stheta-&sxi*&thresh)/(1-&sxi); 
ES95 = Var95/(1-&sxi)+(&stheta-&sxi*&thresh)/(1-&sxi); 
ES99 = Var99/(1-&sxi)+(&stheta-&sxi*&thresh)/(1-&sxi); 
label
var90 ='90% VaR' var95 ='95% VaR' var99 ='99% VaR'
es90 ='90% Expected Shortfall' es95 ='95% Expected Shortfall' es99 ='99% Expected Shortfall';
format var90 percent8.2  var95 percent8.2  var99 percent8.2
es90 percent8.2 es95 percent8.2  es99 percent8.2 ;
run;
title ' One-Month Value-at-Risk for the S&P 500 Index';
proc print data=gpdvar noobs label;
run;
title;



/******************Program 3.24****************/
/*Estimating and Simulating Returns from Student-T Copula*/%datapull(finport,finport.sas7bdat);
proc copula data = finport;
   var raxp rbac rc rgs rjpm;
   /* fit T-copula to Monthly stock returns*/
   fit T /
          marginals = empirical
          method    = MLE
  /*Generate Plots (Chi - tail dependence plot))*/
          plots (chi)  = (datatype = both);
  /* simulate 10000 observations*/
   simulate /
            ndraws = 10000
            seed   = 4321
            out    = simulR
            plots(chi) = (datatype = original);
run;




/******************Program 3.25****************/
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



/******************Program 3.26****************/
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
inset "Basic Bootstrapping (URS)"/title=" Distribution of Sample Means S&P 500 Monthly Returns" position=top 
textattrs=(family="Times New Roman" color=darkblue size=12 ) valuealign=center 
titleattrs=(family="Times New Roman" color=darkblue size=12 weight=bold) labelalign=center;
histogram mret_mean/ nbins=20 dataskin=matte fill transparency=0.8 fillattrs=(color=blue );
density mret_mean/ legendlabel= "(Normal Density Plot for Sample Means)" ;
yaxis values=(0 to 25 by 5);
run;




/******************Program 3.27****************/
/******************Program 3.27A****************/
/*Block Bootstrapping Using SAS*/
data aspx_ret;
set spx_ret;
block =ceil(_n_/12);
run;
proc surveyselect data=aspx_ret out=bootsamp2
method=urs sampsize=1 reps=1000 seed=12345;
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

/******************Program 3.27B****************/
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
append from yb; close bootsamp;
create bootstats3 from smean;
append from smean; close bootstats3;
print ("Block Bootstrapping Using PROC IML");
colnames = "obs1":"obs12";
print (yb[1:2,1])[label='Block ID']  (yb[1:2, 2:13])[format=percent8.2 colname=colnames]; /*Print first 13 obs*/
print ("Descriptive Statistics");
print (bsmean)[label='Mean'] (ssd) [label='Std Error']; /*Standard error of sample means*/
quit;
proc sgplot data=bootstats3;
inset "Block Bootstrapping (RS)"/title=" Distribution of Sample Means S&P 500 Monthly Returns" position=top 
textattrs=(family="Times New Roman" color=darkblue size=12 ) valuealign=center 
titleattrs=(family="Times New Roman" color=darkblue size=12 weight=bold) labelalign=center;
histogram col1/ nbins=20 dataskin=matte fill transparency=0.8 fillattrs=(color=blue );
density col1/ legendlabel= "(Normal Density Plot for Sample Means)" ;
yaxis values=(0 to 25 by 5);
xaxis  label= 'Mean';
run;





/******************Program 3.28****************/
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
P ='Put Price';
format rf percent8.2 vol percent8.2 time best8.2;
run;
proc print data=optionv noobs label ;
title 'Valuing S&P 500 Index Options';
var XP	rf	price vol time;
var c p /style(data)=[fontweight=bold  backgroundcolor=liggr]; 
run;
title;



/******************Program 3.29****************/

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
format Sigma percent8.2;
run;

proc tabulate data=optionvalue  ;
var Sigma CallP PutP; 
table  (mean stderr ),(Sigma*F=percent8.2  CallP PutP)  ;
Label CallP = 'Call Price' PutP='Put Price' Sigma ='Volatility';
Keylabel StdErr='SE';
run;

proc sgplot data=optionvalue;
inset "Block Bootstrapping Stock Return Volatility"/title=" Distribution of One Month S&P 500 Index Option Prices" position=top 
textattrs=(family="Times New Roman" color=darkblue size=12 ) valuealign=center 
titleattrs=(family="Times New Roman" color=darkblue size=12 weight=bold) labelalign=center;
histogram CallP/ nbins=20 dataskin=matte fill transparency=0.8 fillattrs=(color=blue );
histogram PutP/ nbins=20 dataskin=matte fill transparency=0.8 fillattrs=(color=depk);  
density CallP/ legendlabel= "(Normal Density Plot for Call Prices)" ;
density PutP/ legendlabel= "(Normal Density Plot for Put Prices)" ;
yaxis values=(0 to 35 by 5);
xaxis  label= 'Option Prices';
run;

