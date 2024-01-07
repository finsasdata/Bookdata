/******************Program 4.1****************/
/*Simulating Data from Bernoulli Distribution */
%let prob=0.5; /*Probability of Success*/
data simul1;
	call streaminit(4321);/*Random seed generator*/
	do i=1 to 100; /*Number of Iteration*/
		Simnum =rand("Bernoulli", &prob); /*Invoking the RAND function*/
		output; /*To the values from each iteration*/
	end;
run;

/*Computing Simulation Statistics*/
ods graphics on;

proc freq  data=simul1;
	table Simnum/
		plots = freqplots;
run;


/******************Program 4.2****************/
/*Simulating Data from Binomial Distribution */
%let prob=0.5; /*Probability of Success*/
%let num = 1; /*Number of trials*/
data simul2;
	call streaminit(4321);/*Random seed generator*/
	num = 1;

	do i=1 to 100; /*Number of Iteration*/
		Simnum =rand("Binonmial", &prob,&num); /*Invoking the RAND function*/
		output; /*To the values from each iteration*/
	end;
run;

/*Computing Simulation Statistics*/
ods graphics on;

proc freq  data=simul2;
	table Simnum/
		plots = freqplots;
run;

/******************Program 4.3****************/
/*Simulating Data from the Normal Distribution */
data simul3;
	call streaminit(4321);/*Random seed generator*/

	do i=1 to 100; /*Number of Iteration*/
		Simnum =rand("normal"); /*Invoking the RAND function*/
		output; /*To the values from each iteration*/
	end;
run;

/*Computing Simulation Statistics*/
proc univariate  data=simul3;
	Var  Simnum;
	histogram;
run;

/******************Program 4.4****************/
/*Simulating S&P 500 Returns Using the Normal Distribution */
/*Specify distribution statistics*/
%let smean=0.0067658;
%let ssd = 0.04465726;

data simul4 (keep=i simnum);
	call streaminit(4321);/*Random seed generator*/
	do i=1 to 1000; /*Number of Iteration*/
		Simnum =rand("normal",&smean,&ssd); /*Invoking the RAND function*/
		output; /*To the values from each iteration*/
	end;
run;

/*Computing Simulation Statistics*/
proc univariate  data=simul4;
	Var  Simnum;
	histogram / normal kernel;
run;

/******************Program 4.5****************/
/*Simulating Stock Returns Using the Multivariate Normal Distribution */
%let n=5000; /*number of repetitions or sample size*/

proc iml;
	mean = {0.0215,-0.0038}; /*Mean Vector*/
	vcv ={0.0122 0.000452,
		0.000452 0.00258}; /*Covariance Matrix*/
	call randseed (4321);
	MRET =RandNormal(&n,mean,vcv); /*Simulate 1000x2 vector*/
	smean =mean(MRET); /*calculate sample mean*/
	svcv = cov(MRET); /*calculate sample covariance*/
	scorr =inv(sqrt(diag(svcv)))*svcv*inv(sqrt(diag(svcv)));/*calculate sample correlations*/
	vname ={"AMZN","WMT"}; /*specify column and row label*/
	print(MRET[1:5,])[colname=vname];
	print(smean)[colname=vname] [label='Sample Mean'];
	print(svcv)[colname=vname rowname=vname] [Label='Sample Covariance'];
	print(scorr)[colname=vname rowname=vname] [Label='Sample Correlations'];
	/*Following step creates a SAS dataset
	(simul5) to store the simulated values*/
	create simul5 from MRET[colname=vname];
	append from MRET;
	close simul5;
quit;
proc sgplot data=simul5;
	inset "Amazon and Walmart"/title="Scatter Plot of Monthly Returns" position=top 
		textattrs=(family="Times New Roman" color=darkblue size=12 ) valuealign=center 
		titleattrs=(family="Times New Roman" color=darkblue size=12 weight=bold) labelalign=center;
	scatter x=amzn y=wmt;
	reg  x=amzn y=wmt/ clm;
	xaxis label= "Amazon Returns" valuesformat= percent8.2 values=(-0.4 to 0.4 by .1);
	yaxis label= "Walmart Returns" valuesformat= percent8.2 values=(-0.2 to 0.2 by .1);
run;

/******************Program 4.6****************/
/*Simulating High-Frequency Foreign Exchange Returns*/
/*Simulating 10-minute FX Returns */
data simul6;
	%let mu=-0.0000066;
	%let sigma=0.003600;
	format Date datetime.;
	keep Date returns;
	call streaminit(4321);
	InitDate = '2Jan2021:00:00'dt;
	dt=0;

	do i = 1 to 1008;/*Number of 10 mins in one week*/
		dt+10;
		Date=	intnx('minutes',InitDate,dt);
		Returns= rand('normal',&mu,&sigma);
		output;
	end;
run;
proc sgplot data=simul6;
	inset "USD/EUR Exchange Rate"/title="Simulated Ten-Minutes FX Returns" position=top 
		textattrs=(family="Times New Roman" color=darkblue size=12 ) valuealign=center 
		titleattrs=(family="Times New Roman" color=darkblue size=12 weight=bold) labelalign=center;
	series x=date y=Returns;
	xaxis label= "Date" valuesformat= datetime.;
	yaxis label= "Returns" valuesformat= percent8.2 values=(-0.02 to 0.02 by .01);
run;



/******************Program 4.7A****************/
/*Simulating Random Walk Returns */
/************Data Step************/
%let n=100;
%let sigma=0.001;
%let mu=0;

data simul7A;
	format Date datetime.;
	keep Date ret Err;
	call streaminit(4321);
	InitDate = '2Jan2021:00:00'dt;
	dt=0;
	Ret=&mu;

	do i = 1 to &n;
		dt+1;
		Date=	intnx('minutes',InitDate,dt);/*Simulating Date variable*/
		Err= (rand("normal",&mu,&sigma)); /*Simulating Gaussian Errors*/
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
	yaxis label= "FX Returns" valuesformat= percent8.2 values=(-0.04 to 0.04 by .01);
run;

/******************Program 4.7B****************/
/*Simulating Random Walk Returns */
/************IML Procedure************/
%let N=100;
%let sigma=0.001;
%let mu=0;

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
	create simul7B from Rt[colname=vname];
	append from Rt;
	close simul7B;
quit;

proc sgplot data=simul7B;
	inset "PROC IML"/title="Series Plot of Random Walk with Drift" position=top 
		textattrs=(family="Times New Roman" color=darkblue size=12 ) valuealign=center 
		titleattrs=(family="Times New Roman" color=darkblue size=12 weight=bold) labelalign=center;
	series x=date y=Ret;
	xaxis label= "Date" valuesformat= datetime.;
	yaxis label= "FX Returns" valuesformat= percent8.2 values=(-0.04 to 0.04 by .01);
run;

proc univariate data=simul7a;
	var Err Ret;
run;

/******************Program 4.8****************/
/*Simulating the Sample Path for S&P 500 Index*/
%let smean=0.0067658;/*Mean Return*/
%let ssd = 0.04465726;/*Standard Deviation of Returns*/
%let inprice=3756.07; /*Initial Index Level*/

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

/******************Program 4.9****************/
%datapull(vix,vix.sas7bdat);
ods graphics on;

proc arima data=VIX (where=(date>'31dec2017'd)) out=work.tarama 
	plots(unpack)=(series(acf pacf) residual(acf pacf normal) forecast(forecast));
	identify var=vixcls;

	/*calculates the first difference of the volume AR(1,1,1)*/
	estimate p=1 plot;
run;

/******************Program 4.9A****************/

/*Simulating Daily CBOE VIX Index Ornstein-Uhlenbeck Process*
/*Numerical Solution*/
%let N=364;
%let sigma=2.412801;
%let alpha=13.8262;
%let beta=0.97559;

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
create simul9A from Vol[colname=vname];
append from Vol;
close simul9A;
quit;

proc sgplot data=simul9A;
	inset "Ornstein-Uhlenbeck Process -Numerical Approach"/title="Simulating CBOE Volatility (VIX) Index" position=top 
		textattrs=(family="Times New Roman" color=darkblue size=12 ) valuealign=center 
		titleattrs=(family="Times New Roman" color=darkblue size=12 weight=bold) labelalign=center;
	series x=date y=VIX;
	xaxis label= "Date" valuesformat=mmddyy10. values=('2jan21'd to '31dec21'd by month) interval=day;
	yaxis label= "Volatility" valuesformat= best8.2 values=(0 to 30 by 5);
run;

/******************Program 4.9B****************/
/*Simulating Daily CBOE Volatility (VIX) Index Using AR(1) Model*/
%let N=364;
%let sigma=2.412801;
%let mu=13.8262;
%let beta=0.97559;

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
	create simul9B from Vol[colname=vname];
	append from Vol;
	close simul9B;
quit;
 data simulx;
 set simul9B;
 format Date mmddyy10.;
 run;

proc sgplot data=simul9B;
	inset "Discretized OU Process - AR(1)"/title="Simulating CBOE Volatility (VIX) Index" position=top 
		textattrs=(family="Times New Roman" color=darkblue size=12 ) valuealign=center 
		titleattrs=(family="Times New Roman" color=darkblue size=12 weight=bold) labelalign=center;
	series x=date y=VIX;
	xaxis label= "Date" valuesformat=mmddyy10. values=('2jan21'd to '31dec21'd by month) interval=day;
	yaxis label= "Volatility" valuesformat= best8.2 values=(0 to 30 by 5);
run;

/******************Program 4.10****************/
/*Montecarlo Simulation of S&P 500 Levels*/
%let smean=0.0067658;
%let ssd = 0.04465726;
%let inprice=3756.07;
%let nreps=1000;/*number of repititions*/

data simul10;
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

		dt=0;
		sumret=0;
	end;

	label
		Fmret='Simulated Monthly Returns'
		Price = 'Simulated Monthly Index Level'
		Sumret = 'Cumulative Monthly Returns'
		sampleID = 'Sample ID';
run;

/*Graphing Simulated SP500 Levels*/
proc sgplot data=simul10;
	inset "January 2021 to December 2022" 
		/title="Monte Carlo Simulation of S&P 500 Index Levels" position=top 
		textattrs=(family="Times New Roman" color=darkblue size=12 ) valuealign=center;
	series x=Date y=Price / group=SampleID;
	xaxis grid;
	yaxis grid values=(2000 to 9000 by 1000);
	;
run;

/******************Program 4.11****************/
/*Calculating Average Path of the S&P 500 Index*/
proc sort data=simul10 out=ssimul10;
	by Date;
run;

proc means data=ssimul10 mean noprint;
	by date;
	var price;
	output out=out_simul10 mean=Price;
run;

proc sgplot data=out_simul10;
	inset "Average Path from January 2021 to December 2022" 
		/title="Monte Carlo Simulation of S&P 500 Index Levels" position=top 
		textattrs=(family="Times New Roman" color=darkblue size=12 ) valuealign=center;
	series x=Date y=Price;
	xaxis  grid valuesformat= monyy. values=('31Jan21'D to '31Dec22'D by month);
	yaxis grid values=(2000 to 6000 by 1000);
run;

proc means data=simul10(where=(Date='31Dec22'D))  mean std min max median  p5 
	p10 q1 q3 p95 p99 qmethod=os;
	var Price;
run;

/******************Program 4.12****************/
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

title 'Capital Expenditure Worksheet';

proc print data=capbud1 noobs;
	var CF0 CF1 CF2 CF3 CF4 CF5 Rate;
	var NPV IRR/style(data)={just=l fontweight=bold };
run;

title;

/******************Program 4.13A****************/
/*Capital Budgeting Worksheet in SAS*/
/*Storing Key Project Info in Dataset*/
data pinfo;
	IC=	5245000;
	TL= 4;
	SPU= 35;
	CPU= 12;
	FC=	550000;
	NWC= 132400;
	RR=	0.123;
	TR=	0.21;
	QTY	= 120000;
run;
/******************Program 4.13B****************/
/*Creating Capital Budget Worksheet Using PROC COMPUTAB*/
title 'ABA Manufacturing Inc.';
title2 'Capital Expenditure Worksheet ';

proc computab data=pinfo out=capbud2;
	col YR0 YR1 YR2 YR3 YR4 /'Year' f=10.2;
	col  YR0/ 'Zero' zero=' ';
	col  YR1/ 'One';
	col  YR2/ 'Two';
	col  YR3/ 'Three';
	col  YR4/ 'Four';

	/*Defining Proforma Income Statement Items*/
	row Rev /'Income Statement' 'Sales' format= nlmny16.2;
	row	COGS /	'Cost of Sales'  format= nlmny16.2;
	row GP  /'Gross Profit' dol ul format= nlmny16.2;
	row DP /'Depreciation' format= nlmny16.2;
	row	SGA	 /'General Expense' ul format= nlmny16.2;
	row EBT /'Taxable Income' ul format= nlmny16.2;
	row Tax	 / 'Taxes'  format= nlmny16.2;
	row NI  /'Net Income' dol dul format= nlmny16.2 skip;

	/*Defining Cash Flow Worksheet Items*/
	row OCF /' '
		'Cash Flow Estimation:'
		'Project Cash Flows' 'Operating Cash Flow' format= nlmny16.2;
	row CAPEX /'Capital Expenditure'  format= nlmny16.2 zero=' ';
	row WCR /'Working Capital Requirement'  format= nlmny16.2  zero=' ';
	row CF/ 'Net Project Cash Flow' dol dul format= nlmny16.2 skip;

	/*Defining Decision Criteria*/
	row NPV /' '
		'NPV Calculation:'
		'Net Present Value' format= nlmny16.2 zero=' ';
	row IRR/'Internal Rate of Return'  LJC format=percent8.2 zero=' ';

	/*Populating Income Statement Items with Values*/
colcalc:

	/*Revenue of 5% per year and cost increase of 3%*/
	if REV then do;
		YR1=QTY*SPU;
		YR2=YR1*1.05;
		YR3=YR2*1.05;
		YR4=YR3*1.05;
	end;

	if COGS then do;
		YR1=QTY*CPU;
		YR2=YR1*1.03;
		YR3=YR2*1.03;
		YR4=YR3*1.03;
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
	if _col_=1 then do;

		/*Initial CAPEX and WCR*/
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
		tempnpv =finance('npv',rr,yr1, yr2, yr3, yr4)+yr0;
		yr0=tempnpv;
		yr1=0;
		yr2=0;
		yr3=0;
		yr4=0;
	
	end;

	if IRR then do;
		tempirr =finance('irr',yr0, yr1, yr2, yr3, yr4);
		yr0=tempirr;
		yr1=0;
		yr2=0;
		yr3=0;
		yr4=0;
	end;
run;

title;
title2;

/******************Program 4.14A****************/
/*SAS Macro to Implement Monte Carlo Simulation of Capital Budgeting*/
%macro capsimul(nrep); /*specify number of reps*/
	proc datasets nodetails nolist;
		delete simul11;
	run;

	%local  i nrep; /*local macro variable*/

	%do i=1 %to &nrep;

		/******Reused Same Code from Program 3.XXXXX****/
		data pinfo;
			IC	=	5245000;
			TL	=	4;
			SPU	=	35;
			CPU	=	12;
			FC	=	550000;
			NWC	=	132400;
			RR	=	0.123;
			TR	=	0.21;
			QTY	= 120000 *(1+rand("normal",0,0.1)) ;/*simulating quantity sold*/
		run;

		proc computab data=pinfo out=capbud2 noprint;
			cols YR0 YR1 YR2 YR3 YR4 /'Year' f=10.2;
			col  YR0/ 'Zero' zero=' ';
			col  YR1/ 'One';
			col  YR2/ 'Two';
			col  YR3/ 'Three';
			col  YR4/ 'Four';

			/*Defining Proforma Income Statement Items*/
			row Rev /'Income Statement' 'Sales' format= nlmny16.2;
			row	COGS /	'Cost of Sales'  format= nlmny16.2;
			row GP  /'Gross Profit' dol ul format= nlmny16.2;
			row DP /'Depreciation' format= nlmny16.2;
			row	SGA	 /'General Expense' ul format= nlmny16.2;
			row EBT /'Taxable Income' ul format= nlmny16.2;
			row Tax	 / 'Taxes'  format= nlmny16.2;
			row NI  /'Net Income' dol dul format= nlmny16.2 skip;

			/*Defining Cash Flow Worksheet Items*/
			row OCF /' '
			'Cash Flow Estimation:'
			'Project Cash Flows' 'Operating Cash Flow' format= nlmny16.2;
			row CAPEX /'Capital Expenditure'  format= nlmny16.2 zero=' ';
			row WCR / 'Working Capital Requirement'  format= nlmny16.2  zero=' ';
			row CF	/ 'Net Project Cash Flow' dol dul format= nlmny16.2 skip;

			/*Defining Decision Criteria*/
			row NPV /' '
				'NPV Calculation:'
				'Net Present Value' format= nlmny16.2 zero=' ';
			row IRR/'Internal Rate of Return'  LJC format=percent8.2 zero=' ';

			/*Populating Income Statement Items with Values*/
	colcalc:

			/*Revenue of 5% per year and cost increase of 3%*/
			if REV then do;
				YR1=QTY*SPU;
				YR2=YR1*1.05;
				YR3=YR2*1.05;
				YR4=YR3*1.05;
			end;

			if COGS then do;
				YR1=QTY*CPU;
				YR2=YR1*1.03;
				YR3=YR2*1.03;
				YR4=YR3*1.03;
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
			if _col_=1 then do;

				/*Initial CAPEX and WCR*/
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
				tempnpv =finance('npv',rr,yr1, yr2, yr3, yr4)+yr0;
				yr0=tempnpv;
				yr1=0;
				yr2=0;
				yr3=0;
				yr4=0;
				yr5=0;
			end;

			if IRR then do;
				tempirr =finance('irr',yr0, yr1, yr2, yr3, yr4);
				yr0=tempirr;
				yr1=0;
				yr2=0;
				yr3=0;
				yr4=0;
			end;
		run;

		/****Reused Codes Ends Here***/

		/*Merge project info data with cap budgeting results*/
		data _simul_;
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

/*Macro invoked to do 1000 repetitions*/
%capsimul(1000);




proc tabulate data=simul11;
	var qty npv irr;
	table qty*F=bestn10.2 npv*F=dollar15.2 irr*F=percent8.2,(min median mean max  p5  p10 );
run;

/******************Program 4.14B****************/
/*SAS Code to Graph Projects NPV Distribution*/
proc sgplot data=simul11;
	inset "NPV Distribution (Monte Carlo Simulation)"/title="ABA Manufacturing Inc." position=top 
	textattrs=(family="Times New Roman" color=darkblue size=12 ) valuealign=center 
	titleattrs=(family="Times New Roman" color=darkblue size=12 weight=bold) labelalign=center;
	histogram npv / nbins=20 dataskin=matte fill transparency=0.8 fillattrs=(color=bipb );
	density npv/ legendlabel= "(Normal Density Plot for Project's NPV)";
	yaxis values=(0 to 20 by 2.5);
run;

proc sgplot data=simul11;
	inset "Quantity Sold Distribution (Monte Carlo Simulation)"/title="ABA Manufacturing Inc." position=top 
		textattrs=(family="Times New Roman" color=darkblue size=12 ) valuealign=center 
		titleattrs=(family="Times New Roman" color=darkblue size=12 weight=bold) labelalign=center;
	histogram qty / nbins=20 dataskin=matte fill transparency=0.8 fillattrs=(color=bipb );
	density qty/ legendlabel= "(Normal Density Plot for Project's NPV)";
	yaxis values=(0 to 20 by 2.5);
run;