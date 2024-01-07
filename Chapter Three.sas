
/****************Program 3.1****************/
%datapull(rspx,rspx_monthly.sas7bdat);
ods graphics on;

/*Enable ODS graphics to automatically generate accompanying plots*/
proc x13 data=rspx_monthly  date=Date INTERVAL = month;
	var volume;
	transform function=log;
	arima model=( (0,1,1)(0,1,1) );/*ARIMA with Exponential Smoothing*/
	outlier CV=3.6;
	estimate;
	x11;
	output out=arspx_monthly a1 d1 d10 d12 d13 e18;

	/* a1 =original series|d1=Modified Series| d10= Seasonal component|
	  d12 = trend cycle components| d13 = irregular component*/
run;

ods graphics off;

/*Plot Time Series Component Using SGPLOT*/
proc sgplot data=arspx_monthly;
	title 'Timeseries Components of S&P 500 Trading Volume ';
	series	 x=date y=volume_d13 / name="Volumeb" lineattrs=(color=aquamarine) legendlabel= " Irregular Component";
	series  x=date y=volume_d10 / name="Volumea" 
		lineattrs=(color=green) legendlabel= " Seasonal Component";
	series x=date y=volume_d12 /name="Volumec" y2axis   lineattrs=(color=blue) legendlabel= "Trend Cycle Component ";
	yaxis label='Seasonal and Irregular Components';
	y2axis label='Trend Cycle Components';
	keylegend "Volumea" "Volumeb" "Volumec" / across=3 noborder position=bottomleft location=outside;
run;

title;

/****************Program 3.2****************/
/*Using HP Filter in PROC Expand*/
proc expand data=rspx_monthly out=frspx
	method=none;
	id date;
	convert volume=volume_t/transformout=(hp_t 14400);/*HP Filter for Trend component Lambda=14400*/
	convert volume=volume_c/transformout=(hp_c 14400);/*HP Filter for Cycle component lambda=14400*/
run;

/*Plot Time Series Component Using SGPLOT*/
proc sgplot data=frspx;
	title 'Trend and Cycle Components of S&P 500 Trading Volume ';
	series	 x=date y=volume_t/ name="Volumed" lineattrs=(color=black thickness=2) legendlabel= " Trend Component";
	Series  x=date y=volume_c / name="Volumee" 
		lineattrs=(color=green) legendlabel= " Cycle Component";
	series x=date y=volume /name="Volumef"  lineattrs=(color=blue) legendlabel= "Volume ";
	yaxis label='Trend and Cycle Components';
	keylegend "Volumed" "Volumee" "Volumef" / across=3 noborder position=bottomleft location=outside;
run;

title;

/****************Program 3.3****************/
%datapull(trade,trade.sas7bdat);
ods graphics on;

/*Using X13 to Adjust for Seasonality in Retail Sales*/
ods output D8A=MRTSAG_D8;
ods output D10=MRTSAG_D10;

proc x13 data=trade date=date seasons=12 interval = month;
	var MRTSAG;
	transform power=0;
	arima model=((0,1,1)(0,1,1) );/*ARIMA with Exponential Smoothing*/
	estimate;
	x11;
	output out=adjtrade a1 d8 d10 d11;

	/* a1 =original series| d8=Seasonality tests| d10=Seasonal adjustment Factor| d10d= Seasonal Difference| d11=seasonally adjusted series */
run;

data stest;
	set MRTSAG_D8 (where=(cvalue1 ^='') keep=label1 cvalue1 firstobs=2);
	rename label1 = 'Seasonality Tests'n;
	rename cvalue1 = 'Probability Level'n;
run;

proc print data= stest noobs;
run;


/****************Program 3.4****************/
ods graphics on;
/* Using PROC SGPLOT to Display Seasonally Adjusted Retail Sales Data*/

proc sgplot data=adjtrade;
	title 'Seasonality Adjusted Monthly US Retail Sales ';
	title2  '(Millions of Dollars)';
series x=date y=mrtsag_d11/ name="salesa" lineattrs=(color=blue) legendlabel= " Seasonality Adjusted Retail Sales";
	series  x=date y=mrtsag_a1 / name="salesb" 
	lineattrs=(color=Green) legendlabel= " Unadjusted Retail Sales";
xaxis label = 'Date' values=('01jan10'd to '01nov22'd by month) valuesformat=monyy.;
	yaxis label='US Retail Sales';
keylegend "salesa" "salesb"/ across=3 noborder position=bottomleft location=outside;
run;
title;
ods graphics off;

/****************Program 3.5****************/
%datapull(rspx,rspx_monthly .sas7bdat);
ods graphics on;

/*Visualizing Autocorrelations in Volume using AUTOREG*/
proc autoreg data= rspx_monthly  plots= all;
	model volume= / nlag=1 DW=1 dwprob;
run;

ods graphics off;

/****************Program 3.6****************/
/*Using AUTOREG TO test for Heteroscedasticity*/
%datapull(bonds,corporate_bonds.sas7bdat);
ods graphics on;

proc autoreg data= corporate_bonds(where=(date>'31dec12'd)) plots=all;
	model raaap= /  archtest=(qlm);	/*Q&LM-Test for Heteroscedasticity*/
	model raaap= /  archtest=(wl,lk);/*wl-Wong and Li |LK - Lee and King*/
	output out=pred residual=resid   predicted=rhat;
run;

quit;

ods graphics off;

/****************Program 3.7****************/
/* Using Generalized Least Square and HAC Errors*/
ods graphics on;

proc autoreg data= corporate_bonds (where=(date>'31dec12'd)) plots=all;
	model raaap=/  	/*AAA Bond Indices*/

	method=yw 		/*Yule-Walker is a type of Generalized Least Square estimator*/
	covest=Neweywest ;	/*Heteroscedasticity and Autocorrelation Robust Errors*/
	output out=pred residual=resid   predicted=rhat;
run;

quit;

ods graphics off;

/****************Program 3.8****************/
/* Modeling Conditional Variances Using GARCH Specification*/
ods graphics on;

proc autoreg data= corporate_bonds (where=(date>'31dec12'd)) plots=all;
	model raaap=/   	/*AAA Bond Indices*/
	garch=(p=1,q=1);
	output out=pred residual=resid  ht=ht  predicted=rhat;
run;

quit;

/*Plot conditional Variance Using SGPLOT*/
proc sgplot data=pred;
	title 'Conditional Variance Plot ';
	Series x=Date y=ht/;
	Xaxis label = 'Date';
	yaxis label='Conditional Volatility';
run;

title;
ods graphics off;

/****************Program 3.9****************/
/*Transforming US Real GDP Series*/
%datapull(gdp,gdp.sas7bdat);

data GDP;
	set gdp (where=(date>'31dec1979'd));
	format date year4.;
	y= log(rgdp);
	dy = dif(y);
	Label Y='Log Real GDP';
	Label DY = 'Change in Log Real GDP';
	t=_n_;
run;

/*Testing for Stationarity in US Real GDP*/
proc autoreg data= gdp;
	model y= /  				/*Log US Real GDP*/
	stationarity=(adf=(1 2 3),pp=(1 2 ));

	/*adf-Augmented Dickey-Fuller //pp-Phillips-Perron*/
run;

quit;

/****************Program 3.10****************/
/*Creating Stationary Series Using PROC ARIMA*/
ods graphics on;

proc arima data=gdp
	plots=all;
	identify var=y(0,1,0) scan stationarity=(pp=3);
	identify var=y(0,1,0)scan stationarity=(adf=3);
run;

quit;

ods graphics off;

/****************Program 3.11****************/
/*Assessing Normality in Quarterly Changes in US Real GDP*/
proc capability data=gdp normaltest;
	Var dy;
	qqplot/ normal(mu=est sigma=est color=blue);
	ppplot / normal (mu=est sigma=est color=blue);
run;

title;

/****************Program 3.12****************/
/*Examining Jumps in S&P 500 Index Returns*/
%datapull(jumps,jumps.sas7bdat);

proc gam data=jumps plots=components(clm);
	class month;
	model SPC=param(month) / dist=Poisson;
	output out=est p;
run;

proc sort data=est;
	by month;
run;

/*Plot conditional Jumps Using SGPLOT*/
proc sgplot data=est;
	title ' Days Per Month with 2% Jumps in the S&P 500 Index';
	Scatter x=month_name y=spc/ name="spca" markerattrs=(color=Green) legendlabel= " Actual Jumps";
	Series x=month_name y=p_spc / name="spcb" lineattrs=graphfit legendlabel= " Predicted Jumps";
	Xaxis label = 'Month';
	Yaxis label='Number of Days with Jumps';
	keylegend "spca" "spcb"/ across=3 noborder position=bottomleft location=outside;
run;