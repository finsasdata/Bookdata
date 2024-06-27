/*****************************************/
/* Financial Data Science with SAS       */
/* SAS Codes for Chapter Four Examples   */
/*****************************************/

/*Run the datapull macro below (if you have not already done so) before running the remaining programs.

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


/******************Program 2.1****************/
/*Reading JSON File into SAS*/
filename compinfo "%sysfunc(getoption(WORK))/company_tickers_exchange.json";
filename myjmap "%sysfunc(getoption(WORK))/myjmap.json";

proc http 
	url="https://github.com/finsasdata/Bookdata/raw/main/company_tickers_exchange.json"
	out=compinfo
	method ="get";
run;

proc http 
	url="https://github.com/finsasdata/Bookdata/raw/main/myjmap.json"
	out=  myjmap
	method ="get";
run;

libname compinfo JSON  map="%sysfunc(getoption(WORK))/myjmap.json" automap=reuse;

/*Printing the first 5 observations */
/*options nodate pageno=1 linesize=80 pagesize=30 obs=5;*/
proc print data=COMPINFO.DATA (obs=5);
	title 'Stock Listings by Exchange';
	var cik Ticker Name Exchange;
run;

title;

/******************Program 2.2****************/
/*SAS Macro to pull data from GitHub Repository*/
%macro datapull(fref,pname);
	filename  &fref "%sysfunc(getoption(WORK))/&pname";

	proc http
		url="https://github.com/finsasdata/Bookdata/raw/main/&pname"
		out=&fref
		method ="get";
	run;

%mend datapull;

/******************Program 2.3****************/
%datapull(ecod,ecodata.sas7bdat);

/*Accumulating Data into Quarterly Frequency*/
proc timedata data=ecodata out=necodata;
	id date interval=qtr
		accumulate=mean;
	var SPX INJCJC MBAVCHNG ETSLTOTL USURTOT GDP CPI;
run;

proc print data=necodata;
run;

/*2.3B*/
%datapull(ecod,ecodata.sas7bdat);

/*Time-Stamping Data into Quarterly Frequency*/
proc timedata data=ecodata out=necodata;
	id date interval=qtr
		accumulate=last;
	var SPX INJCJC MBAVCHNG ETSLTOTL USURTOT GDP CPI;
run;

Proc Print data=necodata;
run;

/******************Program 2.4****************/
data bitcoin;
	input Date mmddyy10. Bitcoin;
	format Date mmddyy10. Bitcoin dollar10.;
	datalines;
12/16/2022	16837.95
12/17/2022	16718.5
12/18/2022	16753.05
12/19/2022	16586.82
12/20/2022	16881.54
12/21/2022	16793.1
12/22/2022	16792.25
12/23/2022	16811.33
12/24/2022	16829.82
12/25/2022	16830.25
12/26/2022	16833.01
12/27/2022	16692.55
;
run;

proc sort data=bitcoin;
	by Date;
run;

data spx;
	input Date mmddyy10. Spx;
	format Date mmddyy10. Spx dollar10.;
	datalines;
12/16/2022	3852.36
12/19/2022	3817.66
12/20/2022	3821.62
12/21/2022	3878.44
12/22/2022	3822.39
12/23/2022	3844.82
12/27/2022	3829.25
;
run;

proc sort data=spx;
	by Date;
run;

data bitspx;
	merge bitcoin spx;
	by Date;
run;

proc print data=bitspx;
run;

data crypto_stock;
	merge Bitcoin (in=a) Spx(in=b);
	by date;

	if b and  a;
run;

proc print data=crypto_stock;
run;

/******************Program 2.6****************/
/*Populating the missing values using lagged values*/
data nbitspx;
	set bitspx;
	retain _spx_;

	if not missing(spx) then
		_spx_=spx;
	else Spx=_spx_;
	drop _spx_;
run;

/******************Program 2.6****************/
/*simulate monthly returns for 4 stocks*/
data simult;
	format date date.;
	array ret[4] ret1-ret4;
	keep date ret1-ret4;
	call streaminit(1);
	initdate = '31dec2020'd;
	dt=0;

	do i = 1 to 24;
		dt+1;

		do k = 1 to 4;
			date=	intnx('month',initdate,dt,'end');
			ret[k] = rand('normal');
		end;

		output;
	end;
run;

/*Simulate Random Missing Values*/
data Randsimul(drop=i);
	call streaminit(1234);
	set work.simult;
	array x {*} _numeric_;

	do i = 2 to dim(x); /* set i=2 to skip date column*/
		if rand("Bern", 0.2) then   /*Random Binary Value*/
			x[i]=.;
	end;
run;

proc print data=randsimul(obs=10);
run;

/*Inferring Missing Values using PROC MI*/
proc mi data=randsimul nimpute=25 minimum=-10 maximum=10 out=imfsimul;
	mcmc start=value   plots=all;
	var Ret1 Ret2 Ret3 Ret4;
run;

proc print data=imfSimul (obs=10);
run;

/******************Program 2.7****************/
%datapull(vol,rspx_monthly.sas7bdat);

proc capability data=rspx_monthly noprint;
	title 'Outlier Detection Plots for Monthly S&P 500 Trading Volume';
	Var volume;
	qqplot/ normal(mu=est sigma=est color=blue );
	ppplot / normal (mu=est sigma=est color=blue);
	format Volume Best8.;
run;

/******************Program 2.8****************/
/*Automatic Outlier Detection and Correction Using PROC X13*/
proc x13 data=rspx_monthly  date=Date INTERVAL = month;
	var volume;
	transform function=log;
	arima model=( (0,1,1)(0,1,1) );/*ARIMA with Exponential Smoothing*/
	outlier CV=3.6;
	estimate;
	x11;

	/*a1 is the preadjusted timeseries,e1 is the outlier adjusted series*/
	output out=arspx_monthly a1 e1;
run;

/******************Program 2.9****************/
proc sgplot data=arspx_monthly;
	title 'Automatic Outlier Detection and Correction ';
	series x=date y=volume_A1 / name="Preadjusted Volume" markers
		markerattrs=(color=red symbol='circle')
		lineattrs=(color=red) legendlabel="Preadjusted Volume";
	series x=date y=volume_E1 / name="Adjusted Volume" markers
		markerattrs=(color=blue symbol='asterisk')
		lineattrs=(color=blue) legendlabel= " Outlier Adjusted Volume";
	yaxis label='Original and Outlier Adjusted S&P 500 Trading Volume';
	keylegend "Preadjusted Volume" "Adjusted Volume" / across=3 noborder position=bottomRight location=inside;
run;

title;

/******************Program 2.10****************/
/*Simulating One year of 5 minutes Returns for Bitcoin*/
data Bitcoin;
	format Date datetime.;
	length Bitcoin 8;
	call streaminit(1);
	InitDate = '1Jan2021:00:00'dt;
	dt=0;

	do i = 1 to 105120;/*Number of 5 mins Interval in ayear*/
		dt+5;
		Date=	intnx('minutes',InitDate,dt);
		Bitcoin = rand('normal');
		output;
	end;
run;

/*Accumulating Returns into Hourly, Last Observation*/
proc timedata data=Bitcoin out=mseries plot=all;
	id date interval=hour /*Options include Hour,Day, Week, Month, Qtr, Year*/
	accumulate=last; /*Option include first, mean, median, last,*/
	var Bitcoin;
run;

/******************Program 2.11****************/
/*Using DATAPULL Macro to download portfolio data from Git*/
%datapull(pprice,portfolio_prices.sas7bdat);
%datapull(pattrib,portfolio_attrib.sas7bdat);
%datapull(bprice,benchmark_prices.sas7bdat);
%datapull(battrib,benchmark_attrib.sas7bdat);

/*Merging attributes and price and calculating monthly returns */
data PortA;
	format Price dollar13.2 MHoldings dollar16.2 
		Mcap dollar32.;
	merge Portfolio_Attrib  Portfolio_Prices;
	by Ticker;
	MHoldings = price*quantity;/*Monthly Holdings for each stock*/
run;

proc sort data= PortA;
	By Date;
run;

/*Calculating Total Monthly Portfolio Holdings and Returns*/
proc sql;
	create table portb as select * from porta;
	create table Totalhold as select date, 
		sum(mholdings) as Totalholdings /*Monthly portfolio values*/
	format=dollar16.2 from porta group by date;
quit;

data Totalhold;
	set Totalhold;

	/*Calculating monthly portfolio returns-PReturns*/
	PReturns =log(Totalholdings/lag(Totalholdings));
	format PReturns Percent8.2;
	label PReturns ='Portfolio Returns';
run;

/******************Program 2.12****************/

/*Calculating Monthly Weights for Stocks in the Portfolio
by merging Totalhold and Portb*/
proc sql;
	create table Portfolio_Metrics as select * from  Totalhold right join portb
		on totalhold.date=portb.date;

	/*Monthly Weights calculated as Monthly Holdings(per stock)/Total Holdings (per month)*/
	alter table Portfolio_Metrics add Weights num label='Portfolio Weights';
	update Portfolio_Metrics set Weights= (Mholdings/TotalHoldings);
quit;

/******************Program 2.13****************/
data MBMPort;
	format Date monyy.;
	merge TotalHold Benchmark_Prices;
	EXPort = PReturns - TBY;
	EXRUA = RRUA -TBY;/*TBY-Monthy Yield on 10Yr Treasury Bond*/
	label EXPort ='Excess Portfolio Return';
	label EXRUA ='Excess Benchmark Return';
	by Date;
run;

proc means data=MBMPort(where=(date>'31Dec20'D)) mean stddev skew kurt;
	var PReturns RRUA EXPORT EXRUA;
run;

/******************Program 2.14****************/
/* Graphing Monthly Portfolio Returns and Value*/
proc template;
	define statgraph barline;
		begingraph;
			entrytitle "Portfolio Performance in 2021";
			layout overlay / 
				xaxisopts=(label="Month" timeopts=(tickvalueformat=monname3.))
				yaxisopts=(label="Values (Millions)" offsetmin=0
				linearopts=(tickvaluesequence=(start=0 end=80 increment=20)
				tickvaluepriority=true))
				y2axisopts=(label="Returns" offsetmin=0
				linearopts=(viewmin=-.1 viewmax=.1));
				barchart category=date response=eval(Totalholdings/1000000) /
					name="bar" legendlabel="Portfolio Values" fillattrs=(transparency=0.6 color=bip);
				linechart category=date response=PReturns / 
					legendlabel="Portfolio Returns" vertexlabel=true
					vertexlabelattrs=(color=darkblue weight=bold)
					name="line" stat=sum yaxis=y2 display=(line markers)
					markerattrs=(symbol=circlefilled color=darkblue)
					lineattrs=(color=darkblue);
				discretelegend "bar" "line";
			endlayout;
		endgraph;
	end;
run;

proc sgrender data=totalhold(where=(date>'31dec20'd))  template=barline;
	format date monname3. PReturns percent6.2;
run;

/******************Program 2.15****************/
/*Comparing the Monthly Performance of the Portfolio to the Benchmark*/
proc sgplot data=MBMPort(where=(date>'31dec20'd));
	title "Monthly Portfolio and Bechmark Performances in 2021";
	styleattrs datacolors=(olive purple);
	vbar date/response=PReturns /*Portfolio Returns*/
	dataskin=pressed barwidth=0.6 
		baselineattrs=(thickness=0)
		discreteoffset=-0.1;
	vbar date/response=RRUA /*Benchmark Returns*/
	dataskin=pressed barwidth=0.6 
		baselineattrs=(thickness=0)
		discreteoffset= 0.1;
	xaxis label='Date' valuesrotate=vertical;
	yaxis display=(noline) grid;
run;

title;

/******************Program 2.16****************/
/*Using Pie Chart to Display Portfolio Attributes*/
proc template;
	define statgraph pietemp;
		begingraph;
			entrytitle "Portfolio Sector Weights";
			layout region;
				piechart category=Sector / stat=pct datalabellocation=outside	 othersliceopts=(percent=1.5);
			endlayout;
		endgraph;
	end;
run;

proc sgrender data=Portfolio_Attrib template=pietemp;
run;

/****************Program 2.17****************/
/*Using Heatmap to Display Portfolio Attribute*/
proc sgplot data=Portfolio_Attrib;
	title 'Sector-Level Portfolio Characteristics';
	heatmap x=mcapc y=sector/ colorresponse=pe colorstat=mean weight=weights;
run;

/***************Program 2.18****************/
proc template;
	define statgraph barchart;
		begingraph/border=false
			datacolors = (pink plum paleturquoise)
			datacontrastcolors = (black);
		entrytitle "Portfolio Factor Exposure By Capitalizations";
		layout overlay;
			barchart category=mcapc  response=pb / name="pbar"
				stat=mean orient=vertical dataskin=sheen
				colorbyfreq=true  colorstat=pct colormodel=datacolors;
			continuouslegend "pbar" / 
				title="Market Capitalization Allocations";
			endlayout;
				endgraph;
	end;

proc sgrender data=Portfolio_Attrib template=barchart;
run;

/****************Program 2.19****************/
/*Using Stacked Column Chart to Display Portfolio Attributes*/
proc sgplot data=Portfolio_Attrib;
	title 'Performance Attribution by Sectors and Market Capitalizations';
	hbar sector /
		response=returns weight=weights stat=mean group=mcapc 
		dataskin=pressed;
run;

title;

/****************Program 2.20****************/
/*Using Stacked Column Chart to Display Portfolio Income*/
proc template;
	define statgraph barchart2;
		begingraph;
			entrytitle "Portfolio Income By Sectors";
			layout overlay;
				barchart category=Sector  response=income / name="pdisplay"
					stat=sum display=all orient=horizontal group=mcapc;
				discretelegend "pdisplay";
			endlayout;
		endgraph;
	end;

proc sgrender data=Portfolio_Attrib template=barchart2;
run;

/****************Program 2.21****************/
/*Using Scatterplot to Display Reward to Risk*/
proc sgplot data=Portfolio_Attrib;
	title ' Portfolio Reward-to-Risk';
	scatter x=beta y=returns/;
	reg x=beta y=returns/ CLM  CLI='Prediction' alpha=.05;
	Xaxis label = 'Beta' values=(0 to 2 by 0.2);
	yaxis label ='Returns'  valuesformat=percent12.2;
	keylegend / location=inside position=bottomright;
run;

title;

/****************Program 2.22****************/
proc sgplot data=Portfolio_Attrib;
	title ' Performance Dispersion by Sector';
	hbox returns/category=sector dataskin=matte fillattrs=(color='Aquamarine');
run;

title;

/****************Program 2.23****************/
/*Using Geomaps to Display Geographic Portfolio Concentration*/
%datapull(plot,plot_data.sas7bdat);
%datapull(states,states.sas7bdat);

proc sgmap mapdata=states     /* Map boundaries */
	maprespdata= plot_data
		plotdata=plot_data /* location data */
	;
	Title 'Portfolio Concentration By State';
	openstreetmap;
	choromap  count / mapid=state density=2
		name='choro';
	text x=long y=lat text=statename /textattrs=(size=9pt);
	gradlegend 'choro'/title='Number of Companies in Each State' 
		extractscale;
run;

/****************Program 2.24****************/
/*********Abbreviated Program*****************/
/*Generating Portfolio Performance Attribute Report*/
%datapull(attrib,performance_attrib.sas7bdat);

proc print data=Performance_Attrib noobs label 
	style(header)={just=c fontsize=1.8} style(data)={just=c fontsize=1.8} style(grandtot)={just=c fontweight=bold fontsize=1.8} grandtot_Label='Total';
	title ' Portfolio Performance Attribution';
	variables Sector/style(data)={just=l fontweight=bold width=1.8in};
	var BGWeights Returns Sector_P Index_Weights  Index_Returns Sector_I Sector_T Sector_Alpha  
		PAllocation  PSelection  PInteraction;
	sum BGWeights Index_Weights Sector_P Sector_I PAllocation PSelection Pinteraction;
	footnote H=0.5  'Benchmark is the Russell 3000 Index | Sector tilt = Portfolio overweighting(underweighting) of each sector relative to the index | Sector Alpha = Portfolio Outperformance(underperformance) of each sector relative to the index';
run;

title;
footnote;

/*****************************************************************************************/

/**********Full Program**************************
/*Generating Beginning and Ending Values of Portfolio Components*/
proc sql;
	create table PortR as select * from Portfolio_Metrics
		group by ticker having date = min(date) or date=max(date)
	;
quit;

proc sort data=PortR;
	by ticker date;
run;

data PortRR;
	set PortR;
	by ticker;
	retain tempweight weights;
	Period=2;

	if first.ticker then do;

		/* Assign BGWeights to Annual Sector Returns*/
		tempWeight= weights;
		Period = Period -1;
	end;

	BGWeights = tempweight;
	Label BGWeights = 'Beginning Weights';
	Label Weights = 'Ending Weights';
run;

/*Reporting Sector-Level Portfolio Performance*/
/*Sum Attributes that Are Weighted Separately Those that are not*/
proc tabulate data=PortRR(where=(period=2)) 
	OUT=WORK.DSTATSONE(LABEL="Portfolio Statistics"
	Drop=_Type_ _Page_ _Table_
	rename=(BGweights_sum=BGWeights weights_sum=EDWeights Quantity_sum=Quantity Mholdings_sum=Holdings Income_sum=Income));
	Var  BGWeights Weights Quantity MHoldings Income ; /*Rename Output*/
	class Date;
	class sector;
	Table Date*sector* sum, BGWeights*F=percent8.2 Weights*F=percent8.2  Quantity MHoldings*F=dollar16.2  Income*F=dollar16.2;
run;

proc tabulate data=PortRR(where=(period=2)) 
	OUT=WORK.DSTATSTWO(
	LABEL="Portfolio Statistics" 
	Drop=_Type_ _Page_ _Table_
	rename=(returns_mean=Returns beta_mean=Beta pb_mean=PB pe_mean=PE)
	label=data);
	Var  returns pb pe beta;
	class Date;
	class sector;

	Table Date*sector*mean, returns*F=percent8.2  beta pb pe;
		weight bgweights;
run;

proc sort data=Benchmark_Attrib out=BMAttrib;
	by sector;
run;

/*Joining Dstats one and two with BMAttrib*/
proc sql;
	create table Mergedstats as select * from dstatsone left join dstatstwo
		on dstatsone.sector=dstatstwo.sector left join BMAttrib on dstatstwo.sector=BMAttrib.sector;
quit;

proc print data=mergedstats(drop=date) label;
run;

/*Performance Attributions*
Formatting the variables in the Performance Attribution dataset*/
data Performance_Attrib;
	Set Mergedstats(Keep=Sector BGWeights EDWeights Returns Index_weights Index_Returns);
	Sector_P=BGWeights*Returns;
	Sector_I=Index_Weights*Index_Returns;
	Sector_T=BGWeights-Index_Weights;
	Sector_Alpha = Returns-Index_Returns;
	PAllocation = Sector_T*Index_Returns;
	PSelection = Sector_Alpha*Index_Weights;
	PInteraction = Sector_T*Sector_Alpha;
	Label 
		BGWeights='Beginning Weights'
		EDWeights='Ending Weights'
		Sector_P ='Sector Contribution (Portfolio)'
		Sector_I ='Sector Contribution (Benchmark)'
		Sector_T ='Sector Tilt'
		Sector_Alpha = 'Sector Alpha'
		PAllocation = 'Allocations'
		PSelection = 'Selection '
		PInteraction = 'Interaction';
	format 
		BGWeights Percent8.2 
		EDWeights Percent8.2 
		Returns Percent8.2 
		Sector $char23.
		Index_weights Percent8.2 
		Index_Returns Percent8.2 
		Sector_P Percent8.2 
		Sector_I Percent8.2 
		Sector_T Percent8.2 
		Sector_Alpha Percent8.2 
		PAllocation Percent8.2 
		PSelection Percent8.2 
		PInteraction Percent8.2;
run;

/*Generating Portfolio Performance Attribute Report*/
proc print data=Performance_Attrib noobs label style(header)={just=c width =1.2in fontsize=3.20} style(data)={just=c  fontsize=3.50} style(grandtot)={just=c fontweight=bold fontsize=3.50}  grandtot_Label='Total';
	title ' Portfolio Performance Attribution';
	variables Sector/style(data)={just=l fontweight=bold width=2.2in};
	var BGWeights Returns Sector_P Index_Weights  Index_Returns Sector_I Sector_T Sector_Alpha  
		PAllocation  PSelection  PInteraction;
	sum BGWeights Index_Weights Sector_P Sector_I PAllocation PSelection Pinteraction;
	footnote H=0.2in 'Benchmark is the Russell 3000 Index | Sector tilt = Portfolio overweighting(underweighting) of each sector relative to the index |Sector Alpha = Portfolio Outperformance(underperformance) of each sector relative to the index';
run;

title;
footnote;
