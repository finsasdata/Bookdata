/*****************************************/
/* Financial Data Science with SAS       */
/* SAS Codes for Chapter Nine Examples   */
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



/*********Program 9.1**************************************/
/*Maximizing Portfolio Return in the Mean-Variance Optimization using Proc OPTMODEL*/
%datapull(portret,portfolio_returns.sas7bdat);
%datapull(portvcv,portfolio_covariances.sas7bdat);
%let max_risk = 0.045;
%let alpha=0.99;
%let rfr=0.00178;

proc optmodel;
	/* declare sets and parameters */
	set <str> ASSETS;
	num returns {ASSETS};
	num covariance {ASSETS, ASSETS};

	/* read portfolio data from SAS data sets */
	read data Portfolio_Returns into ASSETS=[Ticker] returns;

	*print returns;
	read data Portfolio_Covariances into [Ticker]  
		{j in ASSETS} <covariance[Ticker,j]=col(j)>;

	*print covariance;
	/* declare variables */
	var Weights {ASSETS} >= 0;

	/*Calculate Portfolio Risk Measures*/
	impvar Portfolio_Variance=sum {i in ASSETS, j in ASSETS}covariance[i,j]*Weights[i]*Weights[j];
	impvar Portfolio_Risk = sqrt(Portfolio_Variance);

	/*Included Code: Value at Risk and Expected Shortfall*/
	impvar zes =(((2*constant("pi"))**(0.5))*(exp(((2**-0.5)*quantile('normal',&alpha))**2))*(1-&alpha))**-1;
	impvar VaR=sum {j in ASSETS} 
		-returns[j]*Weights[j]+quantile("normal",&alpha)*sqrt(Portfolio_Variance);
	impvar Expected_Shortfall=sum {j in ASSETS} 
		-returns[j]*Weights[j]+zes*sqrt(Portfolio_Variance);

	/*Calculate portfolio return*/
	impvar Expected_Return=sum {j in ASSETS} 
		returns[j] * Weights[j];

	/* declare constraints */
	con Portfolio_Weights: sum {j in ASSETS} Weights[j] = 1;
	con Variance: Portfolio_Variance <= &max_risk**2;

	/* declare objective */
	max Portfolio_Return = Expected_Return;
	solve ;

	/*Portfolio Sharpe Ratio*/
	Impvar Sharpe_Ratio = (Expected_Return-&rfr)/Portfolio_Risk;

	/*Print Optimization Outputs*/
	print  Portfolio_Return percent8.2 Portfolio_Risk percent8.2 Sharpe_Ratio best8.5 VaR percent8.2 Expected_Shortfall percent8.2;
	print Variance.ub Variance.dual;
	print {j in ASSETS: Weights[j]>1e-4} Weights percent8.2
		{j in ASSETS: Weights[j]>1e-4} returns percent8.2;

	/* write data to SAS data sets */
	create data mvmax from [Ticker]={j in ASSETS: Weights[j]>1e-4}
		Weights=Weights;
quit;

proc sgpie data=mvmax;
	title1 font=swissb height=2 'Asset Allocations Using Mean-Variance Portfolio Optimization';
	title2 font=swissb height=2 '(Return Maximization)';
	pie  Ticker / response=Weights otherpercent=1  statfmt=percent8.2   datalabelloc=callout;
run;

title1;
title2;
quit;



/*********Program 9.2**************************************/
/*Minimizing Portfolio Risk in the Mean-Variance Optimization using Proc OPTMODEL*/
%let minimum_return = 0.0179;
%let alpha=0.99;
%let rfr=0.00178;


proc optmodel;
	/* declare sets and parameters */
	set <str> ASSETS;
	set <str> VIEWS;
	num returns {ASSETS};
	num covariance {ASSETS, ASSETS};

	/* read portfolio data from SAS data sets */
	read data Portfolio_Returns into ASSETS=[Ticker] returns;

	*print returns;
	read data Portfolio_Covariances into [Ticker]  
		{j in ASSETS} <covariance[Ticker,j]=col(j)>;

	*print covariance;
	/* declare variables */
	var Weights {ASSETS} >= 0;

	/*Calculate Portfolio Risk Measures*/
	impvar Portfolio_Variance=sum {i in ASSETS, j in ASSETS}covariance[i,j] * Weights[i] * Weights[j];


	/*Included Codes: Value at Risk and Expected Shortfall*/
	impvar zes =(((2*constant("pi"))**(0.5))*(exp(((2**-0.5)*quantile('normal',&alpha))**2))*(1-&alpha))**-1;
	impvar VaR=sum {j in ASSETS} 
		-returns[j] * Weights[j]+quantile("normal",&alpha)*sqrt(Portfolio_Variance);
	impvar Expected_Shortfall=sum {j in ASSETS} 
		-returns[j] * Weights[j]+zes*sqrt(Portfolio_Variance);

	/*Calculate portfolio return*/
	impvar Expected_Return=sum {j in ASSETS} 
		returns[j] * Weights[j];
	impvar Portfolio_Return = Expected_Return;


	/* declare constraints */
	con Portfolio_Weights: sum {j in ASSETS} Weights[j] = 1;
	con Minimum_return: Expected_Return >= &minimum_return;

	/* declare objective */
	min Portfolio_Risk = sqrt(Portfolio_Variance);
	solve ;
	
	/*Portfolio Sharpe Ratio*/
	Impvar Sharpe_Ratio = (Expected_Return-&rfr)/Portfolio_Risk;

	/*Print Optimization Outputs*/
	print  Portfolio_Return percent8.2 Portfolio_Risk percent8.2 Sharpe_Ratio best4.2 VaR percent8.2 Expected_Shortfall percent8.2;

	print Minimum_Return.lb Minimum_Return.dual;
	print {j in ASSETS: Weights[j]>1e-4} Weights percent8.2
		{j in ASSETS: Weights[j]>1e-4} returns percent8.2;

	/* write data to SAS data sets */
	create data mvmin from [Ticker]={j in ASSETS: Weights[j]>1e-4}
		Weights=Weights;
quit;

proc sgpie data=mvmin;
	title1 font=swissb height=2 'Asset Allocations Using Mean-Variance Portfolio Optimization';
	title2 font=swissb height=2 '(Risk Minimization)';
	pie  Ticker / response=Weights otherpercent=1  statfmt=percent8.2   datalabelloc=callout;
run;

title1;
title2;
quit;


/*********Program 9.3**************************************/
/* Use PROC IML to implement Maximum Sharpe ratio optimization*/
%let n=63;
proc iml;
	/* Read asset returns and covariances into Returns and Sigma*/
	use Portfolio_Returns;
	read all var _num_ into returns[colname=NumerNames];
	read all var _char_ into assets[colname=CharNames];

	use Portfolio_Covariances;
	read all var _num_ into sigma[colname=NumerNames rowname=Ticker];

	rfr=0.00178;/*Monthly Risk-free rate*/

	start max_sharpe(w) global(rfr,sigma,returns,portfolio_return, portfolio_risk, sharpe);
		portfolio_return=W*returns;
		portfolio_risk=sqrt(w*sigma*t(w));
		sharpe=(portfolio_return-rfr)/portfolio_risk;
		return (sharpe);
	finish;

	/*Constraint: lower bound, upper bound, sum weights equals 1)*/
	p={0,1,1};
	con=repeat(p,1,&n)||{. .,. .,0 1};;

	/* Setting the optimization to maximize the objective function and print optimization summary*/
	optn={1 1};
		/*Invoke non-linear Quasi Newton for linearly constrained problem. Set initial value set to equal Weights*/
w=j(1,&n,1/&n);
	call NLPQN(rc,OptW,"max_sharpe",w,optn,con);

/* Print Optimization Output*/
	print portfolio_return[format=percent8.2] portfolio_risk[format=percent8.2] sharpe[format=best4.2] ;

	TOptW=t(OptW);
	ind = loc(TOptw>0);

	if ncol(ind)>0 then
		rn=NumerNames[ind];
		Weights=TOptW[ind];
	print Weights[format=percent8.2 l='Portfolio Allocations' colname='Ticker' rowname=rn ];
	cnames='Ticker'||'Weights';
	create Sharpemax from rn Weights[colname=cnames];
	append from rn Weights;
	close sharpemax;

proc sgpie data=sharpemax;
title1 font=swissb height=2 'Asset Allocations from Mean Variance Portfolio Optimization';
title2 font=swissb height=2 '(Sharpe Ratio  Maximization)';
	pie  ticker / response=Weights otherpercent=1  statfmt=percent8.2   datalabelloc=callout;
run;



/*********Program 9.4A**************************************/
/*Creating Efficient Frontier and Capital Allocation Line in SAS*/
%let rfr=0.00178; /*Risk-free rate*/
%let Max_Sharpe=0.36; /*Sharpe ratio*/
%let m =100;/*Number of portfolio to optimized and simulate*/
%let r =63;/*Number of current stocks in portfolio*/

%macro EFsim(nrep); /*spccify number of reps*/
	proc datasets nodetails nolist;
		delete simfrontier;	run;

	%local i nrep; /*local macro variable*/
	%do i=1 %to &nrep;

		proc optmodel printlevel=1;
			/* declare sets and parameters */
			set <str> ASSETS;
			num returns {ASSETS};
			num covariance {ASSETS, ASSETS};
		/* read portfolio data from SAS  datasets */
read data Portfolio_Returns into ASSETS=[Ticker] returns;
			read data Portfolio_Covariances into [Ticker]  
			{j in ASSETS} <covariance[Ticker,j]=col(j)>;

			/* declare variables */
			var Weights {ASSETS} >= 0;
			impvar max_risk=&i*.001+.033;

			/*Calculate Portfolio Risk Measures*/
impvar Portfolio_Variance=sum {i in ASSETS, j in ASSETS}covariance[i,j]*Weights[i]*Weights[j];
			impvar Portfolio_Risk = sqrt(Portfolio_Variance);

						/*Calculate portfolio return*/
			impvar Expected_Return=sum {j in ASSETS} 
				returns[j] * Weights[j];

			/* declare constraints */
			con Portfolio_Weights:sum{j in ASSETS}Weights[j]=1;
			con Variance: Portfolio_Variance = max_risk**2;

			/* declare objective */
			max Portfolio_Return = Expected_Return;
			solve;

			/*Portfolio Sharpe Ratio*/
Impvar Sharpe_Ratio = (Expected_Return-&rfr)/Portfolio_Risk;

			/*Print Optimization Outputs*/
print  Portfolio_Return percent8.2 Portfolio_Risk percent8.2 Sharpe_Ratio best4.2;
print {j in ASSETS: Weights[j]>1e-4} Weights percent8.2	{j in ASSETS: Weights[j]>1e-4} returns percent8.2;

			/* write data to SAS  datasets */
create data smvmax from index=&i Returns=Portfolio_return Risks=Portfolio_Risk Sharpe=Sharpe_Ratio;
			quit;

		/*updated table with results from new iteration*/
		proc append base=simfrontier data=smvmax force;
		run;

	%end;
%mend;
%efsim(&m);
	
	/*********Program 9.4B**************************************/
/*Simulating random portfolio weights using PROC IML*/
proc iml;
	use Portfolio_Returns;
	read all var _num_ into returns[colname=NumerNames];
	read all var _char_ into assets[colname=CharNames];
	use Portfolio_Covariances;
	read all var _num_ into sigma[colname=NumerNames rowname=Ticker];

	/*Simulate portfolio weights*/
	x=j(&m,&r);
	call randseed(12348);
	call randgen(x,'exp');
	W=x/x[,+];

	/*Create column names*/
	pnames ='Index'||'RReturns'||'RRisks';

	/*Create vector and calculate portfolio risk and returns*/	
	portplot=j(&m,3);
	do i = 1 to &m;
		portplot[i,1]=i;
		portplot[i,2]=w[i,]*returns;
		portplot[i,3]=sqrt((w[i,])*(sigma*t(w[i,])));
	end;

	create Portplot from Portplot [colname=pnames];
	append from Portplot;
	close Portplot;


/*********Program 9.4C**************************************/
/*Creating Efficient frontier and capital allocation line using PROC SGPLOT*/
data EFrontier;
	merge Simfrontier Portplot;
	by index;
	if index=100 then do;
	Creturns=0.0109523; CRiskS =0.0474768;
	end;
run;

proc sgplot data=Efrontier;
	title "Optimal Portfolio Allocations With Sixty-Three Stock Universe";
	lineparm x=0 y=&rfr slope=&max_sharpe /lineattrs=graphdata6(thickness=4)   legendlabel='Capital Allocation Line';
	series x=Risks Y=Returns/ legendlabel='Efficient Frontier' lineattrs=graphdata1(thickness=3 color=bippk );
scatter x=RRisks Y=RReturns/legendlabel='Unoptimized Random Portfolios' markerattrs=(color=bipb symbol=circlefilled size=3 pt);
scatter x=CRisks Y=CReturns/legendlabel='Initial Portfolio' markerattrs=(color=viypk symbol=starfilled size=6 pt);

	inset "Risk Free Rate = 1.78%" "Maximum Sharpe Ratio =0.36" /  border title="Portfolio Statistics" position=topleft;;
	xaxis label='Portfolio Standard Deviation' valuesformat=percent8.2 values=(0 to 0.11 by 0.02);
	yaxis label='Portfolio Returns' valuesformat=percent8.2 values=(0 to 0.04 by 0.005);
run;
title;


/*********Program 9.5A**************************************/
/* Using PROC IML to obtain Blended Expected Returns and Covariances */

%datapull(pmatrix,pmatrix.sas7bdat);
%datapull(omega,omega.sas7bdat);
%datapull(qret,qret.sas7bdat);
%let rfr=0.00178;/*Risk-Free Rate*/
%let alpha=0.99;/*VaR Threshold*/
%let tau=0.05;/*Weight of active risk*/
%let max_risk = 0.045;

/* Use PROC IML to set reverse optimization obtain Revised Expected Returns and Covariances Forecast*/
proc iml;
	/*read subjective views (asset and returns) into vectors*/
	use PMatrix; /*asset with subjective views*/
	read  all var _NUM_ into p [colname=NumerNames rowname=Views];
	use qret; /*subjective performances*/
	read all var _all_ into q[colname=NumerNames rowname=Views];
	use omega; /*Confidence level of views*/
	read all var _all_ into omega[colname=NumerNames rowname=Views];

	*print p q;
	/* Read equilibrium returns and covariances into EQRET and SIGMA*/
	use Portfolio_Returns;
	read all var _num_ into eqret[colname=NumerNames rowname=Ticker];
	read all var _char_ into assets[colname=CharNames];/*Asset names*/

	*print eqret;
	use Portfolio_Covariances;
	read all var _num_ into sigma[colname=NumerNames rowname=Ticker];

	*print sigma;
	/*Compute revised expected returns and covariances*/
	RReturns=inv(inv(&tau*sigma)+t(p)*(inv(omega))*p)*(inv(&tau*sigma)*eqret+t(p)*(inv(omega))*q);
	RCovariances=sigma+inv(inv(&tau*sigma)+t(p)*(inv(omega))*p);
	*print RReturns[colname='Returns'];

	*print RCovariances[colname=Name rowname=Name];
	/*Export revised expected returns into SAS data sets*/
	Anames ='Ticker'||'Returns'||'EQReturns'; /*Variable names setup*/
	create Revised_Portfolio_Returns from Assets rreturns Eqret[colname=Anames];
	append from Assets RReturns Eqret;
	close Revised_Portfolio_Returns;

	/*Export revised covariance into SAS data sets*/
	sname ={'Assets'};/*Variable names setup*/
	cnames ='Ticker'||Numernames;

	*print cnames;
	create Revised_Porfolio_Covariances from Assets RCovariances[colname= cnames ];
	append from Assets RCovariances;
	close Revised_Porfolio_Covariances;



/*********Program 9.5B**************************************/
	/*Black-Litterman Portfolio optimization using PROC OPTMODEL*/
	/*Portfolio optimization*/
proc optmodel;
	/* declare sets and parameters */
	set <str> ASSETS;
	num Returns {ASSETS};
	num EQReturns {ASSETS};
	num covariance {ASSETS, ASSETS};

	/* read revised portfolio data from SAS data sets */
	read data Revised_Portfolio_Returns into ASSETS=[Ticker] returns=returns eqreturns;

	*print returns;
	read data Revised_Porfolio_Covariances into [Ticker]  
		{j in ASSETS} <covariance[Ticker,j]=col(j)>;

	*print covariance;
	/* declare variables */
	var Weights {ASSETS} >= 0;

	/*Calculate Portfolio Risk Measures*/
	impvar Portfolio_Variance=sum {i in ASSETS, j in ASSETS}covariance[i,j] * Weights[i] * Weights[j];
	impvar Portfolio_Risk = sqrt(Portfolio_Variance);

	/*included Codes Value at Risk and Expected Shortfall*/
	impvar zes =(((2*constant("pi"))**(0.5))*(exp(((2**-0.5)*quantile('normal',&alpha))**2))*(1-&alpha))**-1;
	impvar VaR=sum {j in ASSETS} 
		-returns[j] * Weights[j]+quantile("normal",&alpha)*sqrt(Portfolio_Variance);
	impvar Expected_Shortfall=sum {j in ASSETS} 
		-returns[j] * Weights[j]+zes*sqrt(Portfolio_Variance);

	/*Calculate portfolio return*/
	impvar Expected_Return=sum {j in ASSETS} 
		returns[j] * Weights[j];

	/* declare constraints */
	con Portfolio_Weights: sum {j in ASSETS} Weights[j] = 1;
	con Variance: Portfolio_Variance <= &max_risk**2;

	/* declare objective */
	max Portfolio_Return = Expected_Return;
	solve;

	/*Portfolio Sharpe Ratio*/
	Impvar Sharpe_Ratio = (Expected_Return-&rfr)/Portfolio_Risk;

	/*Print Optimization Outputs*/
	print  Portfolio_Return percent8.2 Portfolio_Risk percent8.2 Sharpe_Ratio best4.2 VaR percent8.2 Expected_Shortfall percent8.2;
	print Variance.ub Variance.dual;
	print {j in ASSETS: Weights[j]>1e-4} Weights percent8.2
		{j in ASSETS: Weights[j]>1e-4} Returns percent8.2
		{j in ASSETS: Weights[j]>1e-4} EQReturns percent8.2;

	/* write data to SAS data sets */
	create data blmax from [Name]={j in ASSETS: Weights[j]>1e-4}
		Weights=Weights;
quit;

proc sgpie data=blmax;
	title1 font=swissb height=2 'Asset Allocations from Black-Litterman Portfolio Optimization';
	title2 font=swissb height=2 '(Return Maximization)';
	pie  Name / response=Weights otherpercent=1  statfmt=percent8.2   datalabelloc=callout;
run;

title1;
title2;
quit;


/*********Program 9.6**************************************/
%let rfr=0.00178; /*Risk-Free Rate*/
%let alpha=0.99; /*VaR Threshold*/
%let minimum_return = 0.01787;/*Minimum Portfolio Return*/
%let maximum_risk=0.045; /*Maximum Standard Deviation*/


proc optmodel;
	/* declare sets and parameters */
	set <str> ASSETS;
	num returns {ASSETS};
	num covariance {ASSETS, ASSETS};

	/* read portfolio data from SAS data sets */
	read data Portfolio_Returns into ASSETS=[Ticker] returns;
	read data Portfolio_Covariances into [Ticker]  
		{j in ASSETS} <covariance[Ticker,j]=col(j)>;

	/* Declare decision variables */
	var Weights {ASSETS}>=0;

	/*Count the number of securities in portfolio*/
	impvar N=card(ASSETS);

	*print N;
	/*Calculate Portfolio Risk Measures*/
	impvar Portfolio_Variance=sum {i in ASSETS, j in ASSETS}covariance[i,j] * Weights[i] * Weights[j];
	impvar Portfolio_Risk = sqrt(Portfolio_Variance);

	/*Value at Risk and Expected Shortfall*/
	impvar zes =(((2*constant("pi"))**(0.5))*(exp(((2**-0.5)*quantile('normal',&alpha))**2))*(1-&alpha))**-1;
	impvar VaR=sum {j in ASSETS} 
		-returns[j] * Weights[j]+quantile("normal",&alpha)*sqrt(Portfolio_Variance);
	impvar Expected_Shortfall=sum {j in ASSETS} 
		-returns[j] * Weights[j]+zes*sqrt(Portfolio_Variance);

	/*Calculate risk budget, currently equal risk contribution*/
	impvar rb{j in ASSETS}=1/N;

	*print rb;
	/*Calculate relative risk contribution rrc*/
	impvar rrc{i in ASSETS} = ((sum{j in ASSETS} 
		covariance[i,j] *Weights[j])* Weights[i])/(Portfolio_Variance);

	/*Calculate portfolio return*/
	impvar Expected_Return=sum {j in ASSETS} 
		returns[j] * Weights[j];
	impvar Portfolio_Return = Expected_Return;

		/*Portfolio Sharpe Ratio*/
	Impvar Sharpe_Ratio = (Expected_Return-&rfr)/Portfolio_Risk;

	/* declare constraints */
	con Portfolio_Weights: sum {j in ASSETS} Weights[j] = 1;
	*con Sharpe: Sharpe_Ratio>=0.355;
	con Minimum_return: Portfolio_Return >= &minimum_return;
	con Maximum_Risk: Portfolio_Risk<=&maximum_risk;

	/* declare objective */
	min RiskDeviation =sum{i in Assets} (rrc[i] - rb[i])**2;
	solve;

	/*Print Optimization Outputs*/
	print rrc percent8.2 rb percent8.2;
	print  Portfolio_Return percent8.3 Portfolio_Risk percent8.2  Sharpe_Ratio best4.2 VaR percent8.2 Expected_Shortfall percent8.2;

	*print Minimum_return.ub Minimum_return.dual;
	print {j in ASSETS: Weights[j]>1e-4} Weights percent8.2
		{j in ASSETS: Weights[j]>1e-4} returns percent8.2;

	/* write data to SAS data sets */
	create data rpmin from [Ticker]={j in ASSETS: Weights[j]>1e-4}
		Weights=Weights;
quit;

proc sgpie data=rpmin;
	title1 font=swissb height=2 'Asset Allocations from Risk Parity Portfolio Optimization';
	title2 font=swissb height=2 'Minimum Risk Deviation';
	pie  Ticker / response=Weights otherpercent=1.5   statfmt=percent8.2   datalabelloc=callout;
run;

title1;
title2;
quit;


/*********Program 9.7A**************************************/
/* Use PROC IML to Simulate 1,000 Portfolio Scenarios*/
%let j =1000;
proc iml;
	/* Read returns and covariances into EQRET and SIGMA*/
	use Portfolio_Returns;
	read all var _num_ into returns [colname=NumerNames];
	read all var _char_ into assets[colname=CharNames];

	use Portfolio_Covariances;
read all var _num_ into sigma[colname=NumerNames rowname=Ticker];

	call randseed (7564321); 	/*Random Seed*/

	%let r =nrow(eqret);
	SRet =RandNormal(&j,returns,sigma);/*Simulate 1000x N-Assets*/
	SMean =mean(SRet); /*calculate sample mean*/
	SCOV = cov(SRet); /*calculate sample covariance*/

	/*Index of Scenarios*/
	SNum = t("SC1":"SC&j");

	/*Export revised expected returns into SAS  datasets*/
	Anames ='Scenario'||NumerNames; /*Variable names setup*/
	create Simulated_Returns from SNum SRet [colname=Anames];
	append from SNum SRet;
	close Simulated_Returns;


/*********Program 6.17B**************************************/
/*Implementing Conditional Value-at-Risk Optimization using PROC OPTMODEL*/
%let alpha=0.95;
%let rfr=0.00178;
%let minimum_return = 0.0179;
proc optmodel;
	/* declare sets and parameters */
	set <str> ASSETS;
	set <str> SCENARIOS;
	num returns{ASSETS};
	num sreturns {ASSETS,SCENARIOS};
	num covariance {ASSETS, ASSETS};

	/* read portfolio data from SAS data sets */
	read data Portfolio_Returns into ASSETS=[Ticker] returns;
	read data simulated_Returns into SCENARIOS=[Scenario] 
		{i in ASSETS} <sreturns[i,Scenario]=col(i)>;

	*print sreturns;
	* print return;
	read data Portfolio_Covariances into [Ticker]  
		{i in ASSETS} <covariance[Ticker,i]=col(i)>;

	* print covariance;
	/* declare decision variables */
	var Weights {ASSETS} >= 0;
	Var Opt_VaR>=0;
	Var Z{SCENARIOS}>=0;

	/*Calculate Portfolio Risk Measures*/
	impvar Portfolio_Variance=sum {i in ASSETS, j in ASSETS}covariance[i,j] * Weights[i] * Weights[j];
	impvar Portfolio_Risk = sqrt(Portfolio_Variance);

	/*Value at Risk and Expected Shortfall*/
	impvar zes =(((2*constant("pi"))**(0.5))*(exp(((2**-0.5)*quantile('normal',&alpha))**2))*(1-&alpha))**-1;
	impvar VaR=sum {i in ASSETS} 
		-returns[i] * Weights[i]+quantile("normal",&alpha)*sqrt(Portfolio_Variance);
	impvar Expected_Shortfall=sum {i in ASSETS} 
		-returns[i] * Weights[i]+zes*sqrt(Portfolio_Variance);

	/*Functions of Scenario Portfolios and Optimized VaR*/
	impvar FWVaR{s in SCENARIOS}=sum{i in ASSETS}(-Weights[i]*sreturns[i,s])-Opt_VaR;
	*impvar FXV =Opt_VaR+1/(&j*(1-&alpha))*(sum{s in SCENARIOS} Z[s]);

	/*Calculate portfolio return*/
	impvar Expected_Return=sum {i in ASSETS} 
		returns[i] * Weights[i];
	impvar Portfolio_Return = Expected_Return;

	/* declare constraints */
	con CZ{s in SCENARIOS} : Z[s]+sum {i in ASSETS} Weights[i]*sreturns[i,s]+Opt_VaR>=0;
	con SZ{s in SCENARIOS} : Z[s]>=0;
	con Minimum_Return: Expected_Return>=&minimum_return;
	con Portfolio_Weights: sum {i in ASSETS} Weights[i] = 1;

	*con Minimum_return: Expected_Return >= &minimum_return;
	/* declare objective */
	min CVAR =Opt_VaR+1/(&j*(1-&alpha))*(sum{s in SCENARIOS} Z[s]);
	solve;

	/*Portfolio Sharpe Ratio*/
	Impvar Sharpe_Ratio = (Expected_Return-&rfr)/Portfolio_Risk;

	/*Print Optimization Outputs*/
	print  Portfolio_Return percent8.2 Portfolio_Risk percent8.2 Sharpe_Ratio best4.2 CVAR percent8.2 Opt_VaR percent8.2 VaR percent8.2  Expected_Shortfall percent8.2;
	print {i in ASSETS: Weights[i]>1e-4} Weights percent8.2
	{i in ASSETS: Weights[i]>1e-4} returns percent8.2;

	/* write data to SAS data sets */
	create data cvarmin from [Ticker]={i in ASSETS: Weights[i]>1e-4}
		Weights=Weights;
quit;

proc sgpie data=cvarmin;
	title1 font=swissb height=2 'Asset Allocations Using Mean-CVAR Portfolio Optimization';
	title2 font=swissb height=2 '(CVaR Minimization)';
	pie  Ticker / response=Weights otherpercent=1  statfmt=percent8.2   datalabelloc=callout;
run;

title1;
title2;
quit;


/*********Program 9.8**************************************/
/*Robust Portfolio Optimization using Scenario Uncertainty Set*/
/* Use PROC IML to Simulate 1,000 Portfolio Scenarios*/
%let j =1000;
%let alpha=0.99;

proc iml;
	/* Read returns and covariances into RETURNS and SIGMA*/
	use Portfolio_Returns;
	read all var _num_ into returns[colname=NumerNames];
	read all var _char_ into assets[colname=CharNames];/*Asset names*/

	*print returns;
	use Portfolio_Covariances;
	read all var _num_ into sigma[colname=NumerNames rowname=Ticker];
	call randseed (7564321);

	%let r =nrow(returns);
	SRet =RandNormal(&j,returns,sigma); /*Simulate 1000x2 vector*/
	SMean =mean(SRet); /*calculate sample mean*/
	SCOV = cov(SRet); /*calculate sample covariance*/

	/*Index of Scenarios*/
	SNum = t("SC1":"SC&j");

	*print SNUM SRET;
	/*Export revised expected returns into SAS data sets*/
	Anames ='Scenario'||NumerNames; /*Variable names setup*/
	create Simulated_Returns from SNum SRet [colname=Anames];
	append from SNum SRet;
	close Simulated_Returns;


/*Estimating Robust Portfolio Optimization using Scenario Uncertainty Set*/
%let rfr=0.00178;
%let minimum_return = 0.0179;
%let max_risk = 0.045;
proc optmodel;
	/* declare sets and parameters */
	set <str> ASSETS;
	set <str> SCENARIOS;
	num returns{ASSETS};
	num sreturns {ASSETS,SCENARIOS};
	num covariance {ASSETS, ASSETS};

	/* read portfolio data from SAS data sets */
	read data Portfolio_Returns into ASSETS=[Ticker] returns;
	read data simulated_Returns into SCENARIOS=[Scenario] 
		{i in ASSETS} <sreturns[i,Scenario]=col(i)>;

	*print sreturns;
	* print return;
	read data Portfolio_Covariances into [Ticker]  
		{i in ASSETS} <covariance[Ticker,i]=col(i)>;


	/* declare decision variables */
	var Weights {ASSETS}>=0;
	Var T;

	/*Calculate Portfolio Risk Measures*/
	impvar Portfolio_Variance=sum {i in ASSETS, j in ASSETS}covariance[i,j] * Weights[i] * Weights[j];
	impvar Portfolio_Risk = sqrt(Portfolio_Variance);

	/*Value at Risk and Expected Shortfall*/
	impvar zes =(((2*constant("pi"))**(0.5))*(exp(((2**-0.5)*quantile('normal',&alpha))**2))*(1-&alpha))**-1;
	impvar VaR=sum {i in ASSETS} 
		-returns[i] * Weights[i]+quantile("normal",&alpha)*sqrt(Portfolio_Variance);
	impvar Expected_Shortfall=sum {i in ASSETS} 
		-returns[i] * Weights[i]+zes*sqrt(Portfolio_Variance);

	/*Functions of Scenario Portfolios*/
	impvar FSRet{s in SCENARIOS}=sum{i in ASSETS}(Weights[i]*sreturns[i,s]);

	*impvar SSharpe{s in SCENARIOS}=(FSRet[s]-&rfr)/portfolio_risk;
	
	/*Calculate portfolio return*/
	impvar Expected_Return=sum {i in ASSETS} 
		returns[i] * Weights[i];
	impvar Portfolio_Return = Expected_Return;

	/* declare constraints */
	con CT{s in SCENARIOS} : T-FSRet[s]<=0;
	con Maximum_Risk: Portfolio_Risk<=&max_risk;
	con Portfolio_Weights: sum {i in ASSETS} Weights[i] = 1;

	/* declare objective */
	max Z=T;
	solve;
	*print Fsret percent8.2;
	/*Portfolio Sharpe Ratio*/
	Impvar Sharpe_Ratio = (Expected_Return-&rfr)/Portfolio_Risk;

	/*Print Optimization Outputs*/
	print  Portfolio_Return percent8.2 Portfolio_Risk percent8.2 Sharpe_Ratio best4.2   VaR percent8.2  Expected_Shortfall percent8.2;
	print {i in ASSETS: Weights[i]>1e-4} Weights percent8.2
	{i in ASSETS: Weights[i]>1e-4} returns percent8.2;

	/* write data to SAS data sets */
	create data robustscemax from [Ticker]={i in ASSETS: Weights[i]>1e-4}
		Weights=Weights;
quit;

proc sgpie data=robustscemax;
	title1 font=swissb height=2 'Asset Allocations Using Robust Portfolio Optimization';
	title2 font=swissb height=2 '(Return Maximization with Scenario Uncertainty)';
	pie  Ticker / response=Weights otherpercent=1  statfmt=percent8.2   datalabelloc=callout;
run;

title1;
title2;
quit;


/*********Program 9.9A**************************************/
/* Implementing Robust Portfolio Optimization using Ellipsoidal Uncertainty Set in PROC OPTMODEL*/
/* Use PROC IML to Simulate 1,000 Portfolio Scenarios and calculate covariance errors matrix*/
%let n =1000;
proc iml;
	use Portfolio_Returns;
	read all var _num_ into returns[colname=NumerNames rowname=Ticker];
	read all var _char_ into assets[colname=CharNames];/*Asset names*/

	use Portfolio_Covariances;
	read all var _num_ into sigma[colname=NumerNames rowname=Ticker];

	call randseed (7564321);

	SRet =RandNormal(&n,returns,sigma); /*Simulate 1000x2 vector*/
	SMReturns=t(mean(SRet)); /*calculate sample mean*/
	SSigma = cov(SRet); /*calculate sample covariance*/
		
	/*Compute Cholesky decompositions*/
	SigmaD=root((SSigma));

	/*Export covariance of error into SAS data sets*/
	sname ={'Assets'};/*Variable names setup*/
	cnames ='Ticker'||Numernames;

	create Portfolio_SigmaD from Assets SigmaD[colname= cnames ];
	append from Assets SigmaD;
	close Portfolio_SigmaD;


/*********Program 9.9B**************************************/
/* Implementing Robust Portfolio Optimization using Ellipsoidal Uncertainty Set in PROC OPTMODEL*/
%let rfr=0.00178;
%let alpha=0.99;
%let minimum_return = 0.0179;
%let max_risk = 0.045;
%let rho=0.2;

/*Portfolio optimization*/
proc optmodel;
	/* declare sets and parameters */
	set <str> ASSETS;
	num returns{ASSETS};
	num sigmaD{ASSETS,ASSETS};
	num covariance{ASSETS, ASSETS};

	/* read revised portfolio data from SAS data sets */
	read data Portfolio_Returns into ASSETS=[Ticker] returns=returns;

	read data Portfolio_Covariances into [Ticker]  
		{j in ASSETS} <covariance[Ticker,j]=col(j)>;

	*print returns;
	read data Portfolio_SigmaD into [Ticker]  
		{j in ASSETS} <sigmaD[Ticker,j]=col(j)>;
	*print covariance;

	/* declare variables */
	var Weights {ASSETS} >= 0;


	/*Calculate Portfolio Risk Measures*/
	impvar Robust_Sigma =sum{i in ASSETS, j in ASSETS} (Weights[i]*sigmaD[i,j]*Weights[j]);

	impvar Portfolio_Variance=sum {i in ASSETS, j in ASSETS}covariance[i,j] * Weights[i] * Weights[j];
	impvar Portfolio_Risk = sqrt(Portfolio_Variance);

	/*Included Codes: Value at Risk and Expected Shortfall*/
	impvar zes =(((2*constant("pi"))**(0.5))*(exp(((2**-0.5)*quantile('normal',&alpha))**2))*(1-&alpha))**-1;
	impvar VaR=sum {j in ASSETS} 
		-returns[j] * Weights[j]+quantile("normal",&alpha)*sqrt(Portfolio_Variance);
	impvar Expected_Shortfall=sum {j in ASSETS} 
		-returns[j] * Weights[j]+zes*sqrt(Portfolio_Variance);


	/*Calculate portfolio return*/
	impvar Expected_Return=sum {j in ASSETS} 
		returns[j] * Weights[j];
	impvar Portfolio_Return = Expected_Return;

	/* declare constraints */
	con Portfolio_Weights: sum {j in ASSETS} Weights[j] = 1;
	con Maximum_Risk: Portfolio_Risk<=&max_risk;

	/* declare objective */
	max WRet=(Expected_Return-&rho*robust_sigma**0.5);
	solve ;

	/*Portfolio Sharpe Ratio*/
	Impvar Sharpe_Ratio = (Expected_Return-&rfr)/Portfolio_Risk;

		/*Print Optimization Outputs*/
	print  Portfolio_Return percent8.2 Portfolio_Risk percent8.2 Sharpe_Ratio best5.2 VaR percent8.2 Expected_Shortfall percent8.2;
	print robust_sigma;
*	print Minimum_return.lb percent8.2 Minimum_return.dual percent8.2;
	print {j in ASSETS: Weights[j]>1e-4} Weights percent8.2
		{j in ASSETS: Weights[j]>1e-4} returns percent8.2;

	/* write data to SAS data sets */
	create data robustemax from [Name]={j in ASSETS: Weights[j]>1e-4}
		Weights=Weights;
quit;

proc sgpie data=robustemax;
	title1 font=swissb height=2 'Asset Allocations from Robust Portfolio Optimization';
	title2 font=swissb height=2 '(Return Maximization with Ellipsoid Uncertainty)';
	pie  Name / response=Weights otherpercent=1  statfmt=percent8.2   datalabelloc=callout;
run;
title1;
title2;
quit;

