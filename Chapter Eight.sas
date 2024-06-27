/*****************************************/
/* Financial Data Science with SAS       */
/* SAS Codes for Chapter Six Examples   */
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



/*********Program 8.1**************************************

/*Simple Proc OPTMODEL Problem*/
proc optmodel;
	var x >= 0 , y >= 0;
	min f = 7*x +5*y;
	con 4*x + 3*y >= 19;
	con  x + y <= 5;
	solve;
	print x y;
quit;


/*********Program 8.2**************************************/
/*Using PROC OPTMODEL to Solve Simple Optimization Problem*/
proc optmodel;
	var x1 >= 0.0 , x2 >= 0.0;
	max Z = 4*x1 +6*x2;
	con 4.5*x1 + 8*x2 <= 6.0;
	con x1 + x2= 1.0;
	solve with lp / algorithm=ps;
	print x1 x2;
	print _ACON_.dual _ACON_.body;
	expand;
quit;


/*********Program 8.3**************************************/
/*Solving the Dual of an Optimization Problem using PROC OPTMODEL*/
proc optmodel;
	var y1 >= 0.0 , y2 >= 0.0;
	min Z = 6*y1 +y2;
	con 4.5*y1 + y2 >=4.0;
	con  8.0*y1 + y2= 6.0;
	solve with lp / algorithm=ps;
	print y1 y2;
	print _ACON_.dual _ACON_.body;
	expand;
quit;


/*********Program 8.4**************************************/
/*Solving Integer Linear Programming (ILP) Problem (Profit Maximization) Using PROC OPTMODEL*/
proc optmodel;
	/* declare variables */
	var Sedans >= 0 integer, SUVs >= 0 integer, Trucks>=0 integer;

	/* declare constraints */
	con Labor: 40*Sedans + 50*SUVs+ 60*Trucks <= 1000000;
	con Equipment: 35*Sedans + 45*SUVs + 60*Trucks<=2000000;
	con Materials: 40*Sedans + 60*SUVs + 75*Trucks <= 2000000;

	/* declare objective */
	max NetProfit = 26000*Sedans + 37500*SUVs + 45000*Trucks 
		- 50 * (40*Sedans + 50*SUVs+ 60*Trucks) 
		- 35 * (35*Sedans + 45*SUVs + 60*Trucks) 
		- 400 * (40*Sedans + 60*SUVs + 75*Trucks);
	expand;
	solve;
	print Sedans SUVs Trucks;
quit;


/*********Program 8.5A**************************************/
/*Reading Data into OPTMODEL Procedure*/
proc optmodel;
set VEHICLES = /Sedan SUVs Trucks/;
num sales_price {VEHICLES} = [26000 37500 45000];
print sales_price;

/*********Program 8.5B**************************************/
/*Reading SAS Dataset into OPTMODEL Procedure*/

data price_data;
	input Type $ Price;
	datalines;
Sedan 26000
SUVs 37500
Trucks 45000
;
run;

proc optmodel;
	set <str> VEHICLES;
	num sales_price {VEHICLES};
	read data price_data into VEHICLES=[Type]
		sales_price=Price;
	print  sales_price dollar8.;


/*********Program 8.5C**************************************/
/*Reading Data into OPTMODEL Procedure*/
proc optmodel;
set <str> VEHICLES;
num sales_price {VEHICLES};
read data price_data into VEHICLES=[Type]
sales_price=Price;
print  sales_price dollar8.;
impvar Revenue=
		sum{p in VEHICLES} sales_price[p];
		print Revenue dollar8.;
quit;


/*********Program 8.5D**************************************/
/*Reading Data into OPTMODEL Procedure*/
data resource_data;
	format Items $10.;
	input Items $ Costs Available;
	datalines;
Labor  50 1000000 
Equipment 35 2000000 
Material  400 2000000 
;
run;

data production_data;
	input Types $ Price Labor Equipment Material;
	datalines;
Sedan 26000 40 35 40
SUVs 37500 50 45 60
Trucks 45000 60 60 75
;
run;


/*********Program 8.5E**************************************/
/*Using Index Set, Arrays, and SAS  dataset in OPTMODEL Procedure*/
proc optmodel;
	set <str> RESOURCES,VEHICLES;
	num sales_price {VEHICLES};
	num costs{RESOURCES},available{RESOURCES};
	num inputs{VEHICLES,RESOURCES};

	/*read data price_data into index sets*/
	read data resource_data into RESOURCES=[Items]
		costs available;
	read data production_data into VEHICLES=[Types]
		{r in RESOURCES} <inputs[Types,r]=col(r)>
		sales_price=price;
	
	/*Validate Data Read into Problem*/
	print costs available;
	print sales_price inputs;

	/*Declare Variables*/
	var Units{VEHICLES}>=0 integer;
	impvar Revenue=
		sum{p in VEHICLES} sales_price[p]*Units[p];
	impvar Usage {r in RESOURCES}=
		sum{p in VEHICLES} Units[p]*Inputs[p,r];
	impvar ProductCost=
		sum{r in RESOURCES} costs[r]*Usage[r];

	/*Declare Constraints*/
	con Utilization {r in RESOURCES}:
		Usage[r]<=available[r];

	/*Declare Objective*/
	max Profit = Revenue - ProductCost;
	expand;
	solve;
	print units usage;
quit;


/*********Program 8.6**************************************/
/*Solving MILP Problems Using OPTMODEL Procedure*/
Data production_data;
	input Items $ Price Labor Equipment Material Ounits rate lsp;
	datalines;
Sedan 26000 40 35 40 600 0.05 0.35
SUVs 37500 50 45 60 650 0.08 0.25
Trucks 45000 60 60 75 630 0.09 0.35
;
run;
%let minimum_unit=4000;
proc optmodel;
	set <str> RESOURCES,VEHICLES;
	num sales_price {VEHICLES};
	num costs{RESOURCES},available{RESOURCES};
	num inputs{VEHICLES,RESOURCES};
	num lsp{VEHICLES};
	num ounits{VEHICLES};
	num rate{VEHICLES};

	read data resource_data into RESOURCES=[Items]
		costs available;

	read data production_data into VEHICLES=[Items]
	{r in RESOURCES} <inputs[Items,r]=col(r)> ounits
	 rate lsp sales_price=price;

	/*Declare explicit and Implicit Variables*/
	var Units{VEHICLES}>=0 integer;
	var Discount{VEHICLES} binary;
		impvar Revenue=	
		sum{p in VEHICLES}  (sales_price[p]*Units[p]);
	impvar AdjRevenue=
		sum{p in VEHICLES} ((Discount[p]*((1-rate[p])-0.1*lsp[p])+(1-Discount[p])*(1-lsp[p]))*ounits[p]*sales_price[p]);

impvar Usage {r in RESOURCES}=
		sum{p in VEHICLES} Units[p]*Inputs[p,r];
	impvar ProductCost=
		sum{r in RESOURCES} costs[r]*Usage[r];

	impvar AdjProductCost=
		sum{r in RESOURCES,p in VEHICLES} ((Discount[p]+(1-Discount[p])*(1-lsp[p]))*(ounits[p]*Inputs[p,r]*costs[r]));

	/*Declare Constraints*/
	con Utilization {r in RESOURCES}:
		Usage[r]<=available[r];
	
	/*Declare Initial Objective Function*/
	max Profit = Revenue - ProductCost;

	/*Declare Additional Constraints*/
	con Erosion{p in VEHICLES}: (1/ounits[p])*units[p]>=7;
	con ProfitBound:AdjRevenue-AdjProductCost>=0;
	con Minimum_Units {p in VEHICLES}: Units[p]>=&minimum_unit;

	/*Declare Second Stage Objective Function*/
	max AdjProfit= Profit+AdjRevenue - AdjProductCost;
	expand;

	solve obj AdjProfit with milp/relobjgap=0;
	print units usage rate percent8. discount  lsp percent8.;
print Revenue dollar12.  AdjRevenue dollar12. ProductCost dollar12. AdjProductCost dollar12.;
quit;



/*********Program 8.7**************************************/
/*Solving Capital Budgeting with Capital Rationing problem using PROC OPTMODEL*/
data equipment_data;
	input equipment $ discount_rate;
	datalines;
A 0.1
B 0.1
C 0.1
D 0.1
E 0.1
;
run;
data cashflow_data;
	input cf $ A B C D E;
	datalines;
cf0 -10000 -15000 -20000 -24000 -32000
cf1 3000 5000 5000 7000 8000
cf2 4000 7000 7000 8500 7500
cf3 3500 9000 10000 11000 14000
cf4 800 8000 13000 13000 15600
cf5 0 7500 15000 16000 23000
;
run;

%let max_budget=50000;

proc optmodel;
	set <str> PROJECT,CASHFLOW;
	num  discount_rate{PROJECT};
	num  cashflows{PROJECT,CASHFLOW};
	num  year{CASHFLOW} =[0 1 2 3 4 5];
	read data equipment_data into PROJECT=[equipment]
		discount_rate;
	read data cashflow_data into CASHFLOW=[cf]
		{r in PROJECT} <cashflows[r,cf]=col(r)>;

	/*Declare Decision Variables*/
	var X{r in PROJECT} binary;

	/*Calculate PV of Cash flows*/
	impvar cf{r in Project}=sum{p in CASHFLOW} (cashflows[r,p]/(1+discount_rate[r])**year[p]);
	/*Calculate Initial Capital Outlay*/
	impvar cost{r in PROJECT} =sum{p in CASHFLOW} (if year[p]=0 then X[r]*cashflows[r,p]);

	/* declare constraints */
	con Budget: sum{r in PROJECT} cost[r] >=-&max_budget;

	/* declare objective */
	max NPV  = sum{r in PROJECT} X[r]*cf[r];
	expand;
	solve;

	/*Print Optimization Output*/
	print X;
	print Budget.lb dollar8.; 


/*********Program 8.8**************************************/
/*Using PROC OPTMODEL to solve Cashflow Dedication Problem*/
%datapull(outflows,outflows.sas7bdat);
%datapull(bonds,bondsdata.sas7bdat);
proc optmodel;
	/* declare sets and parameters */
	set <num> CASHFLOWS;
	set <str> BONDS;
	num year{CASHFLOWS}, liabilities{CASHFLOWS};
	num price{BONDS},coupon{BONDS},maturity{BONDS};
	num mature{CASHFLOWS};

	/*Problem data from SAS  dataset*/
	read data outflows into CASHFLOWS=[_N_]
		year liabilities mature=_N_ ;

	*print outflow dollar8. mature;
	read data bondsdata into BONDS=[bond] 
		price coupon maturity;

	/*Count the number of years of funding need*/
	impvar N=card(CASHFLOWS);

	/*Declare Decision Variables*/
	var X{i in BONDS} >=0 integer, Z{j in 0..N}>=0;

	/*Setup liability funding pattern (Coupon+Principal at maturity + surplus). Coupons received every year till maturity, Principal received at maturity	*/

	impvar coupons {j in CASHFLOWS}=
		sum{i in BONDS} (if maturity[i]>=mature[j] then coupon[i]*10*X[i]); /*Coupons*/

	impvar Principal{j in CASHFLOWS}= 
	sum{i in BONDS}(if maturity[i]=mature[j] then 1000*X[i]); /*Principal*/

	/*Surplus cash*/
	impvar Surplus{j in CASHFLOWS}=Z[j]-Z[j-1]; /*Surplus*/

	/*Dollar Value of Bond Purchased*/
	impvar Holdings {i in BONDS} = X[i]*price[i]*10;

	/*Declare Constraints Bond Cashflows = liability funding+Surplus*/
	con Cfcon {j in CASHFLOWS}: coupons[j]+principal[j]-surplus[j]=liabilities[j]*1000;
	 

	/*Declare Objective function*/
	min Totalcost =Z[0]+sum{i in BONDS} price[i]*10*X[i];
	solve with milp;
	expand;

	/*Print Optimization Output*/
	print Totalcost dollar12.;
	print X comma10. Holdings dollar12.;
	print Z dollar12. surplus dollar12.;

		/* write data to SAS  datasets */
	create data Holdings from [Bond]={j in BONDS: Holdings[j]}
		Holdings=Holdings;
quit;

	
/*********Program 8.9**************************************/
/*Solving QP using Interior Point Algorithm*/
proc optmodel; var x >= 0, y >= 0;
min Z = (x**2)+(y**2)-2*x+6*y ;
solve with nlp obj z/algorithm=ip;
print x x.dual y y.dual;
con A:  3*x+8*y>=25;
con B:  x+y <= 5;
solve with nlp obj z/algorithm=ip;
print x x.dual y y.dual;
expand a;
print A.dual A.body;
print B.dual B.body;
quit;



/*********Program 8.10**************************************/
/*Implementing Return-Based Style Analysis using PROC OPTMODEL*/
%datapull(constr,constrained.sas7bdat);

proc optmodel;
	/* declare sets and parameters */
	set <num> FUND;
	set <str> FACTOR={'MRP', 'SMB', 'HML','RMW', 'CMA'};
	num Preturns {FUND};
	num Factors{FUND,FACTOR};

	/* read portfolio data from SAS data sets */
	read data Constrained into FUND=[Date] PReturns;

	*read data Constrained into FACTORS=[Month] PReturns;
	read data Constrained into [Date]  
		{j in FACTOR} <Factors[Date,j]=col(j)>;

	/* declare variables */
	var Weights{FACTOR} >= 0;

	/*Calculate Portfolio Risk Measures*/
impvar TE{i in FUND}=sum{j in FACTOR}(preturns[i]-Weights[j]*factors[i,j])**2;

	/* declare constraints */
	con Factor_Weights: sum {j in FACTOR} Weights[j] = 1;

	/* declare objective */
	min Fund_Factor = sum{i in FUND}TE[i];
	solve with QP;
	print Weights;

	/* write data to SAS data sets */
	create data factweight from [Factors]={j in FACTOR}
		Weights=Weights;
create data RSquarecal from [date]={i in FUND} TE=TE PReturns= preturns;
quit;
proc iml;
	use Rsquarecal;
	read  all into RR[colname=NumerNames];
	CR=RR[,2:3];
	VCV=cov(CR);
	print VCV;
	RSquare=1-VCV[1,1]/VCV[2,2];
	print RSquare[ format=percent8.2];
quit;
