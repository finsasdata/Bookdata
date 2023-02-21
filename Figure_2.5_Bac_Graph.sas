%datapull(bac,bac_price.sas7bdat);

TITLE;
TITLE1 font="times new roman/bold" " Bank of America (BAC) Stock Price Returns";
TITLE2 font="times new roman/bold" "(5 Minutes Interval)";
proc sgplot data=bac_price;
series x=date_5m y=returns_5m / lineattrs=(thickness=2);
xaxis type=linear 	labelattrs=(size=12pt weight=bold family="Times New Roman")
values=('03jan2023:09:30:00'dt to '03feb2023:16:00:00'dt by dtday2); 
yaxis offsetmin=0.05 offsetmax=0.05 labelattrs=(size=12pt weight=bold family="Times New Roman");
run;
Title;

TITLE;
TITLE1 font="times new roman/bold" " Bank of America (BAC) Stock Price Returns";
TITLE2 font="times new roman/bold" "(10 Minutes Interval)";
proc sgplot data=bac_price;
series x=date_10m y=returns_10m / lineattrs=(thickness=2);
xaxis type=linear 	labelattrs=(size=12pt weight=bold family="Times New Roman")
values=('03jan2023:09:30:00'dt to '03feb2023:16:00:00'dt by dtday2); 
yaxis offsetmin=0.05 offsetmax=0.05 labelattrs=(size=12pt weight=bold family="Times New Roman");
run;
Title;

TITLE;
TITLE1 font="times new roman/bold" " Bank of America (BAC) Stock Price Returns";
TITLE2 font="times new roman/bold" "(60 Minutes Interval)";
proc sgplot data=bac_price;
series x=date_1h y=returns_1h / lineattrs=(thickness=2);
xaxis type=linear 	labelattrs=(size=12pt weight=bold family="Times New Roman")
values=('03jan2023:09:30:00'dt to '03feb2023:16:00:00'dt by dtday2); 
yaxis offsetmin=0.05 offsetmax=0.05 labelattrs=(size=12pt weight=bold family="Times New Roman");
run;
Title;

TITLE;
TITLE1 font="times new roman/bold" " Bank of America (BAC) Stock Price Returns";
TITLE2 font="times new roman/bold" "(Daily Interval)";
proc sgplot data=bac_price;
series x=date y=returns_Daily / lineattrs=(thickness=2);
xaxis type=linear 	labelattrs=(size=12pt weight=bold family="Times New Roman")
values=('03jan2023:09:30:00'd to '03feb2023:16:00:00'd by day); 
yaxis offsetmin=0.05 offsetmax=0.05 labelattrs=(size=12pt weight=bold family="Times New Roman");
run;
Title;