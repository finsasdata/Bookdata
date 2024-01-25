/*This macro program that can be used to pull data from the
book's GitHub Repository. You must run the macro first before you
invoke it using the macro statement shown below.*********
*/
%macro datapull(fref,pname);
	filename  &fref "%sysfunc(getoption(WORK))/&pname";

	proc http
		url="https://github.com/finsasdata/Bookdata/raw/main/&pname"
		out=&fref
		method ="get";
	run;

%mend datapull;

/*
The macro is invoked as needed using the macro statement 
**********************************************
%datapull(‘File Reference’,’Physical Name’);
**********************************************
*/
