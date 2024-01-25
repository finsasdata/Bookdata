/*************************************************/
/*****WARNING**********************WARNING********/

/*This code will delete your local SASGit Repository
Use only if you are sure that you don't need any of the files
in it. You should first try to manually clear your directory
before using this program

***************USE AT YOUR OWN RISK******************
/***************************************************/
/*data _null_;
 rc=git_delete_repo("&datapath.");
 put rc=;
run;
*/

/******************Program 1.5****************/
options dlcreatedir;
%let datapath = %sysfunc(getoption(WORK))/finsasdata;
libname findata "&datapath.";
run;

data _null_;
	rc = git_clone("https://github.com/finsasdata/Bookdata/",
	"&datapath.");
	%put rc=;
run;

/******************Program 1.6****************/
proc datasets library=findata;
run;

quit;