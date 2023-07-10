
*************************************************************;
*******       Begin Scoring Code from HP Tree  		  *******;
*************************************************************;
%include "%sysfunc(pathname(work))/splitscore.sas";

*------------------------------------------------------------*;
*Computing Classification Vars: Target;
*------------------------------------------------------------*;
length I_Target $12;
label  I_Target = 'Into: Target';
length _format200 $200;
drop _format200;
_format200= ' ' ;
length _p_ 8;
_p_= 0 ;
drop _p_ ;
if P_Target1 - _p_ > 1E-8 then do ;
   _p_= P_Target1 ;
   _format200='1';
end;
if P_Target0 - _p_ > 1E-8 then do ;
   _p_= P_Target0 ;
   _format200='0';
end;
I_Target=dmnorm(_format200,32); ;
length U_Target 8;
label U_Target = 'Unnormalized Into: Target';
if I_Target='1' then
U_Target=1;
if I_Target='0' then
U_Target=0;
* Renaming variables for HPTree;
*------------------------------------------------------------*;
* Renaming Posterior variables for HPTree;
*------------------------------------------------------------*;
*drop HPTree_P_Target1;
HPTree_P_Target1 = P_Target1;
*drop HPTree_P_Target0;
HPTree_P_Target0 = P_Target0;
*------------------------------------------------------------*;
* Renaming _WARN_ variable for HPTree;
*------------------------------------------------------------*;
length HPTree_WARN_ $4;
drop HPTree_WARN_;
HPTree_WARN_ = _WARN_;
*------------------------------------------------------------*;
****************************************************************;
******     END OF HP TREE (PROC HPSPLIT) SCORING CODE    ******;
****************************************************************;


*************************************************************;
*******       Begin Scoring Code from HP SVM      *******;
*************************************************************;
%include "%sysfunc(pathname(work))/svmscore.sas";
*------------------------------------------------------------*;
*Computing Classification Vars: Target;
*------------------------------------------------------------*;
length _format200 $200;
drop _format200;
_format200= ' ' ;
length _p_ 8;
_p_= 0 ;
drop _p_ ;
if P_Target1 - _p_ > 1E-8 then do ;
   _p_= P_Target1 ;
   _format200='1';
end;
if P_Target0 - _p_ > 1E-8 then do ;
   _p_= P_Target0 ;
   _format200='0';
end;
I_Target=dmnorm(_format200,32); ;
length U_Target 8;
label U_Target = 'Unnormalized Into: Target';
if I_Target='1' then
U_Target=1;
if I_Target='0' then
U_Target=0;
* Renaming variables for HPSVM;
*------------------------------------------------------------*;
* Renaming Posterior variables for HPSVM;
*------------------------------------------------------------*;
*drop HPSVM_P_Target1;
HPSVM_P_Target1 = P_Target1;
*drop HPSVM_P_Target0;
HPSVM_P_Target0 = P_Target0;
*------------------------------------------------------------*;
* Renaming _WARN_ variable for HPSVM;
*------------------------------------------------------------*;
length HPSVM_WARN_ $4;
drop HPSVM_WARN_;
HPSVM_WARN_ = _WARN_;
*------------------------------------------------------------*;
****************************************************************;
******     END OF SCORING CODE FOR PROC HPSVM			  ******;
****************************************************************;



*************************************************************;
*******       Begin Scoring Code from HPNNA    *******;
*************************************************************;
%include "%sysfunc(pathname(work))/neuralscore.sas";

*------------------------------------------------------------*;
*Computing Classification Vars: Target;
*------------------------------------------------------------*;
length I_Target $12;
label  I_Target = 'Into: Target';
length _format200 $200;
drop _format200;
_format200= ' ' ;
length _p_ 8;
_p_= 0 ;
drop _p_ ;
if P_Target1 - _p_ > 1E-8 then do ;
   _p_= P_Target1 ;
   _format200='1';
end;
if P_Target0 - _p_ > 1E-8 then do ;
   _p_= P_Target0 ;
   _format200='0';
end;
I_Target=dmnorm(_format200,32); ;
length U_Target 8;
label U_Target = 'Unnormalized Into: Target';
if I_Target='1' then
U_Target=1;
if I_Target='0' then
U_Target=0;

* Renaming variables for HPNNA;
*------------------------------------------------------------*;
* Renaming Posterior variables for HPNNA;
*------------------------------------------------------------*;
*drop HPNNA_P_Target1;
HPNNA_P_Target1 = P_Target1;
*drop HPNNA_P_Target0;
HPNNA_P_Target0 = P_Target0;
*------------------------------------------------------------*;
* Renaming _WARN_ variable for HPNNA;
*------------------------------------------------------------*;
length HPNNA_WARN_ $4;
drop HPNNA_WARN_;
HPNNA_WARN_ = _WARN_;
*------------------------------------------------------------*;
****************************************************************;
******     END OF SCORING CODE FOR HPNNA	(PROC HPNEURAL)	*****;
****************************************************************;


*************************************************************;
*******      Begin Scoring Code from HPGLM*******;
*************************************************************;
%include "%sysfunc(pathname(work))/glmscore.sas";

* Renaming variables for HPGLM;
*------------------------------------------------------------*;
* Renaming Posterior variables for HPGLM;
*------------------------------------------------------------*;
*drop HPGLM_P_Target1;
HPGLM_P_Target1 = P_Target1;
*drop HPGLM_P_Target0;
HPGLM_P_Target0 = P_Target0;
*------------------------------------------------------------*;
* Renaming _WARN_ variable for HPGLM;
*------------------------------------------------------------*;
length HPGLM_WARN_ $4;
drop HPGLM_WARN_;
HPGLM_WARN_ = _WARN_;
****************************************************************;
******     END OF HP GLM (PROC HPGENSELECT) SCORING CODE  ******;
****************************************************************;


*************************************************************;
*******  Begin Scoring Code from HPReg     *******;
*************************************************************;
%include "%sysfunc(pathname(work))/logscore.sas";
* Renaming variables for HPReg;
*------------------------------------------------------------*;
* Renaming Posterior variables for HPReg;
*------------------------------------------------------------*;
*drop HPReg_P_Target1;
HPReg_P_Target1 = P_Target1;
*drop HPReg_P_Target0;
HPReg_P_Target0 = P_Target0;
*------------------------------------------------------------*;
* Renaming _WARN_ variable for HPReg;
*------------------------------------------------------------*;
length HPReg_WARN_ $4;
drop HPReg_WARN_;
HPReg_WARN_ = _WARN_;
****************************************************************;
******     END OF HP REG (PROC HPLOGISTICS) SCORING CODE    ******;
****************************************************************;




*------------------------------------------------------------*;
* Ensmbl: Average Posteriors of 5 models;
*------------------------------------------------------------*;
P_Target1 = (
HPSVM_P_Target1 +
HPNNA_P_Target1 +
HPGLM_P_Target1 +
HPReg_P_Target1 +
HPTree_P_Target1
)/5;
P_Target0 = (
HPSVM_P_Target0 +
HPNNA_P_Target0 +
HPGLM_P_Target0 +
HPReg_P_Target0 +
HPTree_P_Target0
)/5;
*------------------------------------------------------------*;
* Ensmbl: Computing Classification Vars;
*------------------------------------------------------------*;
length I_Target $32;
label I_Target = 'Into: Target';
length _format $200;
drop _format;
_format= ' ';
_p_= 0;
drop _p_;
if P_Target1 > _p_ then do;
_p_= P_Target1;
_format= '1';
end;
if P_Target0 > _p_ then do;
_p_= P_Target0;
_format= '0';
end;
%DMNORMCP(_format,I_Target);
*------------------------------------------------------------*;
* Ensmbl: Producing Unformatted variable;
*------------------------------------------------------------*;
label U_Target = "Unnormalized Into: Target";
if I_Target = '1' then U_Target = 1;
else
if I_Target = '0' then U_Target = 0;
run;
****************************************************************;
******     END OF Ensmbl SCORING CODE    ******;
****************************************************************;