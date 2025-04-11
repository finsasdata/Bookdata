
data _null_;
	idMaxLength=length("1");
	idMaxLength=max(idMaxLength, length("2"));

	/* Put it in a macro variable for use in the real code */
	call symput('idLength', idMaxLength);
run;

proc sort data=EMSAS.SPXRAW out=work._sorted_;
	by Target;
run;

proc means data=work._sorted_ noprint;
	by Target;
	output out=work._meansOut_(drop=_type_ _freq_) n=__nobs__;
run;

proc sql noprint;
	select max(__nobs__) into :count from work._meansOut_;
quit;

data EMSAS.SPXRAWP;
	set work._sorted_;
	by Target;
	length Partition $ &idLength;
	retain __tmp1-__tmp%trim(&count) __nobs__ __nobs1__ __nobs2__;
	retain __nobs__ __seed__ _n1_;
	drop __k__;
	drop _i_ __seed__ __tmp1-__tmp%trim(&count);
	drop _n1_ __nobs__ __nobs1__ __nobs2__;
	array __tmp(*) __tmp1-__tmp%trim(&count);

	if (_n_=1) then
		do;
			__seed__=54321;
			__nobs__=&count;
		end;

	if first.Target then
		do;
			set work._meansOut_;
			by Target;

			do _i_=1 to __nobs__;
				__tmp(_i_)=_i_;
			end;

			if (__nobs__ < dim(__tmp)) then
				do;

					do _i_=__nobs__+1 to dim(__tmp);
						__tmp(_i_)=0;
					end;
				end;
			call ranperm(__seed__, of __tmp(*));

			if (__nobs__ < dim(__tmp)) then
				do;

					/* Move non-zero values to beginning of list */
					do _i_=1 to dim(__tmp);

						if (__tmp(_i_)=0) then
							do;

								if (_i_ < dim(__tmp)) then
									do;
										__k__=_i_ + 1;

										do while(__k__ < dim(__tmp) and __tmp(__k__)=0);
											__k__=__k__+1;
										end;

										if (__k__ <=dim(__tmp)) then
											do;
												__tmp(_i_)=__tmp(__k__);
												__tmp(__k__)=0;
											end;
									end;
							end;
					end;
				end;
			_n1_=0;
			__nobs1__=round(0.7*__nobs__);
			__nobs2__=round(0.3*__nobs__)+__nobs1__;
		end;
	_n1_=_n1_ + 1;

	if (_n1_ <=dim(__tmp)) then
		do;

			if (__tmp(_n1_) > 0) then
				do;

					if (__tmp(_n1_) <=__nobs1__) then
						do;
							Partition="1";
							output;
						end;
					else if (__tmp(_n1_) <=__nobs2__) then
						do;
							Partition="2";
							output;
						end;
				end;
		end;
run;

ods noproctitle;

proc print data=EMSAS.SPXRAWP(obs=10);
	title "Subset of EMSAS.SPXRAWP";
run;

title;

proc delete data=work._sorted_;
run;

proc delete data=work._meansOut_;
run;