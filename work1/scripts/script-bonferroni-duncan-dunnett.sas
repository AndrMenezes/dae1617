proc delete data=_all_;run;
*A clinical study was conducted to assess the effect of three formulations of the same drug on reducing
cholesterol. The formulations were 20mg at once (1time), 10mg twice a day (2times), and 5mg four times
a day (4times). In addition, two competing drugs were used as control group (drugD and drugE). 
The purpose of the study was to find which of the formulations, if any, is efficacious and how these 
formulations compare with the existing drugs;

data Cholesterol;
do Trt = '1time','2times','4times','drugD','drugE';
do i = 1 to 10;
input Response @@;
output;
end;
end;
datalines;
3.86 10.39 5.91 3.06 7.72 2.71 4.92 2.30 7.53 9.41
10.40 8.60 13.63 3.51 7.77 8.63 9.23 6.32 15.83 8.34
13.96 13.96 13.92 8.05 11.04 12.37 10.39 9.03 12.84 18.18
16.98 15.46 19.98 14.74 13.59 10.86 17.59 8.82 17.96 17.63
21.51 27.24 20.52 15.77 22.89 23.95 21.59 18.31 20.39 17.31
;

/*Teste de bonferoni*/
proc glm data=Cholesterol;
	class Trt;
	model Response=Trt;
	means trt / bon cldiff;
	lsmeans Trt / adjust=bon plot=diff;
	ods output CLDiffs=icdiff(drop=Effect Dependent Method);
quit;


/*Importando para o LaTeX*/
ods tagsets.tablesonlylatex file="bonferroni.tex" (notop nobot) newfile=table;
proc print data=icdiff noobs;
run;
ods tagsets.tablesonlylatex close;

/*Teste de Duncan*/
proc glm data=Cholesterol;
	class Trt;
	model Response=Trt;
	means Trt / duncan lines;
quit;


/*Teste de Dunnet com controle o "drugD"*/
proc glm data=Cholesterol;
	class Trt;
	model Response=Trt;
	means Trt / dunnett('drugD') cldiff;
	lsmeans Trt / adjust=dunnett pdiff = control("drugD") plot=diff pdiff;
	ods output CLDiffs = icdiff2(drop = Effect Dependent Method);
quit;

/*Importando para o LaTeX*/
ods tagsets.tablesonlylatex file="dunnett.tex" (notop nobot) newfile=table;
proc print data=icdiff2 noobs;
run;
ods tagsets.tablesonlylatex close;


* Teste de Dunnett unilateral a esquerda com grupo controle "drugD";
proc glm data=Cholesterol;
	class Trt;
	model Response=Trt;
	means Trt / dunnettl('drugD') cldiff;
	lsmeans Trt / adjust=dunnett pdiff = controll("drugD") plot=diff pdiff;
	ods output CLDiffs = icdiff3(drop = Effect Dependent Method);
quit;
