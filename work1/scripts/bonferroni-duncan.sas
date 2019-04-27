proc delete data=_all_;run;
*A clinical study was conducted to assess the effect of three formulations of the same drug on reducing
cholesterol. The formulations were 20mg at once (1time), 10mg twice a day (2times), and 5mg four times
a day (4times). In addition, two competing drugs were used as control group (drugD and drugE). 
The purpose of the study was to find which of the formulations, if any, is efficacious and how these 
formulations compare with the existing drugs;

proc format;
	value ct 0=' ' 1='1time' 2='2times' 3='3times' 4='drugD' 5='drugE';
run;


data Cholesterol(drop=i);
do trt= 1 to 5;
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

/*Importando o boxplot*/
ods graphics on / reset = all height= 4 in width=4 in border= off ;
ods listing image_dpi=200 gpath="C:\Users\André Felipe\Dropbox\UEM\3° Série\Planejamento e Análise de Experimentos I\Trabalho - Testes (Duncan Bonferoni e Dunnet)\Apresentação";
ods graphics / imagename = "boxplot" ; * diffogram file;
proc glm data=Cholesterol;
	class Trt;
	model Response=Trt;
	means trt / bon cldiff;
	lsmeans trt / adjust=bon;
	ods output CLDiffs=icdiff(drop=Effect Dependent Method);
quit;
ods graphics off;

*Importando o difograma;
ods graphics on / reset = all height= 4 in width=4 in border= off ;
ods listing image_dpi=200 gpath="C:\Users\André Felipe\Dropbox\UEM\3° Série\Planejamento e Análise de Experimentos I\Trabalho - Testes (Duncan Bonferoni e Dunnet)\Apresentação";
ods graphics / imagename = "diffograma-bonferroni" ; * diffogram file;
proc glimmix data=Cholesterol order=internal;
    class Trt; 
    model Response = Trt;
    lsmeans Trt /cl diff adjust=bon plots=diffogram(noabs center);
	ods output lsmeans=lsm(KEEP=trt estimate lower upper)
					   diffs=dfs(KEEP= trt _trt estimate adjlower adjupper adjp);
run;
ods graphics off;
*(keep=trt estimate lower upper)  (keep=trt _trt estimate adjlower adjupper adjp);


* Forest Plot;
data dfs2;
	set dfs(rename =(estimate=mndif1 adjlower=adjlower1 adjupper=adjupper1));
	drop ptxt;
	length label $30 ptxt $5;
	if adjp < .001 then ptxt=', p'; 
	else ptxt=', p=';
			label = cat(put(trt,ct.),' vs ',
			put(_trt,ct.),ptxt,STRIP(put(adjp,pvalue6.3)));
	if (adjp ge 0.05) then do;
		mndif2 = mndif1; adjlower2 = adjlower1; adjupper2 =adjupper1;
		mndif1 = . ; adjlower1 = . ; adjupper1 = . ;
	end;
run;

ods graphics on / reset = all height= 4 in width=4 in border= off ;
ods listing image_dpi=200 gpath="C:\Users\André Felipe\Dropbox\UEM\3° Série\Planejamento e Análise de Experimentos I\Trabalho - Testes (Duncan Bonferoni e Dunnet)\Apresentação";
ods graphics / imagename = "forest-plot" ;
proc sgplot DATA=dfs2 noautolegend ;
	REFLINE 0 / axis=x lineattrs=(color=black pattern=2 thickness=1) transparency=0;
	SCATTER y=label x=mndif1 / xerrorlower=adjlower1 xerrorupper=adjupper1
	errorbarattrs=(color=black pattern=1 thickness=2)
	markerattrs=(color=black symbol= circlefilled size=6)
	datalabel=mndif1 datalabelattrs=(color=black weight=bold size=7);
	/* print the differences at the center of each confidence interval */
	SCATTER y=label x=mndif2 / xerrorlower=adjlower2 xerrorupper=adjupper2
	errorbarattrs=(color=black pattern=2 thickness=2)
	markerattrs=(color=black symbol= circlefilled size=6)
	datalabel=mndif2 datalabelattrs=(color=black weight=bold size=7);
	XAXIS offsetmin=0.05 offsetmax=0.05 label='Difference' values=(-20 to 4 by 1);
	YAXIS offsetmin=0.12 offsetmax=0.12 display=(Nolabel Noticks) reverse ;
	FORMAT mndif1 mndif2 5.1;
	TITLE "Differences and Adjusted 95% Confidence Intervals";
run;
ods graphics off;


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
ods graphics on;
proc glm data=Cholesterol;
	class Trt;
	model Response=Trt;
	lsmeans Trt / adjust=dunnett diff=control("4") plot=diff pdiff;
quit;
ods graphics off;

