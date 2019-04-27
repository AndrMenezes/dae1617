* Basic SAS code assumes coding categorical factors as integers, 1, 2, 3, .. ;
* Apply a format to attach factor level descriptions;
PROC FORMAT ;
VALUE ct 0=' ' 1='A' 2='B' 3='C' 4='D' 5=' ';
run;
DATA ctlyst;
label catalyst='Catalyst' conc='Concentration';
input catalyst conc @@;
cards;
1 58.2 1 57.2 1 58.4 1 55.8 1 54.9
2 56.3 2 54.5 2 57.0 2 55.3
3 50.1 3 54.2 3 55.4
4 52.9 4 49.9 4 50.0 4 51.7
;
ODS OUTPUT lsmeans=lsm(KEEP=catalyst estimate lower upper)
diffs=dfs(KEEP= catalyst _catalyst estimate adjlower adjupper adjp);
ODS GRAPHICS ON / reset = all height= 4 in width=4 in border= off ;
ODS LISTING image_dpi=200 gpath="C:\Users\André Felipe\Dropbox\UEM\3° Série\Planejamento e Análise de Experimentos I\Trabalho - Testes (Duncan Bonferoni e Dunnet)";
ODS GRAPHICS / imagename = "diffogram" ; * diffogram file;
PROC GLIMMIX DATA=ctlyst order=internal;
CLASS catalyst;
MODEL conc = catalyst / solution;
LSMEANS catalyst / cl diff adjust=tukey PLOT=diff(noabs center);
FORMAT catalyst ct. ;
run;
ODS GRAPHICS off;

* Forest Plot of Differences ;
* with the file of differences
add a text variable called label describing each comparison;
DATA dfs2;
SET dfs(rename =(estimate=mndif1 adjlower=adjlower1 adjupper=adjupper1));
DROP ptxt;
LENGTH label $30 ptxt $4;
IF adjp < .001 then ptxt=', p'; else ptxt=', p=';
label = CAT(put(catalyst,ct.),' vs ',
put(_catalyst,ct.),ptxt,STRIP(put(adjp,pvalue6.3)));
* new variables for non-significant differences, set existing to missing;
IF (adjp GE 0.05) then do;
mndif2 = mndif1; adjlower2 = adjlower1; adjupper2 =adjupper1;
mndif1 = . ; adjlower1 = . ; adjupper1 = . ;
end;
RUN;
PROC PRINT DATA=dfs2 NOObs;
RUN;

ODS GRAPHICS ON / reset = all height= 4 in width=4 in border= off ;
ods listing image_dpi=200 gpath="C:\Users\André Felipe\Dropbox\UEM\3° Série\Planejamento e Análise de Experimentos I\Trabalho - Testes (Duncan Bonferoni e Dunnet)";
ods graphics / imagename = "forest_diffs" ;
PROC SGPLOT DATA=dfs2 noautolegend ;
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
XAXIS offsetmin=0.05 offsetmax=0.05 label='Difference' values=(-3 to 10 by 1);
YAXIS offsetmin=0.12 offsetmax=0.12 display=(Nolabel Noticks) reverse ;
FORMAT mndif1 mndif2 5.1;
TITLE "Differences and Adjusted 95% Confidence Intervals";
RUN;
ODS GRAPHICS off;

* MMC plot ;
* need values for plotting rounded to the smallest number of decimals
that allow graphical resolution, in this case round to nearest 1 decimal;
* remove attached formats on catalyst and _catalyst ;
* For plotting and assigning formats, round LsMeans
to resolution of graph or smaller, if necessary, so they remain distinct;
%LET rnd = .1 ; * round lsmean to the nearest .1 decimal value;
DATA lsm; set lsm;
estimate=round(estimate,&rnd.);
PROC DATASETS NOlist;
MODIFY lsm; FORMAT catalyst 3.0 estimate 5.1;
RUN; quit;
PROC PRINT DATA=lsm NOObs; run;
PROC DATASETS nolist;
MODIFY dfs; FORMAT catalyst _catalyst 4.0;
run; quit;
proc print data=dfs NOObs; run;
DATA lsm2; SET lsm;
KEEP catalyst estimate start;
start=estimate;
PROC SORT DATA=lsm2;
BY estimate; RUN;
* Labels for means placed the left vertical axis;
DATA Lfmt; SET lsm2;
DROP y cmpr catalyst ;
LENGTH label $15; * NEED length long enough to contain format and mean ;
RETAIN fmtname "Llbl" type 'N';
label = CATX(' ',put(catalyst,ct.),put(estimate,best12.));
run;

proc format cntlin=Lfmt; run;
* test the format;
PROC FREQ DATA=lsm2 order=formatted;
TABLES estimate / nocum ;
FORMAT estimate Llbl. ;
RUN;
* place the lsmeans into a macro variable from smallest to largest
(needed for values=() option YAXIS statement);
PROC SQL NOprint;
SELECT distinct estimate
into: mns separated by ' '
FROM lsm2
ORDER BY estimate;
quit;
* the right vertical axis Y2AXIS, plotting values;
* get low to high values of lsmeans transposed into macro variable;
PROC TRANSPOSE DATA=lsm out=tlsm (drop=_name_ _label_) prefix=_mn;
VAR estimate; ID catalyst;
proc print data=tlsm NOObs; run;
DATA dfs2; SET dfs;
IF _n_ = 1 then set tlsm; * read transposed means;
DROP _mn1 _mn2 _mn3 _mn4 ptxt catalyst _catalyst;
LENGTH ylabel $35 ptxt $4;
FORMAT y2 6.2 ;
ARRAY mns{4} _mn1 _mn2 _mn3 _mn4 ;
y = (mns{catalyst} + mns{_catalyst})/2 ;
y2=y; * copy of y for yaxis2;
IF adjp < .001 then ptxt=', p'; else ptxt=', p=';
ylabel = CAT(strip(PUT(catalyst,ct.)),' vs ',strip(PUT(_catalyst,ct.)),
strip(ptxt),strip(PUT(adjp,pvalue6.3))); * values to label y2axis;
grp=(adjp < .05);
* For non-significant differences and confidence intervals;
IF grp=0 then
DO; estimate2=estimate; adjupper2=adjupper; adjlower2=adjlower;
estimate = . ; adjupper = . ; adjlower = . ;
END;
RUN;
proc print data=dfs2 NOObs;
var ylabel Estimate adjLower adjUpper adjp grp estimate2 adjupper2 adjlower2 y: ;
format estimate adjlower adjupper 6.1 adjp pvalue6.3 ;
run;
* place average of pairs of lsmeans into a macro variable
from smallest to largest for Y2AXIS;
PROC SQL NOprint;
SELECT distinct y2
into: amns separated by ' '
FROM dfs2
ORDER BY y2;
quit;
* the value of y2 must be different for each comparison for the format to work;
* for equal values, need to either refine the rounding or offset values;

* Difference labels for the right vertical axis;
DATA Rfmt;
SET dfs2(keep=y2 ylabel rename=(y2=start ylabel=label));
RETAIN fmtname "Rlbl" type 'N';
run;
proc format cntlin=Rfmt; run;
* test the format;
proc freq data=dfs2 order=formatted;
tables y2 / nocum ;
format y2 Rlbl. ;
run;
* find the min and max values derived from both of these PROC MEANS;
proc means data=dfs2 min max maxdec=3; var y y2; run;
proc means data=lsm2 min max maxdec=3; var estimate ; run;
* Enter boundary values for lsmeans from MEANS output;
%LET ymn=51; * minimum for both vertical axes;
%LET ymx=57; * maximum for both vertical axes;
* annotate data set for YAXIS scale placed inside graph;
DATA LsmT;
DROP yt;
LENGTH function $4 label $2;
RETAIN linecolor 'black' linestyle 'solid' linesize 1
y1space "datavalue" x1space "wallpercent"
y2space "datavalue" x2space "wallpercent"
textcolor 'black' textsize 7 ;
* tick mark for LsMeans (left axis);
DO yt = &ymn. to &ymx. by 1;
function= 'line'; label= ' ';
x1 = 0 ; y1 = yt;
x2 = x1 + 1; y2 = yt; OUTPUT;
function= "text"; label= strip(put(yt,3.0));
x1=x2 + 2; x2=x1 + 5; OUTPUT;
end;
proc print data=LsmT NOObs; run;
proc means data=dfs2 min max maxdec=2;
var adjlower adjupper adjlower2 adjupper2;
run;
* Enter boundary values for differences (horizontal axis) from PROC MEANS ;
%LET dmn= -3; * minimum difference;
%LET dmx= 10; * maximum difference;
%LET dcr= 1; * increment for differences on lower horizontal axis;

ODS GRAPHICS on / reset = all height= 4 in width=6 in border= off;
ODS LISTING image_dpi=200 gpath="C:\Users\André Felipe\Dropbox\UEM\3° Série\Planejamento e Análise de Experimentos I\Trabalho - Testes (Duncan Bonferoni e Dunnet)";
ODS GRAPHICS / imagename = "mmc_diffs" ;
PROC SGPLOT DATA=dfs2 noautolegend sganno=lsmT;
* plot the differences, solid for significant, dashed for non-significant;
SCATTER x=estimate y=y / y2axis xerrorlower=adjlower xerrorupper=adjupper
errorbarattrs=(color=black pattern=1 thickness=2)
markerattrs=(color=black symbol= circlefilled size=5)
datalabel=estimate datalabelattrs=(color=black weight=bold size=7);
SCATTER x=estimate2 y=y / y2axis xerrorlower=adjlower2 xerrorupper=adjupper2
errorbarattrs=(color=black pattern=2 thickness=2)
markerattrs=(color=black symbol= circlefilled size=5)
datalabel=estimate2 datalabelattrs=(color=black weight=bold size=7);
Y2AXIS offsetmin=0.05 offsetmax=0.05
min=&ymn. max=&ymx. values=( &amns ) VALUESHINT valueattrs=(weight=bold size=7)
tickvalueformat=data display=(Nolabel);
* min and max values needed determined by range of lsmeans;
XAXIS offsetmin=0.05 offsetmax=0.05
values=(&dmn. to &dmx. by &dcr. ) valueattrs=(weight=bold size=7)
label="Differences" labelattrs=(weight=bold size=8);
REFLINE 0 / axis=x LINEATTRS=(thickness=1 color=black pattern=21)
transparency=.2 name="zr";
KEYLEGEND "zr" / across=1 down=1 noborder location=inside position=bottom
title='No Difference' TITLEATTRS=(Color=black Size=8 Weight=Bold);
* place the formatted label for means on the Left Vertical axis (does not plot);
SCATTER x=estimate y=y2 / markerattrs=(color=white size=0);
YAXIS offsetmin=0.05 offsetmax=0.05
min=&ymn. max=&ymx. values= ( &mns ) VALUESHINT valueattrs=(weight=bold size=7)
grid tickvalueformat=data display=(nolabel);
FORMAT estimate: 5.1 y2 Llbl. y Rlbl. ;
TITLE1 'Means and Adjusted 95% Confidence Intervals for Differences';
TITLE2 "Catalyst Data" ;
run;
ods graphics off;
