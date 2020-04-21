title 'Compression Test Experiment';
data compression;
input stress tag reinf tc;
CARDS;
14.746	1	1	1
15.255	1	1	1
15.675	1	1	1
15.647	1	1	1
16.023	1	1	1
11.262	2	1	2
12.760	2	1	2
13.091	2	1	2
13.139	2	1	2
15.210	2	1	2
11.676	3	1	3
13.672	3	1	3
13.573	3	1	3
14.651	3	1	3
20.090	3	1	3
11.616	4	1	4
13.555	4	1	4
12.928	4	1	4
14.320	4	1	4
14.727	4	1	4
9.664	1	2	5
10.570	1	2	5
10.720	1	2	5
10.844	1	2	5
10.705	1	2	5
9.735	2	2	6
9.754	2	2	6
9.578	2	2	6
10.057	2	2	6
9.937	2	2	6
10.058	3	2	7
9.922	3	2	7
9.076	3	2	7
11.389	3	2	7
10.483	3	2	7
9.056	4	2	8
9.539	4	2	8
10.420	4	2	8
10.820	4	2	8
10.805	4	2	8

proc print;

label stress = 'Compressive Stress';
label tag = 'Tag modification Levels';
label reinf = 'Reinforcement Type';
label tc = 'Treatment Combination';

/* Raw data Plot*/
/*goptions reset=all;*/
axis1 label=(angle = 90);
axis2 offset=(5,5) minor=none;
symbol1 v=square c=black; symbol2 v=plus c= black;
proc gplot data = compression;
plot stress*tag=reinf / vaxis=axis1 haxis=axis2;
proc univariate data=compression plot; /*box plot*/
var stress;
by tc;

/* GLM Procedure to obtain model form- ANOVA*/
proc glm data=compression;
classes tag reinf;
model stress = tag | reinf;
ESTIMATE 'Kevlar vs Glass'
          reinf 1 -1;
CONTRAST 'Kevlar vs Glass'
          reinf 1 -1;
CONTRAST 'No Tag vs Tagged'
          tag 3 -1 -1 -1/ divisor = 3;
CONTRAST 'Complete Tag and Tag with Cuts'
          tag 2 -2 -1 -1/ divisor = 2;
lsmeans tag|reinf/ pdiff=all cl adjust=tukey alpha=0.1;
output out=compressionout p=yhat r=e rstudent=tres;
proc print;


/* Checking Model Assumptions*/
/* Constant Variance */
/* Normal Probability */
/*normal scores for resifuals*/
proc rank normal=blom out=enrm data=compressionout;
var e;
ranks enrm;

data compressionnew; set compressionout; set enrm;

proc corr data=compressionnew;
var e enrm;
/*Plots*/
axis1 label=(angle=90);
axis2 offset=(0,0) minor=none;
symbol v=dot c=black;
label e = 'Residuals';
label yhat = 'Estimated Compressive Stress';
label enrm = 'Normal Scores';

proc gplot data=compressionnew;
plot e*yhat/vref=0 vaxis=axis1 haxis=axis2;
plot e*tag/vref=0 vaxis=axis1 haxis=axis2;
plot e*reinf/vref=0 vaxis=axis1 haxis=axis2;
plot e*tc/vref=0 vaxis=axis1 haxis=axis2;
plot e*enrm/vaxis=axis1;

/*Modified Levene Test*/ /*check again*/
data compressionmod; set compressionnew;
group =1;
if tc>4 then group=2;
proc sort data=compressionmod;
by group;
proc univariate data=compressionmod noprint;
by group;
var e;
output out=mout median=mede;
proc print data=mout;
var group mede;

data mtemp; merge compressionmod mout;
by group;
d = abs(e-mede);
proc sort data = mtemp;
by group;

proc anova data=mtemp;
class group;
model d=group;
run;

/*check outliers*/
data outlier;
tinvtres = tinv(0.999875,31);
proc print;


/*Transformations*/
data compression2; set compression;
logY =log10(stress);

proc glm data = compression2;
classes tag reinf;
model logY = tag | reinf;
ESTIMATE 'Kevlar vs Glass'
          reinf 1 -1;
CONTRAST 'Kevlar vs Glass'
          reinf 1 -1;
CONTRAST 'No Tag vs Tagged'
          tag 3 -1 -1 -1/ divisor = 3;
CONTRAST 'Complete Tag and Tag with Cuts'
          tag 2 -2 -1 -1/ divisor = 2;
lsmeans tag|reinf/ pdiff=all cl adjust=tukey alpha=.01;
output out=compression2out p=yhat2 r=e2 rstudent=tres2;
proc print;

/*Residual Plots for Transformations*/
label logY = 'Transformed Compressive Stress';
label tag = 'Tag Modification';
label reinf = 'Reinforcement Type';
axis1 label=(angle=90);
axis2 offset=(5,5) minor = none;
symbol1 v=square c=black; symbol2 v=plus c= black;
proc gplot data = compression2;
plot logY*tag=reinf/ vaxis=axis1 haxis=axis2;
proc univariate data=compression2 plot; /*Box Plot*/
var logY;
by tc;

/*Model Assumptions for new model*/
/* Constant Variance */
/* Normal Probability */
/*normal scores for resifuals*/
proc rank normal=blom out=enrm2 data=compression2out;
var e2;
ranks enrm2;

data compression2new; set compression2out; set enrm2;

proc corr data=compression2new;
var e2 enrm2;
/*Plots*/
axis1 label=(angle=90);
axis2 offset=(0,0) minor=none;
symbol v=dot c=black;
label e2 = 'Residuals';
label yhat2 = 'Estimated Compressive Stress';
label enrm2 = 'Normal Scores';

proc gplot data=compression2new;
plot e2*yhat2/vref=0 vaxis=axis1 haxis=axis2;
plot e2*tag/vref=0 vaxis=axis1 haxis=axis2;
plot e2*reinf/vref=0 vaxis=axis1 haxis=axis2;
plot e2*tc/vref=0 vaxis=axis1 haxis=axis2;
plot e2*enrm2/vaxis=axis1;

/*Modified Levene Test*/ /*check again*/
data compression2mod; set compression2new;
group2 =1;
if tc>4 then group2=2;
proc sort data=compression2mod;
by group2;
proc univariate data=compression2mod noprint;
by group2;
var e2;
output out=mout2 median=mede2;
proc print data=mout2;
var group2 mede2;

data mtemp2; merge compression2mod mout2;
by group2;
d2 = abs(e2-mede2);
proc sort data = mtemp2;
by group2;

proc anova data=mtemp2;
class group2;
model d2=group2;
run;

/*check outliers*/
data outlier2;
tinvtres2 = tinv(0.999875,31);
proc print;

/*Log Transformation Used hereafter*/

/*Analysis of Variance*/
/*Interaction Plots*/


proc sort data=compression2; by tag reinf;
proc means data = compression2 noprint mean var;
var logY; by tag reinf;
output out=compressionout2 mean=avgY var=varY;
proc print;
var tag reinf avgY varY;
symbol1 v=square i=join c=black;
symbol2 v=plus i=join c=black;
symbol3 v=circle i=join c=black;
proc gplot data=compressionout2;
plot avgY*tag=reinf/ vaxis=axis1 haxis=axis2;
proc print;

/*/*Box Cox*/
*%include macros(boxglm);
*%boxglm(data=compression,
        resp=stress,
        model=tag reinf, classes=tag reinf, 
        /*outplot=compressionpow,*/
        gplot= RSME EFFECT INFL,
        lopower=-0.1, hipower=0.8, npower=19);
*proc print data=compressionpow;
/*run;*/
/*Weighted Least Square Method*/
/*

proc glm data=compression;
classes tc;
model stress = tc;
means tc;
output out=compout r=compe;
run;

/*calculate weights*/
/*
data temp;
set compression;
tc1=0;
if tc=1 then tc1=1;
tc2=0;
if tc=2 then tc2=1;
tc3=0;
if tc=3 then tc3=1;
tc4=0;
if tc=4 then tc4=1;
tc5=0;
if tc=5 then tc5=1;
tc6=0;
if tc=6 then tc6=1;
tc7=0;
if tc=7 then tc7=1;
tc8=0;
if tc=8 then tc9=1;
run;
/*
proc sql;
create table compressionwts as
select *, 1/(var(stress)) as w
from temp
group by tc;

/*factor effects model*/
/*
data facmod;
set compressionwts;
tau1=0;
tau2=0;
tau3=0;
tau4=0;
tau5=0;
tau6=0;
tau7=0;
if tc=1 then tau1=1;
if tc=2 then tau2=1;
if tc=3 then tau3=1;
if tc=4 then tau4=1;
if tc=5 then tau5=1;
if tc=6 then tau6=1;
if tc=7 then tau7=1;
if tc=8 then tau1=-1;
if tc=8 then tau2=-1;
if tc=8 then tau3=-1;
if tc=8 then tau4=-1;
if tc=8 then tau5=-1;
if tc=8 then tau6=-1;
if tc=8 then tau7=-1;
proc print data = facmod;
var stress tc tc1 tc2 tc3 tc4 tc5 tc6 tc7 tc8 w tau1 tau2 tau3 tau4 tau5 tau6 tau7;
run;

/*WLS via reference population model*/
/*
proc reg data=compressionwts;
weight w;
model stress = tc1-tc7;
run;

/*WLS via factor effects model*/
/*
proc reg data=facmod;
weight w;
model stress= tau1-tau7;
run;
*/

