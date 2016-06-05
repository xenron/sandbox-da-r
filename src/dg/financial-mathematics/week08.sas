libname ResDat 'D:\ResDat';
/* 创建数据集*/
data a;
set ResDat.bchmkir;
if code="B2W" and date>='1jul1998'd;
rename ir=rf;
keep ir date;run;

data b;
set ResDat.bankir;
if code="d1y" ;
run;

data c;
set b;
format date yymmdd10.;
if enddt=. Then enddt=date();
do date=begdt to enddt; 
output;
end;
run;

data d;
set c;
if date<'1jul1998'd;
keep date rf;
rf=ir*1.1; 
run;

data rfyr;
set d  a;
if date<='31dec2005'd;
year=year(date);
month=month(date);
run;

proc means data=rfyr noprint;
output out=rf mean=meanrf;
var rf;
class year month;run;

data rf;
set rf;
drop _type_ _freq_ ;
rf= meanrf/12;
if 1995<=year<=2005;
drop meanrf;;
run;

data Stk8;
input stkcd$6.;
cards;
000002
000007
000011
000016
600601
600604
600651
600653
;
run;

proc sort data=Stk8;
by stkcd;
proc sort data=ResDat.monret out=monret; 
by stkcd;
data return;
merge Stk8(in=stkflg)  monret;
by stkcd;
if stkflg;
if 1995<=year(date)<=2005;
keep stkcd date year month Monret;
year=year(date);
month=month(date);
run;

proc sort data=return;
by year month;

proc sort data=rf;
by year month;

data return1;
merge return rf;
by year month;
if Monret=. Or date=. Then delete;
run;

data monretm;
set ResDat.monretm;
If 1995<=year(date)<=2005 and Exchflg='0' and Mktflg='A';  
/* Exchflg='0'－沪、深两交易所；Mktflg='A' －A股票市场 */
Keep date Mretmc year month;
Year=year(date);
Month=month(date);
Run;

Proc sort data=return1;
By date;
Run;
data return2;
merge return1  monretm;
by year month;
if Mretmc=. Then delete;
run;

data return;
set return2;
r_rf=Monret-rf;
r_m=Mretmc-rf;
drop monret rf Mretmc;
run;

/*  月超额收益作图*/

/*	月超额收益散点图*/
proc plot data=return(where=(stkcd='000002')) vpct=80;
plot  r_rf*r_m='*' / href=0;  
title2  '股票万科A的超额收益对市场超额收益图';
quit;

 	/*月超额收益时序图*/
proc timeplot data=return(where=(stkcd='000002')) maxdec=5 ; 
plot r_rf='X'  r_m='M'  / overlay ;
id date;  /*ID语句打印输出了列表中的ID变量值*/
title2  '股票万科A的超额收益、市场超额收益时序图';
run;

/*CAPM拟合程序句法解释 */
proc reg data=return(where=(stkcd='000002'))  outest=capmest1;
model r_rf=r_m / dw spec;
output out=out1 r=r  p=p  l95=l  u95=u;
slope:test r_m=1;
title2 '万科A股票';
quit;

/* 预测值和实际值图*/
goptions reset=global gunit=pct cback=white border
    htitle=6 htext=3 ftext=swissb colors=(black);
goptions reset=symbol; 
proc sort data=out1 out=r_out ;
    by r_m;
run;
data regdata(keep=y_value pt_type r_m);
    set r_out;
    label pt_type='Observation Type';
    array regvar(4) r_rf  p  l u;
    array varlabel(4) $12.  _temporary_
          ('Actual' 'Predicted'  'Lower Limits'  'Upper Limits');
    do i=1 to 4;
       y_value=regvar(i);       pt_type=varlabel(i);
       output;
    end;
run;
proc gplot data=regdata;
    plot y_value*r_m=pt_type / haxis=axis1 vaxis=axis2
                                 hminor=4 vminor=4;
    symbol1 v=* h=3.5 pct font=swissb color=black r=1;
    symbol2 i=join font=swissb l=1 color=blue r=1;
    symbol3 i=join font=swissb l=1 color=green r=1;
    symbol4 i=join font=swissb l=1 color=red r=1;
    axis1 label=('r_m') order=(-.2 to .15 by .05);
    axis2 label=(angle=90 'r_rf')
           order=(-.5 to .5 by .25);
    title f=HWDMX001 '实际值和预测值'; 
    title2 f=HWDMX001 '带有上、下置信限';
run;
quit;

/* 残差自相关性的直观检验*/
proc plot data=out1 vpct=80;
plot r*date='*' / vref=0 vaxis=-.25 to .25 by .1
               haxis='31jan95'd to '31dec05'd by year;
title2 '残差对时间散点图';
quit;

/* 残差异方差性的直观检验  */
proc plot data=out1 vpct=80;
    plot   r*r_m='*' / vref=0 vaxis=-.25 to .25 by .1 ;
    title2 '残差对R_M的散点图';
quit;

/*  CAPM拟合的一般程序 */
proc sort data=return;
by stkcd;
proc reg data=return outest=capmest1 rsquare adjrsq cp;
by stkcd;
model r_rf=r_m/dw spec;
slope: test r_m=1;
run;
quit;

/* 直接输出检验统计量和结果到数据集  */
ods trace on/label listing;
proc reg data= return outest=capmest1  rsquare  adjrsq  cp ;
model r_rf=r_m / dw spec;
output out=out1  r=r   p=p  l95=l  u95=u;
quit;
ods trace off;
Ods listing close; 
Ods output ParameterEstimates(match_all persist=proc)= Estimates;
Ods output FitStatistics(match_all persist=proc)=FitStatistics;
Ods output SpecTest (match_all persist=proc)=SpecTest;
Ods output DWStatistic(match_all persist=proc)=DWStatistic;
proc reg data=return outest= capmest1 rsquare  adjrsq  cp;
by stkcd;
model r_rf = r_m / dw spec;
slope: test r_m=1;
run;
quit;
Ods listing; /*打开LISTING */

Data Estimates; 
Set Estimates Estimates1 Estimates2 Estimates3 Estimates4 Estimates5 
Estimates6 Estimates7 ;
Run;

Data Dwstatistic;
Set Dwstatistic Dwstatistic1 Dwstatistic2 Dwstatistic3 Dwstatistic4 Dwstatistic5 Dwstatistic6 Dwstatistic7;
Run;

Data Fitstatistics;
Set Fitstatistics Fitstatistics1 Fitstatistics2 Fitstatistics3 Fitstatistics4 Fitstatistics5 Fitstatistics6 Fitstatistics7;
Run;

Data Spectest;
Set Spectest Spectest1 Spectest2 Spectest3 Spectest4 Spectest5 Spectest6 Spectest7
;
Run;

/*打印输出结果数据集 */
data a;
set capmest1;
format _numeric_ 8.4;
run;
options nocenter;
proc print data=a ;
var intercept r_m _rsq_ _adjrsq_ ;
id stkcd;
run;
options nocenter;
proc print data= Estimates;
id stkcd;
var variable estimate stderr tvalue probt;
run;
options nocenter;
proc print Data= Fitstatistics;
var label1 cvalue1 label2 cvalue2;
id stkcd;
run;
options nocenter;
proc print Data= Dwstatistic;
id stkcd;
var label1 cvalue1 nvalue1;
quit;
options nocenter;
proc print data=Spectest;
id stkcd;
var df chisq probchisq ;
quit;

/* 拟合无截距回归模型 */
Ods listing close; /*关闭LISTING */ 
Ods output ParameterEstimates(match_all persist=proc)= Estimates;
Ods output FitStatistics(match_all persist=proc)=FitStatistics;
Ods output SpecTest (match_all persist=proc)=SpecTest;
Ods output DWStatistic(match_all persist=proc)=DWStatistic;
proc reg data=return outest=  capmest11 rsquare  adjrsq  cp;
by stkcd;
model r_rf  = r_m / noint dw spec; 
slope: test r_m=1;
run;
quit;
Ods listing; 

data a;
set  capmest11;
format _numeric_ 8.4;
run;
options nocenter;
proc print data=a ;
var  r_m _rsq_ _adjrsq_ ;
id stkcd;
run;

/* 残差分布的正态性检验*/
data  residuals (keep=date);
set return;
proc reg data=return;
by stkcd;
model r_rf= r_m / dw spec;
output out=out  r=r_r_rf;
data  residuals;
merge  residuals out;
run;

proc univariate data= residuals normal plot;
by stkcd;
var  r_r_rf;
run; 

/* 异方差残差的修正*/
proc plot data= Residuals(where=(stkcd='000002'))  vpct=80;
plot  r_r_rf*r_m='*' / vref=0 vaxis=-.25 to .25 by .1 ;
title2 '残差对R_M的散点图';
run;

proc plot data= Residuals(where=(stkcd='600651'))  vpct=80;
plot  r_r_rf*r_m='*' / vref=0 vaxis=-.25 to .25 by .1 ;
title2 '残差对R_M的散点图';
run;
 /*用R-M作加权变量进行回归*/
data a (keep= r_rf  r_rfw  r_m  r_mw);
set return;
where stkcd='000002';
r_rfw= r_rf/r_m;
r_mw=1/r_m;
run;

proc reg data=a;
model r_rfw = r_mw /noint dw spec;
output out=out  r=r;
run;

/* 输出CAPM回归的参数估计*/
proc reg data=return(where=(stkcd='000002')) outest=capmest1;
model r_rf = r_m / dw spec; 
run;
 
data capmest1;
set capmest1;
rename r_m=beta;
run;

proc print data=capmest1;
var _depvar_ intercept beta;
title2 '估计CAPM回归参数';
run;

/*预测股票超额收益和收益*/
data forecast;
set capmest1;
do f_r_m= -.02 to .07 by .01;
   pred=intercept+beta*f_r_m;
   rf=0.0018559375;
   return=pred+rf;
   output;
   end;
label    f_r_m='Future Market Risk Premium'
   pred='Predicted Stock Risk Premium'
  return='Predicted Future Stock Return';
run;

options nocenter;
proc print data=forecast(obs=10)  noobs label;
var  f_r_m  pred  return ;
title2 '万科A股票超额收益和收益的点估计';
run;

/* 计算期望收益*/
proc means data=return noprint;
by stkcd;
var r_m  r_rf;
    output out=m_out1 mean=m_mkt  m;
run;
data m_out1;
set m_out1;
drop _type_ _freq_;
proc print data=m_out1 noobs;
title2  '平均月超额收益';
run;

/* 使用证券市场线*/
proc reg data=return outest= capmest1 rsquare  adjrsq  cp;
by stkcd;
model r_rf = r_m / dw spec;
slope: test r_m=1;
run;
quit;

data m_out2;
    merge  capmest1(rename=(r_m=beta)) m_out1;
       sml=beta*m_mkt;  
    capm=intercept+beta*(m);
    devia=capm-sml;      
    if devia >= .01 then  action='buy ';
    else if devia <=-0.002 then action='sell';
    else action='hold';
run;
