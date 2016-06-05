libname ResDat 'D:\ResDat';
/* 11.1.1 创建数据集*/
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
data return;
merge resdat.monret stk8(in=samp); 
by stkcd;
if samp;
year=year(date);
month=month(date);
if 1995<=year<=2005;
keep stkcd date year month monret;
data monretm;
set resdat.monretm; 
year=year(date);
month=month(date);
If 1995<=year<=2005 and Exchflg='0' and Mktflg='A'; 
keep date year month Mretmc;
proc sort data=return;
by year month;
data return;
merge return monretm;
by year month;
proc sort data=return;
by stkcd year month;
run;

/*11.1.2 计算期望收益*/
proc means data=return noprint;
by stkcd;
var monret;
output out=m_out;
data m_out1a;
set m_out;
where _stat_='mean';
keep stkcd monret;
run;

/*11.1.3 风险度量*/
data m_out1b;
set m_out;
where _stat_='std';
keep stkcd monret;
rename monret=std;
label monret='月收益率标准差';
run;

/*11.1.4 计算最优投资组合权重*/
proc reg data=return outest=capmest11;
by stkcd;
model monret=Mretmc/dw;
quit;
data weight1;
input  _id_ : $10.  r000002 r000007 r000011 r000016 r600601 r600604 r600651 r600653   _type_ $  _rhs_ ;
cards;
exp_return  0.0247 0.0127 0.0124 0.008 0.0216 0.0068 0.0263 0.0144 max
beta   1.1185001891 1.3470123011 1.3185749233 1.0260654129 1.3509499965 0.9038958443  1.2559109335 1.2961143173  eq 1.2
sum_wts    1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 eq 1.0
available  1 1 1 1 1 1 1 1 upperbd .
available  0 0 0 0 0 0 0 0 lowerbd .
;
run;

/*11.1.5 增加权重限制条件*/
data weight2;
input  _id_ : $10.  r000002 r000007 r000011 r000016 r600601 r600604 r600651 r600653   _type_ $  _rhs_ ;
cards;
exp_return  0.0247 0.0127 0.0124 0.008 0.0216 0.0068 0.0263 0.0144 max  .
beta 1.1185001891 1.3470123011 1.3185749233 1.0260654129 1.3509499965 0.9038958443  1.2559109335 1.2961143173  eq 1.2
sum_wts    1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 eq 1.0
available  .3333 .3333 .3333 .3333 .3333 .3333 .3333 .3333 upperbd .
available  .05 .05 .05 .05 .05 .05 .05 .05 lowerbd .
;
run;
proc lp data=weight2 primalout=lp_out2;
title2 '多约束的投资组合权重';
run;
quit;