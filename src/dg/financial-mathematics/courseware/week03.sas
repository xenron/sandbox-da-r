libname ResDat 'D:\ResDat';

/*股票类样本数据 */

/*生成指数数据集，一只指数一个数据集*/
%macro a(code, nm);
data ResDat.Idx&code.(label="&nm.|&code.");
set ResDat.idxqttn;
if idxcd="&code";
%mend a;
%a(000001,上证指数);
%a(000002,上证A股);
%a(000010,上证180指数);
%a(399001,深圳成指);
%a(399106,深圳综指);
%a(HSI,香港恒生指数);
%a(I01021,日经225指数);
%a(I05051,道琼斯工业平均指数);
%a(I05091,纽约标普500指数);
run;

/*生成个股数据集，一只股票一个数据集*/
/*生成宏文本*/
data a;/*生成一个临时的a，用以生成宏代码*/
set ResDat.lstkinfo;
file "sampstock.txt";
a='%a(';
b=',';
c=');';
put a $ stkcd $ b $ lstknm $ c $;
run;

%macro a(a1,a2);/*这个宏就是一个set，if，把大表拆成小表*/
data ResDat.stk&a1.(label="&a2.|&a2.");
set ResDat.Qttndist;
if stkcd="&a1.";
%mend a;
%include "sampstock.txt";
run;

/* 创建单期收益计算环境对期末收盘价加标识：*/
data a;
set ResDat.Idx000001;
year=year(date);
qtr=qtr(date);
month=month(date);
proc sort data=a;
by year qtr month;
run;
data bb;
set a;
last_y=last.year;    
last_q=last.qtr;    
last_m=last.month; 
by year qtr month;
run;

/*年收益计算*/
data r_year(keep=date r_pct r_log label="年收益"); 
set bb;
if last_y=1;
r_pct=dif(clpr)/lag(clpr); 
r_log=log(clpr)-log(lag(clpr));
proc print data=r_year;
run;


/*季收益计算*/
data r_qtr (keep=date r_pct r_log label="季收益");
set bb;
if last_q=1;
r_pct=dif(clpr)/lag(clpr);
r_log=log(clpr)-log(lag(clpr));
proc print data=r_qtr;
run;

/*月收益计算*/
data r_month (keep=date r_pct r_log label="月收益");
set bb;
if last_m=1; 
r_pct=dif(clpr)/lag(clpr);
r_log=log(clpr)-log(lag(clpr));
proc print data=r_month;
run;

/*周收益计算程序一：*/
data a;
set ResDat.Idx000001;
wd=weekday(date);
dif=dif(wd);
dif2=dif(date);
if (dif<0 and dif^=.)or dif2>=7 then index=1;
else index=0;
data a(keep=date clpr index);
set a;
date=lag(date);
clpr=lag(clpr);
if index=1;
data r_week(keep=date r_pct r_log);
set a;
r_pct=dif(clpr)/lag(clpr);
r_log=log(clpr)-log(lag(clpr));
proc print data=r_week;
run;

/*程序二：*/
data b;
set ResDat.Idx000001; 
wk=int((date-3)/7+2); 
proc sort;
by date;
run;

data b;
set b;
last_wk=last.wk;
by wk;
run;
data b(keep=date r_pct1 r_log1);
set b;
if last_wk=1;
r_pct1=dif(clpr)/lag(clpr);
r_log1=log(clpr)-log(lag(clpr));
proc print data=b;
run;

/*程序三：*/
data c;
set ResDat.Idx000001; 
last_wk= Wkflg; 
run;
data c(keep=date r_pct2 r_log2);
set c;
if last_wk=1;
r_pct2=dif(clpr)/lag(clpr);
r_log2=log(clpr)-log(lag(clpr));
proc print data=c;
run;

/*检测程序一、程序二和程序三的一致性：*/
data d;
merge r_week b;
by date;
data d;
merge d c;
by date;
if r_pct=r_pct1 and r_pct=r_pct2 then aa=1;
else aa=0;
run;
data e;
set d;
if aa=0;
run;

/* 日收益计算*/
data r_day (keep=date r_pct r_log label="日收益");
set ResDat.Idx000001;
r_pct=dif(clpr)/lag(clpr); 
r_log=log(clpr)-log(lag(clpr));
proc print data=r_day;
run;

/* 绘制收益图*/
proc gplot data=r_day ;
plot r_pct*date / vref=0;
plot r_log*date / vref=0;
run;
quit;


/*多期平均收益率计算  程序一*/
data a1;
set r_year;
where 1995 <= year(date) <= 2005;
proc print;
run;
proc transpose data=a1 out=a2;
var r_pct;
proc print;
run;
data a3(keep=am gm);
set a2;
c1=col1+1; 
c2=col2+1;
c3=col3+1; 
c4=col4+1; 
c5=col5+1;
c6=col6+1;
c7=col7+1; 
c8=col8+1;
c9=col9+1; 
c10=col10+1; 
c11=col11+1;  
gm=(c1*c2*c3*c4*c5*c6*c7*c8*c9*c10*c11)**(1/11)-1;
am=mean(of col1-col11);
proc print;
run;
%let t=%eval(2005-1995+1); 
data a4(keep=am gm);
set a2;
array col(&t) col1-col&t;
array c(&t) c1-c&t;
do i=1 to &t;
c(i)=col(i)+1;
end;
gm=(( c1*c2*c3*c4*c5*c6*c7*c8*c9*c10*c11)**(1/&t))-1; 
am=mean(of col1-col&t); 
proc print;
run;

/* 程序二*/
data a5 ;
set bb;
if last_y=1 and 1997<= year(date) <= 2005;
run;
data a6;
retain begin end;
set a5 end=lastobs;
if _n_=1 then begin=clpr; 
if lastobs then do;
end=clpr; 
output;
end;
data a6(keep=gm);
set a6;
t=2005-1997+1;
gm=(end/begin)**(1/t)-1;
proc print;
run; 

/*  由最新股票信息数据集创建宏文本*/
data _null_;
set ResDat. Lstkinfo;
a='%a(';
b=',';
c=');' ;
file "Stk.txt" ; 
put a $ stkcd $ b $ lstknm $ c $ ;
run;
Data _null_ ;
set ResDat. Lstkinfo;
if substr(stkcd,1,1) in ('6','9') or substr(stkcd,1,2)='99' ;
a='%a(';
b=',';
c=');' ;
file "SHStk.txt" ;
put a $ stkcd $ b $ lstknm $ c $ ;
run;
Data _null_ ;
set ResDat. Lstkinfo;
if substr(stkcd,1,1) ='0' or substr(stkcd,1,2)='20' ;
a='%a(';
b=',';
c=');' ;
file "SZStk.txt" ;
put a $ stkcd $ b $ lstknm $ c $ ;
run;

/* 由个股数据集目录文件创建宏文本*/



data a;
length files $9;
infile "d:\ResDat\outlist"; 
input files $ ;    
if files='stkcdref.' then delete;
run;
Data _null_;
set a;
stkcd=substr(files,4,6);
a='%a(';
c=');' ;
file " Stk1.txt";
put a $ stkcd $ c $ ;
run;

/*  多股票收益计算   程序一： */
options nodate nonotes nosource; 
data ResDat.lg_shanghai(keep=date);
set ResDat.Idx000001; 
where 1995<=year(date)<=2005; 
%macro a(x,y);

data a(keep=date r_1);
set ResDat.stk&x;
where 1995<=year(date)<=2005; 
adjclpr=clpr*Mcfacpr;
r_1=log(adjclpr)-log(lag(adjclpr));  

data ResDat.lg_shanghai(rename=(r_1=r&x));
merge ResDat.lg_shanghai a;
by date;
data ResDat.lg_shanghai;
set ResDat.lg_shanghai;
if r&x=. then r&x=0;
else r&x= r&x;
%mend a;
%include "SHStk.txt";
run;
options nodate nonotes nosource;
data ResDat.r_shanghai(keep=date);
set ResDat.Idx000001;
where 1995<=year(date)<=2005;
%macro a(x,y);
data a(keep=date r_1);
set ResDat.stk&x;
where 1995<=year(date)<=2005; 
adjclpr=clpr*Mcfacpr;  
r_1= (adjclpr-lag(adjclpr))/ lag(adjclpr);  
data ResDat.r_shanghai(rename=(r_1=r&x));
merge ResDat.r_shanghai a;
by date;
data ResDat.r_shanghai;
set ResDat.r_shanghai;
if r&x=. then r&x=0;
else r&x= r&x;
%mend a;
%include "SHStk.txt";
run;
options nodate nonotes nosource;
data ResDat.lg_shenzhen(keep=date);
set ResDat.Idx000001; 
where 1995<=year(date)<=2005;
%macro a(x,y);
data a(keep=date r_1);
set ResDat.stk&x;
where 1995<=year(date)<=2005; 
adjclpr=clpr*Mcfacpr;
r_1=log(adjclpr)-log(lag(adjclpr));  
data ResDat.lg_shenzhen(rename=(r_1=r&x));
merge ResDat.lg_shenzhen a;
by date;
data ResDat.lg_shenzhen;
set ResDat.lg_shenzhen;
if r&x=. then r&x=0;
else r&x= r&x;
%mend a;
%include "SZStk.txt";
run;
options nodate nonotes nosource;
data ResDat.r_shenzhen(keep=date);
set ResDat.Idx000001;
where 1995<=year(date)<=2005;
%macro a(x,y);
data a(keep=date r_1);
set ResDat.stk&x;
where 1995<=year(date)<=2005; 
adjclpr=clpr*Mcfacpr;
r_1= (adjclpr-lag(adjclpr))/ lag(adjclpr);  
data ResDat.r_shenzhen(rename=(r_1=r&x));
merge ResDat.r_shenzhen a;
by date;
data ResDat.r_shenzhen;
set ResDat.r_shenzhen;
if r&x=. then r&x=0;
else r&x= r&x;
%mend a;
%include "SZStk.txt";
run;

/* 程序二   */
data return;
set ResDat.Qttndist;
by stkcd date;
adjclpr=Mcfacpr*Clpr;
lag_adjclpr =lag(adjclpr);
if not first.stkcd then lagadjclpr =lag_adjclpr;
return=(adjclpr-lagadjclpr)/lagadjclpr;
keep stkcd lstknm date return;
run;

/*收益SAS数据集转换为EXCEL数据表 */
proc transpose data=ResDat.lg_shanghai out= lg_shanghai_tr;
run;
data lg_shanghai_tr_1;
set lg_shanghai_tr;
if _n_<=220;
run;
proc transpose data= lg_shanghai_tr_1 out= lg_shanghai_1;
data lg_shanghai_1;
set lg_shanghai_1;
format date yymmdd10.;
run;
data lg_shanghai_tr_2;
set lg_shanghai_tr;
if 221<=_n_<=440;
run;
proc transpose data= lg_shanghai_tr_2 out= lg_shanghai_2;
data lg_shanghai_2;
set lg_shanghai_2;
format date yymmdd10.;
run;
data lg_shanghai_tr_3;
set lg_shanghai_tr;
if 441<=_n_<=660;
run;
proc transpose data= lg_shanghai_tr_3 out= lg_shanghai_3;
data lg_shanghai_3;
set lg_shanghai_3;
format date yymmdd10.;
run;
data lg_shanghai_tr_4;
set lg_shanghai_tr;
if 661<=_n_;
run;
proc transpose data= lg_shanghai_tr_4 out= lg_shanghai_4;
data lg_shanghai_4;
set lg_shanghai_4;
format date yymmdd10.;
run;
proc export data= lg_shanghai_1 
outfile="d:\ResDat\lg_shanghai_1.xls"
dbms=excel2000 replace;
run;
proc export data= lg_shanghai_2 
outfile="d:\ResDat\lg_shanghai_2.xls" 
dbms=excel2000 replace;
run;
proc export data= lg_shanghai_3
outfile="d:\ResDat\lg_shanghai_3.xls"
dbms=excel2000 replace;
run;
proc export data= lg_shanghai_4
outfile="d:\ResDat\lg_shanghai_4.xls"
dbms=excel2000 replace;
run;

/*由最新股票信息数据集Lstkinfo创建宏文本 */
%macro a(x);
data a;
set ResDat.Lstkinfo;
if year(Lstdt)<&x; /* Lstdt为股票上市日期 */
data y%eval(&x)_list ;
set a;
a='%a(';
c=");";
file "AlistedBefore%str(&x).txt";
put a $ stkcd $ c $;
%mend a;
%a(1995);
run;

/*  随机抽股票 */
%macro a(x);
proc sql;  
   create view  _tmp_ as
      select *, ranuni(5) as _ran_ from y%eval(&x)_list
   order by calculated _ran_;
quit;

data  random;
set _tmp_(obs = 20);
drop _ran_;
a='%a(';
c=");";
file " random%str(&x).txt";
put a $ stkcd $ c $;
%mend a;
%a(1995);
run;

/*单个股票收益计算 */
data ResDat.r_port20(keep=date);
set ResDat.Idx000001;
where 1995<=year(date)<=2005;
%macro a(x);
data a(keep=date r_1);
set ResDat.stk&x;
where 1995<=year(date)<=2005; 
adjclpr=clpr*Mcfacpr;
r_1= (adjclpr-lag(adjclpr))/ lag(adjclpr);  
data ResDat.r_port20 (rename=(r_1=r&x));
merge ResDat.r_port20 a;
by date;
data ResDat.r_port20;
set ResDat.r_port20;
if r&x=. then r&x=0;
else r&x= r&x;
%mend a;
%include "Random1995.txt";
run;

/*    股票组合的随机赋权重  程序一：  */
data rv;
id=1;
retain _seed_ 3;
do _i_ = 1 to 20;
   w= 0 + (1 - 0) * ranuni(_seed_);
   output;
end;
drop _seed_ _i_;
data a(keep=sumw id);
set rv end=obs_last;
sumw+w;
if obs_last=1;
id=1;
data b(keep=w);
merge rv a;
by id ;
w=w/sumw;
run;

/* 程序二： */
proc iml;
rv=uniform(repeat(0,20,1));
sum=rv[+,];
b=rv/sum;
sumb=b[+,]; 
print sumb; 
create b from b;
append from b;
quit;

/*  组合收益计算   */
data a(drop=date);
set ResDat.r_port20;
if _n_=1 then delete; 
array s _numeric_;
do over s;
if s=. then s=0;
end;
proc iml;
use a;  
read all var _num_ into xx; 
create xx from xx;   
append from xx;
close xx;
use b;
read all into w;
create w from w;
append from w;
close w;
aaa=xx*w; 
create aaa from aaa; 
append from aaa;
close aaa;
show names; 
quit;
run;
data a;
set ResDat.r_port20;
if _n_=1 then delete;

data r_port20;
merge a aaa(rename=(col1=r_port20)) ;
run;



/*内部收益率IRR*/

data;
p=-2000;
array Cs{2} (800 1600);
r=finance("irr",p,of Cs{*});
run;

data;
p=-500;
array Cs{5} (120 120 120 120 120);
r=finance("irr",p,of Cs{*});
run;

data;
p=-100;
array Cs[6] (20 20  0  0  0 80);
r=finance("irr",p,of Cs{*});
r=(1+r)**3-1;
run;

/*到期收益率*/
data;
p=-2364;
array cs[40] ( 200 200 200 200 200 200 200 200 200 200 200 200 200 200 200 200 200 200 200 200 200 200 200 200 200 200
200 200 200 200 200 200 200 200 200 200 200 200 200 2700);
r=finance("irr",p,of cs[*]);
r=(1+r)**2-1;
run;

data;
p=-2271;
array cs[15] ( 95 95 95 95 95 95 95 95 95 95 95 95 95 95 1595);
r=finance("irr",p,of cs[*]);
r=(1+r)**2-1;
run;

/*有效年利率*/
data;
r=0.1;
n=2;
r_ef=finance("EFFECT",r,n);
run;

data;
r=0.1;
n=4;
r_ef=finance("EFFECT",r,n);
run;

/*资产组合的收益率*/
data;
p=-57259000;
array cs[14] (  2300000  2300000  2300000  2300000  2300000 32300000  1400000  1400000  1400000 11400000  1050000 1050000  1050000 21050000);
r=finance("irr",p,of cs[*]);
run;

/*浮动利率*/
data;
p=-99.3098;
array cs[12](5.4 5.4 5.4 5.4 5.4 5.4 5.4 5.4 5.4 5.4 5.4 105.4);
r=finance("irr",p,of cs[*]);
r=r*2-0.1;
run;