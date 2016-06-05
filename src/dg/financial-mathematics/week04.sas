%LET _CLIENTTASKLABEL='week04';
%LET _CLIENTPROJECTPATH='E:\金融数学\week04.egp';
%LET _CLIENTPROJECTNAME='week04.egp';
%LET _SASPROGRAMFILE=;

GOPTIONS ACCESSIBLE;
/* 久期与修正久期计算 程序一： 直接根据公式计算*/
data a;
c2=0;
tc2=0;
do n=1 to 10;
t=n;
if n<10 then c=5 ;
else if n=10 then c=105 ;
a=1/((1+0.05)**n);
c1=c/((1+0.05)**n);
tc1=t*c1;
c2=c2+c/((1+0.05)**n);
tc2=tc2+t*c/((1+0.05)**n);
if n=10 then 
d=tc2/(c2*2); 
md=d/(1+0.05);
output;
end;
data b;
set a;
drop c2 tc2 n;
label
t='时间' 
c='现金流'
a='1美元的现值' 
c1='现金流的现值' 
tc1='t*pvcf' 
d='久期(以年计)'
md='修正久期';
proc print data=b label noobs;
title '久期及修正久期';
var d  md;
run;

/* 程序二：编写宏函数*/
%macro d(y,cupon,period,p0);
data a;
c2=0;
tc2=0;
do n=1 to &period;
t=n;
if n<&period then c=&cupon;
else if n=&period then c=&cupon+&p0;
a=1/((1+&y)**n);
c1=c/((1+&y)**n);
tc1=t*c1;
c2=c2+c/((1+&y)**n);
tc2=tc2+t*c/((1+&y)**n);
if n=&period then
d=tc2/(c2*2);
md=D/(1+&y);
put d= md= ;
output;
drop n tc2 c2;
end;
%mend d;
%d(0.05,5,10,100) ;
run;


/*  修正久期的近似计算*/

/*收益率上升或下降20个基本点的债券初始价格计算*/
data a;
delete;
%macro a(n,y,cupon,par);
data a1;
p1=0;
%do i=1 %to &n;
p1=p1+&cupon*&par/(1+&y)**&i;
output;
%end;
data a1;
set a1 end=lasobs;
if lasobs;
p2=&par/(1+&y)**&n;
p=p1+p2;
y=200*&y;
y1=100*&y;
data a;
set a a1;
put p=;
%mend a;
%a(40,0.05,0.035,100);
%a(40,0.051,0.035,100);
%a(40,0.049,0.035,100);
run;

/*近似久期计算*/
%macro md(Vu,Vd,V,y);
data a;
md=(&vu-&vd)/(2*&v*&y);
put md=;
%mend md;
%md(75.64,72.92,74.26,0.002);
run;

/*精确值计算*/
%macro d(y,cupon,period,p0);
data a;
c2=0;
tc2=0;
do n=1 to &period;
t=n;
if n<&period then c=&cupon;
else if n=&period then c=&cupon+&p0;
a=1/((1+&y)**n);
c1=c/((1+&y)**n);
tc1=t*c1;
c2=c2+c/((1+&y)**n);
tc2=tc2+t*c/((1+&y)**n);
if n=&period then
d=tc2/(c2*2);
md=D/(1+&y);
put d= md= ;
output;
drop n tc2 c2;
end;
%mend d;
%d(0.05,3.5,40,100) ;
run;

/*  凸度计算  */
%macro d(y,cupon,period,p0);                                                                                                           
data a;                                                                                                                                 
c2=0;                                                                                                                                   
tc2=0;                                                                                                                                  
do n=1 to &period;                                                                                                                      
t=n;                                                                                                                                    
if n<&period then c=&cupon;                                                                                                             
else if n=&period then c=&cupon+&p0;                                                                                                    
a=1/((1+&y)**n);                                                                                                                        
c1=c/((1+&y)**n);                                                                                                                       
tc1=t*(t+1)*c1;                                                                                                                               
c2=c2+c/((1+&y)**n);                                                                                                                    
tc2=tc2+t*(t+1)*c/((1+&y)**n);                                                                                                                
if n=&period then                                                                                                                                                                                                                                
concave=tc2/(c2*((1+&y)**2));
yearlyconcave=concave/4;  
put concave= ;
put yearlyconcave=;
output;                                                                                                                                 
drop n tc2 c2;                                                                                                                          
end; 
proc print  data=a;                                                                                                                                   
%mend d;                                                                                                             
%d(0.05,4,10,100) ;
run;

/* 零息债券*/
%macro concave(n,y);
data a;
concave=&n*(&n+1)/((1+&y)**2);
put concave=;
%mend concave;
%concave(10,0.05);
run;


/* 计算凸度引起的价格变化 */
%macro vp(x,y);
data a;
caused=0.5*&x*(&y**2)*100;
put caused ='%';
%mend vp;
%vp(19.57356,0.03);
run;



/*近似凸度*/
%macro concave(Vu,Vd,V,y);
data a;
yearlyconcoave=(&vu+&vd-2*&v)/(&v*(&y**2));
put yearlyconcoave=;
%mend concave;
%concave(75.64,72.92,74.26,0.002);
run;

/*精确凸度的计算*/
%macro d(y,cupon,period,p0);                                                                                                           
data a;                                                                                                                                 
c2=0;                                                                                                                                   
tc2=0;                                                                                                                                  
do n=1 to &period;                                                                                                                      
t=n;                                                                                                                                    
if n<&period then c=&cupon;                                                                                                             
else if n=&period then c=&cupon+&p0;                                                                                                    
a=1/((1+&y)**n);                                                                                                                        
c1=c/((1+&y)**n);                                                                                                                       
tc1=t*(t+1)*c1;                                                                                                                               
c2=c2+c/((1+&y)**n);                                                                                                                    
tc2=tc2+t*(t+1)*c/((1+&y)**n);                                                                                                                
if n=&period then  
concave=tc2/(c2*((1+&y)**2));
yearlyconcave=concave/4;  
put concave= ;
put yearlyconcave=;
output;                                                                                                                                 
drop n tc2 c2;                                                                                                                          
end; 
proc print  data=a;                                                                                                                                   
%mend d;                                                                                                             
%d(0.05,3.5,40,100) ;
run;

/*债券组合收益率*/
%macro r(v1,v0,d);
data;
r=(&v1-&v0+&d)/&v0;
put r=;
%mend r(v1,v0,d);
%r(2.2,1.99,0.737);
run;

/*算术平均时序收益率程序*/
%macro r(r1,r2,r3,r4,n);
data a;
r=(&r1+&r2+&r3+&r4)/&n;
put r=;
%mend R(r1,r2,r3,r4,n);
%r(0.12,0.25,-0.15,-0.02,4);
run;
/*时间加权几何平均时序收益率程序*/
%macro r(r1,r2,r3,r4,n);
data a;
r=((&r1+1)*(&r2+1)*(&r3+1)*(&r4+1))**(1/&n)-1;
put r=;
%mend R(r1,r2,r3,r4,n);
%r(0.12,0.25,-0.15,-0.02,4);
run;



/*单个股票波动率计算  */
/*读取数据文件*/
DATA WORK.HSI;
    LENGTH
        Date               8
        'HSI.Open'n        8
        'HSI.High'n        8
        'HSI.Low'n         8
        'HSI.Close'n       8
        'HSI.Volume'n      8
        'HSI.Adjusted'n    8 ;
    FORMAT
        Date             YYMMDD10.
        'HSI.Open'n      BEST8.
        'HSI.High'n      BEST8.
        'HSI.Low'n       BEST8.
        'HSI.Close'n     BEST8.
        'HSI.Volume'n    BEST10.
        'HSI.Adjusted'n  BEST8. ;
    INFORMAT
        Date             YYMMDD10.
        'HSI.Open'n      BEST8.
        'HSI.High'n      BEST8.
        'HSI.Low'n       BEST8.
        'HSI.Close'n     BEST8.
        'HSI.Volume'n    BEST10.
        'HSI.Adjusted'n  BEST8. ;
    INFILE 'C:\Users\Administrator\AppData\Local\Temp\SEG7352\HSI-f6ded34a1a3c4088b1538921b474db77.txt'
        LRECL=66
        ENCODING="EUC-CN"
        TERMSTR=CRLF
        DLM='7F'x
        MISSOVER
        DSD ;
    INPUT
        Date             : ?? YYMMDD10.
        'HSI.Open'n      : ?? COMMA8.
        'HSI.High'n      : ?? COMMA8.
        'HSI.Low'n       : ?? COMMA8.
        'HSI.Close'n     : ?? COMMA8.
        'HSI.Volume'n    : ?? BEST10.
        'HSI.Adjusted'n  : ?? COMMA8. ;
RUN;



/*程序：日对数收益率计算*/
options nodate nonotes nosource;
data log_ret(keep=Date r_1 r_2);
set WORK.HSI;
r_1=log(HSI.Adjusted)-log(lag(HSI.Adjusted)); 
if r_1=. then r_1=0;
r_2=r_1**2;
run;

             
/*程序 GARCH(1,1) 计算的波动率*/
data garch(keep=Date);
set log_ret;
%macro a(x);
proc autoreg data=log_ret; 
model r_1= / nlag=1 garch=(q=1,p=1,tr);
output out=out cev=cev ;
data out(keep=Date cev&x);
set out;
cev&x=sqrt(cev);
data garch;
merge garch out log_ret;
by Date;
%mend a;
%a(HSI);
run;

%macro a(x);
proc gplot data=garch;
plot cev&x*Date=1;
symbol1 v=none i=join r=1 c= black;
%mend a;
%a(HSI);
run;

data c;
merge log_ret garch;
by Date;
r_1=abs(r_1);
run;
%macro a(x);
proc gplot data=c;
plot cev&x*Date=1 r_1*Date=2 /overlay;
symbol1 v=none i=join r=1 c=black line=1;
symbol2 v=none i=join r=1 c=black line=20;
%mend a;
%a(HSI);
run;



GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;
%LET _SASPROGRAMFILE=;

