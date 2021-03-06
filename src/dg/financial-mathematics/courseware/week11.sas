/*1.1 随机变量和的分布模拟  程序一*/
data a;
do x1=1 to 6;
do x2=1 to 6;
output;
end;
end;  

data a;
set a;
x=sum(x1,x2);

proc univariate data=a noprint;
var x;
histogram/normal (mu=est sigma=est);
run;
quit;

/* 程序二 */
data a;
do x1=1 to 6;
do x2=1 to 6;
do x3=1 to 6;
output;
end;
end;
end;  
run;
                                                                                          
data a;
set a;
x=sum(x1,x2,x3);
proc univariate data=a noprint;
var x;
histogram/normal (mu=est sigma=est);
run;
quit;

/* 程序三 */
%macro a(n);  
data rv;
do m= 1 to &n;
if m=0 then probb=probbnml(0.8, &n, 0);
else probb=probbnml(0.8, &n, m)-probbnml(0.8, &n, (m-1));
      output;
      end;                                                                                       
symbol1  i=needle   width=6 c=blue h=1 cells;                                                                                                  
proc gplot data=rv;
plot probb*m=1;
%mend a;
%a(10);
%a(15);
%a(20);
%a(25);
%a(50);
%a(100);
%a(200);
%a(1000);
run;

symbol;                                                                                                                                 
goptions ftext= ctext= htext=;
%macro a(n);
data rv;
retain _seed_ 0;
do _i_ = 1 to &n;
binom1 = ranbin(_seed_,&n,0.8); 
output;
end;
drop _seed_ _i_;
run;                                                                                                    
proc univariate data=rv noprint;                                                                                               
var binom1;                                                                                                                          
histogram/normal( mu=est sigma=est);                                                                                                                                    
inset normal ;                                                                                                                       
run;                                                                                                                                    
%mend a;
%a(10);
%a(15);
%a(20);
%a(25);
%a(50);
%a(100);
%a(200);
%a(1000);
run;

/*程序四  */
symbol;                                                                                                                                 
goptions ftext= ctext= htext=;
options nodate nonotes nosource;
data rv;
delete;
%macro b(y);
%do i=1  %to  %eval(&y);  
data a; 
retain _seed_ 0;  
do _i_ = 1 to 100;
exp&i = ranexp(_seed_)/6; 
output;
end;
drop _seed_ _i_;
data rv;
merge rv a; 
%end;
data rv(keep=s&y); 
set rv;
s&y=sum(of exp1-exp&y);
proc univariate data=rv   noprint;                                                                                               
var s&y;                                                                                                                          
histogram/ normal( mu=est sigma=est noprint );                                                                                                                                  
inset normal ;                                                                                                                       
run;                                                                                                                                    
%mend b;
%b(1);
%b(5);
%b(10);
%b(20);
%b(50);
%b(100);
%b(200);
%b(1000);
run;

/*1.2 随机变量均值的分布模拟  */
symbol;                                                                                                                                 
goptions ftext= ctext= htext=;
options nodate nonotes nosource;
data rv;
delete;
%macro b(y);
%do i=1  %to  %eval(&y);
data a; 
retain _seed_ 0;
do _i_ = 1 to 100;
exp&i = ranexp(_seed_)/6;
output;
end;
drop _seed_ _i_;
data rv;
merge rv a; 
%end;
data rv(keep=x&y);
set rv;
x&y=sum(of exp1-exp&y)/&y; 
goptions ftext=swiss ctext=black htext=1 cells;                                                                                         
symbol v=square c=blue h=1 cells;                                                                                                       
proc univariate data=rv   noprint;                                                                                               
var x&y;                                                                                                                          
histogram/normal( mu=est sigma=est noprint );                                                                                                                                  
inset normal ;                                                                                                                       
run;                                                                                                                                    
symbol;                                                                                                                                 
goptions ftext= ctext= htext=;
%mend b;
%b(5);
%b(10);
%b(20);
%b(50);
%b(100);
%b(200);
%b(1000);
run;

/*1.3 统计抽样中的分布模拟 */
symbol;                                                                                                                                 
goptions ftext= ctext= htext=;
options nodate nonotes nosource;
data rv;  
retain _seed_ 0;
do _i_ = 1 to 100;
exp = ranexp(_seed_)/6; 
output;
end;
drop _seed_ _i_;
data rv80;  
delete;
%macro b(y);
%do i=1  %to  %eval(&y);
data a;
set rv;
obs=_n_;
proc sql;
create view tmp as  
    select *, ranuni(0) as _ran_ from a  
    order by calculated _ran_; 
quit; 

data random;
set tmp(obs =80); 
drop _ran_;
rename exp=exp&i;
data rv80;
merge rv80 random;
%end;
data rv80(keep=s&y);
set rv80;
s&y=sum(of exp1-exp&y);
proc univariate data=rv80   noprint;                                                                                               
var s&y;                                                                                                                          
histogram/normal( mu=est sigma=est noprint );                                                                                                                                          
inset normal ;                                                                                                                       
run;                                                                                                                                    
%mend b;
%b(5);
%b(10);
%b(20);
%b(50);
%b(100);
%b(200);
%b(1000);
run;



/* 1.4 随机游动模型 程序一 */
data a;
mu=0;
p1=8.6;
sigma=1;
do time=-50 to 1000;
e=rannor(32585);
p=mu+p1+sigma*e;
if time>0 then output; 
p1=p; 
end;
run;

proc gplot data=a;
symbol1 v=point i=join c=blue; 
symbol2 v=none i=r ; 
plot p*time=1 p*time=2/overlay; 
run;

/*程序二  */
data a;
mu=0;
p1=-0.02;
sigma=0.5;
do time=-50 to 1000;
e=rannor(32585);
p=mu+p1+sigma*e;
if time>0 then output;
p1=p;
end;
run;

proc gplot data=a;
symbol1 v=point i=join c=blue;
symbol2 v=none i=r ;
plot p*time=1 p*time=2/overlay;
run;

/*1.5 异方差模型   */
data a;
mu=0;
p1=-0.02;
do time=-10 to 300;
sigma=1+(100<=time<=200); 
p=mu+p1+sigma*rannor(32585);;
if time>0 then output;
p1=p;
end;
run;

proc gplot data=a;
symbol1 v=star i=join c=blue;
symbol2 v=none i=r  ;
plot p*time=1 p*time=2/overlay;
run;

/* 1.6 ARIMA模型 */
data a;
r1=0.01;
e1=rannor(32585);
beta =-0.8;
do time=-50 to 1000;
e=rannor(32585);
r=r1+beta *e1+e;
if time>0 then output;
r1=r;
e1=e; 
end;
run;

proc gplot data=a;
symbol1 v=star i=join c=green;
symbol2 v=none i=r  ;
plot r*time=1 r*time=2/overlay;
run;

