
clear;clc;
%  �������ݲ�������������
P=xlsread('CH-10-02.xls','sheet3');
for i=1:2;
    R(:,i)=diff(log(P(:,i)));             % ������������
end
T=length(R(:,1));
R=R(2:T,:);                                                  % ȥ���ͺ����е�NA��
T=length(R(:,1));

% % ��������ͳ����
% RMean=mean(R);
% RStd=std(R,0);
% RSkew=skewness(R);
% RKurt=kurtosis(R);
% for i=1:11;
%     [h(i),p(i),j(i)] = jbtest(R(:,i));
% end


%  ����BEKKģ�Ͳ�������
data=[R(:,1),R(:,2)];
p=1;q=1;
[parameters, loglikelihood, Ht, likelihoods, stdresid, stderrors, A, B, scores]  = full_bekk_mvgarch(data,p,q,[]);

% ��ԭģ�͵Ĳ�������
N=size(data,2);
Nstar=N*(N+1)/2;
B0_star=ivech(parameters(1:Nstar));                              % ��ԭΪ������ʽ
B1_star=reshape(parameters(Nstar+1:Nstar+N*N),N,N);
B2_star=reshape(parameters(Nstar+N*N+1:Nstar+2*N*N),N,N);

% ���������ʵ�ͼ��
for i=1:T;
  Vol1(i)=Ht(1,1,i);
  Vol2(i)=Ht(2,2,i);
end
figure;
subplot(1,2,1);plot(Vol1);title('��֤��ָ���������ʹ���');
subplot(1,2,2);plot(Vol2);title('��֤��ָ���������ʹ���');


B0=zeros(3,1);
B0(1)=B0_star(1,1)^2;
B0(2)=B0_star(1,1)*B0_star(2,1);
B0(3)=B0_star(2,1)^2+B0_star(2,2)^2;
B1=BEKK2VGARCH(B1_star);
B2=BEKK2VGARCH(B2_star);
[V,D]=eig(B1+B2)
% s=solve(' 0.6016+2*0.5767*x+0.5528*x^2=0')  %
% ��������2006-2010������Эͬ�����ԣ�����������λ��
s=solve('0.9991+2*0.0420*x+0.0021*x^2=0')   %
% ��������2006-2013��������Эͬ�����ԣ�ֻ����һ����λ������Ϊ�������
% s=solve(' 0.9988+2*-0.0488*x+0.0025*x^2=0') %
% ��������2007-2013��������Эͬ�����ԣ�����һ����λ��
%save ResultFor_CH10.2_ex10.9;
save F:\CH-10\Result B0 B1 B2 V D s

help  full_bekk_mvgarch
