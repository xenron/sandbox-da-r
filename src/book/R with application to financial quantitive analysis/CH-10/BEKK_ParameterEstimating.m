% Main Program for BEKK model
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% estimating BEKK model for two variables
% 'data' is a bivariate time series
% Code established by XUQIFA at 26th Oct. 2005.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clear;clc;
%  �������ݲ�������������
% P=load('data2005.txt');
% for i=1:11;
%     R(:,i)=log(P(:,i))-log(lagmatrix(P(:,i),1));             % ������������
% end
P=xlsread('stock.xls');
for i=1:2;
    R(:,i)=log(P(:,i))-log(lagmatrix(P(:,i),1));             % ������������
end
T=length(R(:,1));
R=R(2:T,:);                                                  % ȥ���ͺ����е�NA��

% % ��������ͳ����
% RMean=mean(R);
% RStd=std(R,0);
% RSkew=skewness(R);
% RKurt=kurtosis(R);
% for i=1:11;
%     [h(i),p(i),j(i)] = jbtest(R(:,i));
% end

%  ����BEKKģ�Ͳ�������
% data=[R(:,1),R(:,2),R(:,3)];
data=[R(:,1),R(:,2)];
p=1;q=1;
[parameters, loglikelihood, Ht, likelihoods, stdresid, stderrors, A, B, scores]  = full_bekk_mvgarch(data,p,q,[]);
% [parameters, loglikelihood, Ht, likelihoods, stdresid, stderrors, A, B, scores]  = full_bekk_mvgarch(data,p,q, BEKKoptions);

% ��ԭģ�͵Ĳ�������
N=size(data,2);                                                %����������
Nstar=N*(N+1)/2;
Cstar=ivech(parameters(1:Nstar));                              % ��ԭΪ������ʽ
Astar=reshape(parameters(Nstar+1:Nstar+N*N),N,N);
Bstar=reshape(parameters(Nstar+N*N+1:Nstar+2*N*N),N,N);
% Cstar=parameters(1:Nstar);                                   % ��ԭΪ������ʽ
% Astar=parameters(Nstar+1:Nstar+N*N);
% Bstar=parameters(Nstar+N*N+1:Nstar+2*N*N);

save ResultForBEKK;