% Program for VIRF caculation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% VIRF based on research by Hafner(1998)
% VIRF of VGARCH model based on BEKK model
% Code established by XUQIFA at 26th Oct. 2005.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clear;clc;
%  �����Ѿ�����ı����ļ�
load ResultForBEKK.mat;

% ���������ʵ�ͼ��
for i=1:498;
  Vol1(i)=Ht(1,1,i);
  Vol2(i)=Ht(2,2,i);
end
figure;
subplot(1,2,1);plot(Vol1);title('��֤��ָ���������ʹ���');
subplot(1,2,2);plot(Vol2);title('��֤��ָ���������ʹ���');

% ���㲨��������Ӧ����
T=20;
C=zeros(3,1);
C(1)=Cstar(1,1)^2;
C(2)=Cstar(1,1)*Cstar(2,1);
C(3)=Cstar(2,1)^2+Cstar(2,2)^2;
A1=BEKK2VGARCH(Astar);
B1=BEKK2VGARCH(Bstar);


InitKesai1=[-2:0.1:2]';
I=length(InitKesai1);
InitKesai2(I,1)=0;
InitKesai=[InitKesai1(:),InitKesai2(:)];
vechSigma=inv(eye(Nstar)-A1-B1)*C;
Sigma=ivech(vechSigma);
for i=1:I;
    vechEpxlon(:,i)=vech(sqrtm(Sigma)*InitKesai(i,:)'*InitKesai(i,:)*sqrtm(Sigma));
end

Evol(:,:,1)=eye(Nstar);
for i=1:I;
    for t=2:T+1;
        Evol(:,:,t)=Evol(:,:,t-1)*(A1+B1);               %  ���(A1+B1)^t
        VIRFt(i,t,:)=(eye(Nstar)-Evol(:,:,t-1))*vechSigma+Evol(:,:,t-1)*A1*vechEpxlon(:,i)+Evol(:,:,t-1)*B1*vechSigma;
    end
end

time=[1:1:T]';
figure;
subplot(3,1,1);mesh(time,InitKesai1,VIRFt(:,2:T+1,1));title('��֤��ָ����������������Ӧ');
subplot(3,1,2);mesh(time,InitKesai1,VIRFt(:,2:T+1,2));title('��֤��ָ����֤��ָ����Э������������Ӧ');
subplot(3,1,3);mesh(time,InitKesai1,VIRFt(:,2:T+1,3));title('��֤��ָ����������������Ӧ');
