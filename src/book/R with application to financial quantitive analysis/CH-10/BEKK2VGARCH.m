function out=BEKK2VGARCH(inp)
% ��BEKKģ�͵Ĳ��������VGARCHģ�͵Ĳ���ֵ
% �������Ϊ2*2����,�������Ϊ3*3�ľ���
if length(inp)~=2
    warning('������󲻷�Ҫ��');
    return;
else
    out=zeros(3,3);
    out(1,1)=inp(1,1)^2;
    out(1,2)=2*inp(1,1)*inp(2,1);
    out(1,3)=inp(2,1)^2;
    out(2,1)=inp(1,1)*inp(1,2);
    out(2,2)=inp(2,1)*inp(1,2)+inp(1,1)*inp(2,2);
    out(2,3)=inp(2,1)*inp(2,2);
    out(3,1)=inp(1,2)^2;
    out(3,2)=2*inp(1,2)*inp(2,2);
    out(3,3)=inp(2,2)^2;
end
