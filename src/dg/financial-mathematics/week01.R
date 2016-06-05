#收益率的计算
Sret<-function(v0=NULL,vt=NULL)
{
  return((vt-v0)/v0)
}