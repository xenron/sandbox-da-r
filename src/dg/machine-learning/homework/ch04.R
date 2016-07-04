# 薛毅书9.2题
consume<-read.table("9.2.txt")
# 作线性回归
lm.sol<-lm(Y~X1+X2+X3+X4,data=consume)
summary(lm.sol)

结果如下：
Call:
lm(formula = Y ~ X1 + X2 + X3 + X4, data = consume)

Residuals:
        1         2         3         4         5         6         7 
0.024803  0.079476  0.012381 -0.007025 -0.288345  0.216090 -0.142085 
        8         9        10 
0.158360 -0.135964  0.082310 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)   
(Intercept) -17.66768    5.94360  -2.973  0.03107 * 
X1            0.09006    0.02095   4.298  0.00773 **
X2           -0.23132    0.07132  -3.243  0.02287 * 
X3            0.01806    0.03907   0.462  0.66328   
X4            0.42075    0.11847   3.552  0.01636 * 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.2037 on 5 degrees of freedom
Multiple R-squared:  0.9988,    Adjusted R-squared:  0.9978 
F-statistic:  1021 on 4 and 5 DF,  p-value: 1.827e-07

所得到的方程并不显著，因此需要进行主成份分析。
consume.pr<-princomp(consume[,1:4], cor=T)
summary(consume.pr, loadings=T)

结果如下：
Importance of components:
                          Comp.1      Comp.2     Comp.3       Comp.4
Standard deviation     1.9859037 0.199906992 0.11218966 0.0603085506
Proportion of Variance 0.9859534 0.009990701 0.00314663 0.0009092803
Cumulative Proportion  0.9859534 0.995944090 0.99909072 1.0000000000

Loadings:
   Comp.1 Comp.2 Comp.3 Comp.4
X1 -0.502 -0.237  0.579  0.598
X2 -0.500  0.493 -0.610  0.367
X3 -0.498 -0.707 -0.368 -0.342
X4 -0.501  0.449  0.396 -0.626

第1个主成分的贡献率就达到98.5%，说明1个因子就足够了

# 利用得出的主成份进行回归
consume.score<-consume.pr$score
consume$z1<-consume.score[,1]
lm.sol<-lm(Y~z1, data=consume)
summary(lm.sol)

回归结果为：
Call:
lm(formula = Y ~ z1, data = consume)

Residuals:
     Min       1Q   Median       3Q      Max 
-0.72237 -0.20946  0.05154  0.21032  0.81856 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 14.03000    0.16615   84.44 4.32e-13 ***
z1          -2.06119    0.08367  -24.64 7.87e-09 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.5254 on 8 degrees of freedom
Multiple R-squared:  0.987,     Adjusted R-squared:  0.9854 
F-statistic: 606.9 on 1 and 8 DF,  p-value: 7.873e-09

即回归方程为：Y=14.030-2.061*z1；

# 为了变回为原来自变量的形式，作如下转换
beta<-coef(lm.sol); A<-loadings(consume.pr)
x.bar<-consume.pr$center; x.sd<-consume.pr$scale
coef<-(beta[2]*A[,1])/x.sd
beta0 <- beta[1]- sum(x.bar * coef)
c(beta0, coef)

(Intercept)           X1           X2           X3           X4 
-23.77771861   0.02992643   0.13365158   0.08361156   0.16965187 

即回归方程为：
Y=-23.77771861 + 0.02992643*X1 + 0.13365158*X2 + 0.08361156*X3 + 0.16965187*X4 

#薛毅书9.3题
## 输入相关矩阵. 
x<- c(1.000, 0.846, 0.805, 0.859, 0.473, 0.398, 0.301, 0.382,
      0.846, 1.000, 0.881, 0.826, 0.376, 0.326, 0.277, 0.277, 
      0.805, 0.881, 1.000, 0.801, 0.380, 0.319, 0.237, 0.345, 
      0.859, 0.826, 0.801, 1.000, 0.436, 0.329, 0.327, 0.365, 
      0.473, 0.376, 0.380, 0.436, 1.000, 0.762, 0.730, 0.629, 
      0.398, 0.326, 0.319, 0.329, 0.762, 1.000, 0.583, 0.577, 
      0.301, 0.277, 0.237, 0.327, 0.730, 0.583, 1.000, 0.539, 
      0.382, 0.415, 0.345, 0.365, 0.629, 0.577, 0.539, 1.000)
names<-c("身高 x1", "手臂长 x2", "上肢长 x3", "下肢长 x4", "体重 x5", 
         "颈围 x6", "胸围 x7", "胸宽 x8")
R<-matrix(x, nrow=8, dimnames=list(names, names))

source("factor.analy.R")
fa<-factor.analy(R, m=2, method="princomp")
vm1<-varimax(fa$loadings, normalize = F); vm1

fa<-factor.analy(R, m=2, method="factor")
vm2<-varimax(fa$loadings, normalize = F); vm2

fa<-factor.analy(R, m=2, method="likelihood")
vm3<-varimax(fa$loadings, normalize = F); vm3

其实，三种方法结果都类似，以下是主成份法的结果：
$loadings

Loadings:
   Factor1 Factor2
X1 -0.913   0.232 
X2 -0.939   0.168 
X3 -0.929   0.136 
X4 -0.911   0.201 
X5 -0.282   0.871 
X6 -0.210   0.827 
X7 -0.137   0.828 
X8 -0.227   0.754 

               Factor1 Factor2
SS loadings      3.603   2.839
Proportion Var   0.450   0.355
Cumulative Var   0.450   0.805

$rotmat
          [,1]       [,2]
[1,] 0.7888429 -0.6145949
[2,] 0.6145949  0.7888429

第一公共因子中，系数绝对值较大的变量主要是：身高、手臂长、上肢长、下肢长，这些主要表现为身材的魁梧程度。
第二公共因子中，系数绝对值较大的变量主要是：体重、颈围、胸围、胸宽，这些主要表现为身材的肥胖程度。