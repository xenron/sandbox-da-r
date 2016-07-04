###################

iris
lm_iris <- lm(Petal.Length ~ Petal.Width, data=iris)
print(lm_iris)
summary(lm_iris)


###################

longley
lm_longley <- lm(GNP.deflator ~ ., data=longley)
print(lm_longley)
summary(lm_longley)
kappa(lm_longley)

lm_longley <- lm(GNP.deflator ~ Employed + Population, data=longley)
summary(lm_longley)

###################

