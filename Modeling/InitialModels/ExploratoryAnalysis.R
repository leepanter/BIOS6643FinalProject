####	Exploratory Analysis	 ####
####	Script Name: ExploratoryAnalysis.R

#-------------------------------------------------------------------------#


####	Script Dependencies	 ####

# Package Dependencies:
library(ggplot2)
library(gridExtra)
library(ggExtra)
library(lme4)
# File Dependencies

# Set Working Directory
WD="/Users/lee/Documents/Lee/School/CU Denver/Fall 2019/BIOS 6643/FinalProject"
setwd(WD)

# Data Dependencies:


#-------------------------------------------------------------------------#
####	Begin Script	 ####
#-------------------------------------------------------------------------#
set.seed(123)
n=2500


g1x=c() ; g1y=c()
g2x=c() ; g2y=c()
g3x=c() ; g3y=c()
g4x=c() ; g4y=c()
g5x=c() ; g5y=c()
g6x=c() ; g6y=c()

g1x=rnorm(n, mean= 0, sd = 1)
g2x=rnorm(n, mean= 0, sd = 1)
g3x=rnorm(n, mean= 0, sd = 1)
g4x=rnorm(n, mean= 0, sd = 1)
g5x=rnorm(n, mean= 0, sd = 1)
g6x=rnorm(n, mean= 0, sd = 1)


g1yRan= rnorm(n, mean = -1, sd = 1) + rnorm(n, mean = 1, sd = 1)*g1x
g2yRan= rnorm(n, mean = -0.5, sd = 1) + rnorm(n, mean = 0.5, sd = 1)*g2x
g3yRan= rnorm(n, mean = 0, sd = 1) + rnorm(n, mean = 0, sd = 1)g3x
g4yRan= rnorm(n, mean = 0, sd = 1) + rnorm(n, mean = 0, sd = 1)*g4x
g5yRan= rnorm(n, mean = 0.5, sd = 1) + rnorm(n, mean = -0.5, sd = 1)*g5x
g6yRan= rnorm(n, mean = 1, sd = 1) + rnorm(n, mean = -1, sd = 1)*g6x

g1yFix=g1x-1+ rnorm(n, mean = -1, sd = 1)
g2yFix=0.5*g2x-0.5+ rnorm(n, mean = -0.5, sd = 1)
g3yFix=g3x+ rnorm(n, mean = 0, sd = 1)
g4yFix=g4x+ rnorm(n, mean = 0, sd = 1)
g5yFix=(-0.5)*g5x+0.5+ rnorm(n, mean = 0.5, sd = 1)
g6yFix=(-1)*g6x+1+ rnorm(n, mean = 1, sd = 1)

X=as.vector(t(c(g1x, g2x, g3x, g4x, g5x, g6x)))
YRan=as.vector(t(c(g1yRan, g2yRan, g3yRan, g4yRan, g5yRan, g6yRan)))
YFix=as.vector(t(c(g1yFix, g2yFix, g3yFix, g4yFix, g5yFix, g6yFix)))

group=as.factor(c(rep(1,times=n),
                  rep(2,times=n),
                  rep(3,times=n),
                  rep(4,times=n),
                  rep(5,times=n),
                  rep(6,times=n)))


df=data.frame(X,YRan,YFix,group)

p=ggplot(df, aes(x=X, y=YRan, color=group))+
  geom_point(alpha=0.5, shape=1)
ggMarginal(p, df, X, Y, type="density", margins="both")


p=ggplot(df, aes(x=X, y=YFix, color=group))+
  geom_point(alpha=0.5, shape=1)
ggMarginal(p, df, X, Y, type="density", margins="both")



# Random Data Models

lmod1=lm(YRan~group+X, data=df)
coef.int=rep(coef(lmod1)[1], times=6)
coef.int.subject=coef(lmod1)[2:6]
coef.int.subject=c(0,coef.int.subject)
coef.int=coef.int+coef.int.subject
plot(YRan~X, dat=df)
for(i in 1:6)
{
  abline(a=coef.int[i], b=coef(lmod1)[7], col=(20+i), lty=2, lwd=2)
}

lmod2=lm(YRan~group+group:X, data=df)
summary(lmod2)
coef.int=rep(coef(lmod2)[1], times=6)
coef.int.subject=coef(lmod2)[2:6]
coef.int.subject=c(0,coef.int.subject)
coef.int=coef.int+coef.int.subject
coef.slope=coef(lmod2)[7:12]
plot(YRan~X, dat=df)
for(i in 1:6)
{
  abline(a=coef.int[i], b=coef.slope, col=(20+i), lty=2, lwd=2)
}


lmod3=lmer(YRan~X+(1|group), data=df)
summary(lmod3)
fe=fixef(lmod3)
re=ranef(lmod3)
coef.int.fix=rep(fe[1], times=6)
coef.int=coef.int.fix+re$group$`(Intercept)`
plot(YRan~X, dat=df)
for(i in 1:6)
{
  abline(a=coef.int[i], b=fe[2], col=(20+i), lty=2, lwd=2)
}


lmod4=lmer(YRan~1+(1|group) + (X|group), data=df)
summary(lmod4)

fe=fixef(lmod4)
re=ranef(lmod4)

coef.int.fix=rep(fe[1], times=6)
coef.int=coef.int.fix+re$group$`(Intercept)`
plot(YRan~X, dat=df)
for(i in 1:6)
{
  abline(a=coef.int[i], b=fe[2], col=(20+i), lty=2, lwd=2)
}

