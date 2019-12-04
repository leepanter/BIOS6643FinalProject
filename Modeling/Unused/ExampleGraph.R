####	Example Graph	 ####
####	Script Name: ExampleGraph.R

#-------------------------------------------------------------------------#
####	Description:	 ####
# This script will generate an example data set on which an example analysis will be performed/


####	Script Dependencies	 ####

# Package Dependencies:
library(ggplot2)
library(gridExtra)
library(ggExtra)

# File Dependencies

# Set Working Directory
WD="/Users/lee/Documents/Lee/School/CU Denver/Fall 2019/BIOS 6643/FinalProject"
setwd(WD)

# Data Dependencies:


#-------------------------------------------------------------------------#
####	Begin Script	 ####
#-------------------------------------------------------------------------#
set.seed(123)

g1x=c() ; g1y=c()
g2x=c() ; g2y=c()
g3x=c() ; g3y=c()
g4x=c() ; g4y=c()
g5x=c() ; g5y=c()
g6x=c() ; g6y=c()

n=2500


g1x=rbinom(n,1,prob = 2/3)
g2x=rbinom(n,1,prob = 2/3)
g3x=rbinom(n,1,prob = 2/3)
g4x=rbinom(n,1,prob = 2/3)
g5x=rbinom(n,1,prob = 2/3)
g6x=rbinom(n,1,prob = 2/3)



g1x=floor(g1x*runif(n, min= 0, max = 2))
g2x=floor(g2x*runif(n, min= 0.5, max = 2.5))
g3x=floor(g3x*runif(n, min= 1.5, max = 3.5))
g4x=floor(g4x*runif(n, min= 2, max = 4))
g5x=floor(g5x*runif(n, min= 2.5, max = 4.75))
g6x=floor(g6x*runif(n, min= 3.5, max = 5.5))


g1y=rbinom(n,1,prob = 2/3)
g2y=rbinom(n,1,prob = 2/3)
g3y=rbinom(n,1,prob = 2/3)
g4y=rbinom(n,1,prob = 2/3)
g5y=rbinom(n,1,prob = 2/3)
g6y=rbinom(n,1,prob = 2/3)


g1y=floor(g1y*abs(g1x^2-g1x +rnorm(n, mean=0, sd=1)))
g2y=floor(g2y*abs((1/2)*g2x^2-(1/2)*g2x +rnorm(n, mean=0.5, sd=1)))
g3y=floor(g3y*abs((1/3)*g3x^2-(1/4)*g3x +rnorm(n, mean=1, sd=1)))
g4y=floor(g4y*abs((1/4)*g4x^2-(1/3)*g4x +rnorm(n, mean=1.5, sd=1)))
g5y=floor(g5y*abs((1/5)*g5x^2-(1/2)*g5x +rnorm(n, mean=2, sd=1)))
g6y=floor(g6y*abs((1/5)*g6x^2-(1/2)*g6x +rnorm(n, mean=2.5, sd=1)))

X=as.vector(t(c(g1x, g2x, g3x, g4x, g5x, g6x)))
Y=as.vector(t(c(g1y, g2y, g3y, g4y, g5y, g6y)))

group=as.factor(c(rep(1,times=n),
                  rep(2,times=n),
                  rep(3,times=n),
                  rep(4,times=n),
                  rep(5,times=n),
                  rep(6,times=n)))


df=data.frame(X,Y,group)

p=ggplot(df, aes(x=X, y=Y, color=group))+
  geom_jitter(alpha=0.65, position = position_jitter(width = 1, height = 0.5))+
  xlim(0,6)+
  ylim(0,8)
ggMarginal(p, df, X, Y, type="density", margins="both")

df1=subset(df, group=="1")
df2=subset(df, group=="2")
df3=subset(df, group=="3")
df4=subset(df, group=="4")
df5=subset(df, group=="5")
df6=subset(df, group=="6")

p1=ggplot(df1, aes(x=X, y=Y))+
  geom_jitter()+
  xlim(0,6)+
  ylim(0,8)


p2=ggplot(df2, aes(x=X, y=Y))+
  geom_jitter()+
  xlim(0,6)+
  ylim(0,8)


p3=ggplot(df3, aes(x=X, y=Y))+
  geom_jitter()+
  xlim(0,6)+
  ylim(0,8)


p4=ggplot(df4, aes(x=X, y=Y))+
  geom_jitter()+
  xlim(0,6)+
  ylim(0,8)


p5=ggplot(df5, aes(x=X, y=Y))+
  geom_jitter()+
  xlim(0,6)+
  ylim(0,8)


p6=ggplot(df6, aes(x=X, y=Y))+
  geom_jitter()+
  xlim(0,6)+
  ylim(0,8)




Pext=grid.arrange(ggMarginal(p1, df, X, Y, type="density", margins="both"),
                  ggMarginal(p2, df, X, Y, type="density", margins="both"),
                  ggMarginal(p3, df, X, Y, type="density", margins="both"),
                  ggMarginal(p4, df, X, Y, type="density", margins="both"),
                  ggMarginal(p5, df, X, Y, type="density", margins="both"),
                  ggMarginal(p6, df, X, Y, type="density", margins="both"),
                  nrow=2, ncol=3)

#save(df, file = "ExampleData.Rdata")
