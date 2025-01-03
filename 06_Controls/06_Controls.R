library(ggplot2)
library(kableExtra)
oj <- read.csv("D:/Documents/Data/DSHR/oj.csv")
basefit <- lm(log(sales) ~ log(price), data=oj)
coef(basefit)

brandfit <- lm(log(sales) ~ brand + log(price), data=oj)
coef(brandfit)

pricereg <- lm(log(sales) ~ brand, data=oj)
phat <- predict(pricereg, newdata=oj) 
presid <- log(oj$price) - phat
residfit <- lm(log(sales) ~ presid, data=oj)
coef(basefit)

data <- read.table("D:/Documents/Data/DSHR/abortion.dat", skip=1, sep="\t")
names(data) <- c("state","year","pop","y_viol","y_prop","y_murd",
                 "a_murd","a_viol","a_prop",'prison','police',
                 'ur','inc','pov','afdc','gun','beer')
data <- data[!(data$state%in%c(2,9,12)),] # AK, DC, HA are strange places
data <- data[data$year>84 & data$year<98,] # incomplete data outside these years
data$pop <- log(data$pop)
t <- data$year - 85
s <- factor(data$state) ## states are numbered alphabetically
controls <- data.frame(data[,c(3,10:17)])
## y is de-trended log crime rate, a is as described below
## note we also have violent and property crime versions
y <- data$y_murd
d <- data$a_murd

summary(orig <- glm(y ~ d + t + s +., data=controls) )$coef['d',]

dcoef <- summary(orig <- glm(y ~ d + t + s +., data=controls) )$coef['d',][1]

exp(dcoef) - 1

data <- read.table("D:/Documents/Data/DSHR/abortion.dat", skip=1, sep="\t")
names(data) <- c("state","year","pop","y_viol","y_prop","y_murd",
                 "a_murd","a_viol","a_prop",'prison','police',
                 'ur','inc','pov','afdc','gun','beer')
data <- data[!(data$state%in%c(2,9,12)),] # AK, DC, HA are strange places
data$pop <- log(data$pop)
t <- data$year - 85
s <- factor(data$state) ## states are numbered alphabetically
controls <- data.frame(data[,c(3,10:17)])
## y is de-trended log crime rate, a is as described below
## note we also have violent and property crime versions
y <- data$y_murd
d <- data$a_murd

summary(orig <- glm(y ~ d + t + s +., data=controls) )$coef['d',]

dcoef <- summary(orig <- glm(y ~ d + t + s +., data=controls) )$coef['d',][1]

exp(dcoef) - 1

cell <- read.csv("D:/Documents/Data/DSHR/us_cellphone.csv")
cellrate <- 5*cell[,2]/(1000*cell[,3]) # center on 1985 and scale by 1997-1985

par(mai=c(.9,.9,.1,.1))
plot(1985:1997, tapply(d, t, mean), bty="n", xlab="year", ylab="rate", pch=21, bg=2)
points(1985:1997, cellrate, bg=4, pch=21)
legend("topleft", fill=c(2,4), legend=c("abortions","cellphones"), bty="n")
phone <- cellrate[ t + 1 ]
tech <- summary(glm(y ~ phone + t + s +., data=controls))$coef['phone',]
phonecoef <- tech[1]
exp(phonecoef) - 1

t <- factor(t)
interact <- glm(y ~ d + t + phone*s + .^2, data=controls)
summary(interact)$coef["d",]

library(gamlr)
## refactor state to have NA reference level
sna <- factor(s, levels=c(NA,levels(s)), exclude=NULL)
x <- sparse.model.matrix( ~ t + phone*sna + .^2, data=controls)[,-1]
dim(x)

treat <- cv.gamlr(x,d, lmr=1e-3); head(summary(treat))
predtreat <- predict(treat, x, select="min"); head(predtreat)
dhat <- drop(predtreat); length(dhat)

par(mai=c(.9,.9,.1,.1))
plot(dhat,d,bty="n",pch=21,bg=8, cex=.8, yaxt="n")
axis(2, at=c(0,1,2,3)) 

## IS R^2?
cor(drop(dhat),d)^2
## Note: IS R2 indicates how much independent signal you have for estimating 
coef(summary( glm( y ~ d + dhat) ))
# re-run lasso, with this (2nd column) included unpenalized (free=2)
causal <- cv.gamlr(cbind(d,dhat,x),y,free=2,lmr=1e-3)
coef(causal, select="min")["d",] 
# AICc says abortion rate has no causal effect on crime.

library(gamlr)
data(hockey)
head(goal, n=2)
player[1:2, 2:7] #players on ice. +1 is home players. 0 is off ice. 
team[1, 2:6] #Sparse Matrix with indicators for each team*season interaction: +1 for home team, -1 for away team. 
config[5:6, 2:7] #Special teams info. For example, S5v4 is a 5 on 4 powerplay, +1 if it is for the home-team and -1 for the away team.
## naive lasso regression
naive <- cv.gamlr(cbind(d,x),y); head(coef(naive))
coef(naive)["d",] 

x <- cbind(config,team,player)
y <- goal$homegoal
fold <- sample.int(2,nrow(x),replace=TRUE) 
head(fold)
nhlprereg <- gamlr(x[fold==1,], y[fold==1],
                   free=1:(ncol(config)+ncol(team)), 
                   family="binomial", standardize=FALSE)
selected <- which(coef(nhlprereg)[-1,] != 0)
xnotzero <- as.data.frame(as.matrix(x[,selected]))
nhlmle <- glm( y ~ ., data=xnotzero, 
               subset=which(fold==2), family=binomial )