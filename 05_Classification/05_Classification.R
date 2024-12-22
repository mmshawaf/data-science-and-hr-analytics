library(ggplot2)
library(kableExtra)
library(MASS)

data(fgl)
dim(fgl)
head(fgl, n = 2)

par(mfrow=c(2,3))
plot(RI ~ type, data=fgl, col=c(grey(.2),2:6), las=2)
plot(Al ~ type, data=fgl, col=c(grey(.2),2:6), las=2)
plot(Na ~ type, data=fgl, col=c(grey(.2),2:6), las=2)
plot(Mg ~ type, data=fgl, col=c(grey(.2),2:6), las=2)
plot(Ba ~ type, data=fgl, col=c(grey(.2),2:6), las=2)
plot(Si ~ type, data=fgl, col=c(grey(.2),2:6), las=2)

x <- scale(fgl[,1:9]) # column 10 is class label, scale converts to mean 0 sd 1
apply(x,2,sd) # apply function sd to columns of x

nearest1 <- knn(train=x[-test,], test=x[test,], cl=fgl$type[-test], k=1)
nearest5 <- knn(train=x[-test,], test=x[test,], cl=fgl$type[-test], k=5)
nearest2 <- knn(train=x[-test,], test=x[test,], cl=fgl$type[-test], k=2)
nearest3 <- knn(train=x[-test,], test=x[test,], cl=fgl$type[-test], k=3)
nearest4 <- knn(train=x[-test,], test=x[test,], cl=fgl$type[-test], k=4)
data.frame(fgl$type[test],nearest1,nearest5,nearest2,nearest3,nearest4)

library(class) #has knn function 
test <- sample(1:214,10) #draw a random sample of 10 rows 
nearest1 <- knn(train=x[-test,], test=x[test,], cl=fgl$type[-test], k=1)
nearest5 <- knn(train=x[-test,], test=x[test,], cl=fgl$type[-test], k=5)
data.frame(fgl$type[test],nearest1,nearest5)
               
credit <- read.csv("D:/Documents/Data/DSHR/credit.csv")
## re-level the credit history and checking account status
credit$history = factor(credit$history, levels=c("A30","A31","A32","A33","A34"))
levels(credit$history) = c("good","good","poor","poor","terrible")
## a few others
credit$foreign <- factor(credit$foreign, levels=c("A201","A202"), labels=c("foreign","german"))
credit$rent <- factor(credit$housing=="A151")
credit$purpose <- factor(credit$purpose, levels=c("A40","A41","A42","A43","A44","A45","A46","A47","A48","A49","A410"))
levels(credit$purpose) <- c("newcar","usedcar",rep("goods/repair",4),"edu",NA,"edu","biz","biz",
credit <- credit[,c("Default", "duration", "amount",
                    "installment", "age", "history",
                    "purpose", "foreign", "rent")],4,10) #draw a random sample of 10 rows 

library(gamlr)
credx <- sparse.model.matrix(Default ~ . ^ 2, data=naref(credit)); colnames(credx)
default <- credit$Default
credscore <- cv.gamlr(credx, default, family="binomial")
par(mfrow=c(1,2))
plot(credscore$gamlr)
plot(credscore)

sum(coef(credscore, s="min")!=0) # min
sum(coef(credscore$gamlr)!=0) # AICc
sum(coef(credscore$gamlr, s=which.min(AIC(credscore$gamlr)))!=0) # AIC
# the OOS R^2
1 - credscore$cvm[credscore$seg.min]/credscore$cvm[1]
## What are the underlying default probabilities
## In sample probability estimates
pred <- predict(credscore$gamlr, credx, type="response")
pred <- drop(pred) # remove the sparse Matrix formatting
boxplot(pred ~ default, xlab="default", ylab="prob of default", col=c("pink","dodgerblue"))

rule <- 1/5 # move this around to see how these change
sum( (pred>rule)[default==0] )/sum(pred>rule) ## false positive rate at 1/5 rule
sum( (pred<rule)[default==1] )/sum(pred<rule) ## false negative rate at 1/5 rule

rule <- 1/10 # move this around to see how these change
sum( (pred>rule)[default==0] )/sum(pred>rule) ## false positive rate at 1/10 rule
sum( (pred<rule)[default==1] )/sum(pred<rule) ## false negative rate at 1/10 rule

rule <- 1/5
sum( (pred>rule)[default==1] )/sum(default==1) ## sensitivity
sum( (pred<rule)[default==0] )/sum(default==0) ## specificity

rule <- 1/10
sum( (pred>rule)[default==1] )/sum(default==1) ## sensitivity
sum( (pred<rule)[default==0] )/sum(default==0) ## specificity

library(glmnet)
xfgl <- sparse.model.matrix(type~.*RI, data=fgl)[,-1] #Design matrix includes chemical composition variables and all their interactions with refractive index (RI).
gtype <- fgl$type
glassfit <- cv.glmnet(xfgl, gtype, family="multinomial") #cross validation experiments
glassfit

plot(glassfit)
par(mfrow=c(2,3), mai=c(.6,.6,.4,.4)) 
plot(glassfit$glm, xvar="lambda")

B  <- coef(glassfit, select="min"); B ## extract coefficients
B <- do.call(cbind, B) 
colnames(B) <- levels(gtype) # column names dropped in previous command. This command adds them back.
DeltaBMg <- B["Mg", "WinNF"] - B["Mg", "WinF"]; DeltaBMg; #B is a matrix. Fixed Row. Vary Columns. k is Mg, a is WinNF, b is WinF. 
exp(DeltaBMg);
1 - exp(DeltaBMg)
DeltaBMg <- B["Mg", "WinNF"] - B["Mg", "WinF"]; DeltaBMg; #B is a matrix. Fixed Row. Vary Columns. k is Mg, a is WinNF, b is WinF. 
exp(DeltaBMg);
1 - exp(DeltaBMg)


plot(trueclassprobs ~ gtype, col="lavender", varwidth=TRUE,
     xlab="glass type", ylab="prob( true class )") 
