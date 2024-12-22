# Chapter 1 - Frequentist vs Bootstrap

browser <- read.csv("D:/Documents/Data/DSHR/web-browsers.csv")
dim(browser)
head(browser)

mean(browser$spend); var(browser$spend)/1e4; sqrt(var(browser$spend)/1e4)

B <- 1000
mub <- c()
for (b in 1:1000){
  samp_b <- sample.int(nrow(browser), replace=TRUE)
  mub <- c(mub, mean(browser$spend[samp_b]))
}
sd(mub)

B <- 1000
mub <- c()
for (b in 1:1000){
  samp_b <- sample.int(nrow(browser), replace=TRUE)
  mub <- c(mub, mean(browser$spend[samp_b]))
}
sd(mub)

B <- 1000
mub <- c()
for (b in 1:1000){
  samp_b <- sample.int(nrow(browser), replace=TRUE)
  mub <- c(mub, mean(browser$spend[samp_b]))
}
sd(mub)

B <- 1000
mub <- c()
for (b in 1:1000){
  samp_b <- sample.int(nrow(browser), replace=TRUE)
  mub <- c(mub, mean(browser$spend[samp_b]))
}
sd(mub)

B <- 1000
mub <- c()
for (b in 1:1000){
  samp_b <- sample.int(nrow(browser), replace=TRUE)
  mub <- c(mub, mean(browser$spend[samp_b]))
}
sd(mub)

B <- 5000
mub <- c()
for (b in 1:5000){
  samp_b <- sample.int(nrow(browser), replace=TRUE)
  mub <- c(mub, mean(browser$spend[samp_b]))
}
sd(mub)

B <- 5000
mub <- c()
for (b in 1:5000){
  samp_b <- sample.int(nrow(browser), replace=TRUE)
  mub <- c(mub, mean(browser$spend[samp_b]))
}
sd(mub)

B <- 5000
mub <- c()
for (b in 1:5000){
  samp_b <- sample.int(nrow(browser), replace=TRUE)
  mub <- c(mub, mean(browser$spend[samp_b]))
}
sd(mub)

B <- 5000
mub <- c()
for (b in 1:5000){
  samp_b <- sample.int(nrow(browser), replace=TRUE)
  mub <- c(mub, mean(browser$spend[samp_b]))
}
sd(mub)

B <- 5000
mub <- c()
for (b in 1:5000){
  samp_b <- sample.int(nrow(browser), replace=TRUE)
  mub <- c(mub, mean(browser$spend[samp_b]))
}
sd(mub)

h <- hist(mub)
xfit <- seq(min(mub), max(mub), length = 40) 
yfit <- dnorm(xfit, mean = mean(browser$spend), sd = sqrt(var(browser$spend)/1e4)) 
yfit <- yfit * diff(h$mids[1:2]) * length(mub) 
#can you explain why we need each term in the last expression? 
lines(xfit, yfit, col = "black", lwd = 2)

B <- 1000
betas <- c()
for (b in 1:1000){
  samp_b <- sample.int(nrow(browser), replace=TRUE)
  reg_b <- glm(log(spend) ~ broadband + anychildren, data=browser[samp_b,])
  betas <- rbind(betas, coef(reg_b))
}; head(betas, n=3)

cov(betas[,"broadband"], betas[,"anychildren"])

spendy <- glm(log(spend) ~ . -id, data=browser)
round(summary(spendy)$coef,2)
pval <- summary(spendy)$coef[-1, "Pr(>|t|)"]
pvalrank <- rank(pval)
reject <- ifelse(pval< (0.1/9)*pvalrank, 2, 1)
plot(pvalrank, pval, ylab="p-value", xlab="p-value rank", pch=16, col=reject)
lines(pvalrank, (0.1/9)*pvalrank)
dev.off()