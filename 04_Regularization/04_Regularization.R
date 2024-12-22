SC <- read.csv("D:/Documents/Data/DSHR/semiconductor.csv")
full <- glm(FAIL ~ ., data=SC, family=binomial)
1 - full$deviance/full$null.deviance

## pred must be probabilities (0<pred<1) for binomial
deviance <- function(y, pred, family=c("gaussian","binomial")){
  family <- match.arg(family)
  if(family=="gaussian"){
    return( sum( (y-pred)^2 ) )
  }else{
    if(is.factor(y)) y <- as.numeric(y)>1
    return( -2*sum( y*log(pred) + (1-y)*log(1-pred) ) )
  }
}

## get null devaince too, and return R2
R2 <- function(y, pred, family=c("gaussian","binomial")){
  fam <- match.arg(family)
  if(fam=="binomial"){
    if(is.factor(y)){ y <- as.numeric(y)>1 }
  }
  dev <- deviance(y, pred, family=fam)
  dev0 <- deviance(y, mean(y), family=fam)
  return(1-dev/dev0)
}

# setup the experiment
n <- nrow(SC) # the number of observations
K <- 20 # the number of `folds'
# create a vector of fold memberships (random order)
foldid <- rep(1:K,each=ceiling(n/K))[sample(1:n)]
# create an empty dataframe of results
Out <- data.frame(full=rep(NA,K)) 
# use a for loop to run the experiment
for(k in 1:K){ 
  train <- which(foldid!=k) # train on all but fold `k'
  
  ## fit regression on full sample
  rfull <- glm(FAIL~., data=SC, subset=train, family=binomial)
  
  ## get prediction: type=response so we have probabilities
  predfull <- predict(rfull, newdata=SC[-train,], type="response")
  
  ## calculate and log R2
  Out$full[k] <- R2(y=SC$FAIL[-train], pred=predfull, family="binomial")
  
  ## print progress
  cat(k, " ")
}

boxplot(Out, col="plum", ylab="R2")
colMeans(Out) 

# FSR
null <- glm(FAIL~1, data=SC)
fwd <- step(null, scope=formula(full), dir="forward")
length(coef(fwd))

# LRP
library(gamlr)
## Browsing History. 
## web has 3 colums: [machine] id, site [id], [# of] visits
web <- read.csv("D:/Documents/Data/DSHR/browser-domains.csv")
## Read in actual website names and relabel site factor
sitenames <- scan("D:/Documents/Data/DSHR/browser-sites.txt", what="character")
web$site <- factor(web$site, levels=1:length(sitenames), labels=sitenames)
## also factor machine id
web$id <- factor(web$id, levels=1:length(unique(web$id)))
## get total visits per-machine and % of time on each site
## tapply(a,b,c) does c(a) for every level of factor b.
machinetotals <- as.vector(tapply(web$visits,web$id,sum)) 
visitpercent <- 100*web$visits/machinetotals[web$id]
## use this info in a sparse matrix
## this is something you'll be doing a lot; familiarize yourself.
xweb <- sparseMatrix(
  i=as.numeric(web$id), j=as.numeric(web$site), x=visitpercent,
  dims=c(nlevels(web$id),nlevels(web$site)),
  dimnames=list(id=levels(web$id), site=levels(web$site)))
# what sites did household 1 visit?
#head(xweb[1, xweb[1,]!=0])
## now read in the spending data 
yspend <- read.csv("D:/Documents/Data/DSHR/browser-totalspend.csv", row.names=1)  # us 1st column as row names
yspend <- as.matrix(yspend) ## good practice to move from dataframe to matrix

spender <- gamlr(xweb, log(yspend), verb=TRUE); spender
plot(spender) ## path plot
betamin = coef(cv.spender, select="min"); betamin

spender <- gamlr(xweb, log(yspend), verb=TRUE); spender
plot(spender) ## path plot
betamin = coef(cv.spender, select="min"); betamin

spender <- gamlr(xweb, log(yspend), verb=TRUE); spender
plot(spender) ## path plot
betamin = coef(cv.spender, select="min"); betamin

head(AIC(spender))



