oj <- read.csv("D:/Documents/Data/DSHR/oj.csv")
head(oj, n=5)    
tail(oj, n=5)    
glm(log(sales) ~ brand + log(price), data=oj)

x <- model.matrix(~ brand + log(price), data=oj); head(x); tail(x)
oj$brand = as.factor(oj$brand)
x <- model.matrix(~ brand + log(price), data=oj); head(x)
oj$mybrand = relevel(oj$brand, "tropicana")
x <- model.matrix(~ mybrand + log(price), data=oj); head(x)

glm(log(sales) ~ log(price)*brand*feat, data=oj)

email <- read.csv("D:/Documents/Data/DSHR/spam.csv")
dim(email)
colnames(email)

glm(spam ~ ., data=email, family='binomial')
spammy <- glm(spam ~ ., data=email, family='binomial')

coef(spammy)["word_free"]; exp(coef(spammy)["word_free"])
coef(spammy)["word_business"]; exp(coef(spammy)["word_business"])
coef(spammy)["word_meeting"]; exp(coef(spammy)["word_meeting"])

predict(spammy, newdata = email[c(300,21),], type="response")

summary(spammy)$deviance
summary(spammy)$null.deviance

D <- summary(spammy)$deviance; D
D0 <- summary(spammy)$null.deviance; D0
R2 <- 1 - D/D0; R2