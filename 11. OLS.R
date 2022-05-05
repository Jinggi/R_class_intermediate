library(dplyr)
library(ggplot2)

library(readxl)
df <- read_excel("office_data.xlsx")

head(df)
str(df)
summary(df)

### Estimation

# Simple and Polynomial

lm(df$price ~ df$rent)

attach(df)
lm(price ~ rent)
detach(df)

lm(price ~ rent, df)

df %>%
  ggplot(aes(x=rent, y=price)) +
  geom_point() +
  geom_smooth(method="lm")

lm(price ~ rent + 0, df)
lm(price ~ rent - 1, df)

lm(price ~ log(rent), df)

lm(price ~ rent + rent^2, df)
lm(price ~ (rent + gfa)^2, df)

lm(price ~ rent + I(rent^2), df)
lm(price ~ poly(rent, 2, raw=T), df)

lm(price ~ region, data=df)
str(df$region)
df$region <- factor(df$region, levels=c("etc", "cbd", "gbd", "ybd"))
str(df$region)
lm(price ~ region, data=df)

lm(price ~ year, data=df)
str(df$year)
df$year <- factor(df$year)
str(df$year)
lm(price ~ year, data=df)

lm(price ~ rent, df, subset = region == "cbd")

lm(price ~ rent, subset = 1:30, df)

# Multiple

lm(price ~ ., df)

lm(price ~ rent + region, df)

df %>%
  ggplot(aes(x=rent, y=price)) +
  geom_point() +
  geom_smooth(method="lm", se=F) +
  facet_wrap(~region)

lm(price ~ rent + region + rent:region, df)
lm(price ~ rent * region, df)
lm(price ~ (rent + region)^2, df)

lm(price ~ rent * gfa * region, df)
lm(price ~ (rent + gfa + region)^2, df)

# Result

ols1 <- lm(price ~ rent + gfa, df)
ols2 <- lm(price ~ rent + gfa + region, df)

summary(ols1)
summary(ols2)

anova(ols1)
anova(ols2)
anova(ols1, ols2)

summary(ols2)$r.squared
deviance(ols2)
logLik(ols2)
AIC(ols2)

coef(ols2)
confint(ols2)

fitted(ols2)
resid(ols2)

library(broom)
tidy(ols2)
augment(ols2)
glance(ols2)

# Subset Selection

step(ols2, direction = "both")
step(ols2, direction = "forward")
step(ols2, direction = "backward")

library(leaps)

fit <- regsubsets(price ~ ., df)

summary(fit)
plot(fit)
plot(fit, scale="adjr2")

library(MASS)

fit_full <- lm(price ~ ., df)
fit_null <- lm(price ~ 1, df)

stepAIC(fit_null,
        scope=list(lower=fit_null, upper=fit_full),
        trace=F)

stepAIC(fit_full,
        scope=list(lower=fit_null, upper=fit_full),
        trace=F)

### Post hoc Analysis

ols <- lm(price ~ rent + gfa, df)
plot(ols, pch=20)

# Outlier

library(car)
infIndexPlot(ols)
influencePlot(ols)
outlierTest(ols)
avPlots(ols)

# Multicollinearity

vif(ols)

# Linearity

library(car)
crPlots(ols)

boxTidwell(price ~ rent + gfa, data = df)

# Normality

shapiro.test(df$price)
powerTransform(df$price)
summary(powerTransform(df$price))

shapiro.test(resid(fit))
powerTransform(resid(fit))

library(car)
boxCox(ols)

# Heteroskedasticity

library(lmtest)
bptest(ols)

library(car)
ncvTest(ols)
hccm(ols)

w <- 1/fitted(lm(abs(resid(ols)) ~ fitted(ols)))^2
wls <- lm(price ~ rent + gfa, df, weight = w)
summary(wls)

library(nlme)
gls <- gls(price ~ rent + gfa, df, weights = varPower())
summary(gls)

library(MASS)
rls <- rlm(price ~ rent + gfa, df)
summary(rls)

# Autocorrelation

durbinWatsonTest(ols)

library(forecast)
checkresiduals(ols)

fit <- auto.arima(df$price,
                  xreg=model.matrix(ols)[,-1],
                  stepwise=F, approximation=F)
summary(fit)
checkresiduals(fit)

library(prais)
fit <- prais_winsten(price ~ rent + gfa, df, index = "year")
summary(fit)
checkresiduals(fit)

library(orcutt)
fit <- cochrane.orcutt(ols)
summary(fit)
checkresiduals(fit)

### Prediction

# Interval

x <- data.frame(rent=1:10, gfa=101:110)
predict(ols, x, interval=c("none"), level=.95)
predict(ols, x, interval=c("confidence"), level=.95)
predict(ols, x, interval=c("prediction"), level=.95)

# Performance

library(caret)

set.seed(1)
dp <- createDataPartition(df$price, p=.8, list=F)
train <- df %>% slice(dp)
test <- df %>% slice(-dp)

fit <- lm(price ~ rent + gfa, train)
pred <- predict(fit, test)

defaultSummary(data.frame(obs=test$price, pred=pred))

data.frame(obs=test$price, pred=pred) %>%
  ggplot(aes(x=obs, y=pred)) +
  geom_point() +
  geom_abline(intercept=0, slope=1)

rm(list=ls())
