library(dplyr)
library(ggplot2)

library(readxl)
df <- read_excel("office_data.xlsx")

head(df)
str(df)
summary(df)

### One-way

# ???? ???? ??��

df$region
df$region <- factor(df$region, levels=c("ETC", "CBD", "GBD", "YBD"))
df$region   #?????? ??��?Ͽ?��?Ƿ? ?Ǿ? ETC?? ???غ???

(fit <- aov(price ~ region, df))

summary(fit)
summary.lm(fit)

plot(fit)

# ???л? ??��

fligner.test(price ~ region, df)
bartlett.test(price ~ region, df)



### Two-way

# ???? ???? ??��

df$year <- factor(df$year)

fit1 <- aov(price ~ region + year, df)
summary(fit1)
summary.lm(fit1)

fit2 <- aov(price ~ region * year, df)
summary(fit2)
summary.lm(fit2)

anova(fit1, fit2)



### ANCOVA: avo, lm ???? ????

fit1 <- aov(price ~ region + year + rent + metro + gfa, df)
summary(fit1)
summary.lm(fit1)

fit2 <- lm(price ~ region + year + rent + metro + gfa, df)
anova(fit2)
summary(fit2)