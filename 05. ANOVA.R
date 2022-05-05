library(dplyr)
library(ggplot2)

library(readxl)
df <- read_excel("office_data.xlsx")

head(df)
str(df)
summary(df)

### One-way

# Estimation

df$region
df$region <- factor(df$region, levels=c("etc", "cbd", "gbd", "ybd"))
df$region

(aov <- aov(price ~ region, df))

summary(aov)
summary.lm(aov)

model.tables(aov, type="mean")
model.tables(aov, type="effects")

TukeyHSD(aov)
plot(TukeyHSD(aov))

# Post hoc Analysis

plot(aov)

shapiro.test(df$price)

bartlett.test(price ~ region, df)
fligner.test(price ~ region, df)

oneway.test(price ~ region, df)

### Two-way

# Estimation

df$year <- factor(df$year)

aov1 <- aov(price ~ region + year, df)
summary(aov1)
summary.lm(aov1)

model.tables(aov1, type="mean")
model.tables(aov1, type="effects")

TukeyHSD(aov1)
plot(TukeyHSD(aov1))

aov2 <- aov(price ~ region * year, df)
summary(aov2)
summary.lm(aov2)

model.tables(aov2, type="mean")
model.tables(aov2, type="effects")

TukeyHSD(aov2)
plot(TukeyHSD(aov2))

anova(aov1, aov2)

# Post hoc Analysis

plot(aov2)

shapiro.test(df$price)

bartlett.test(price ~ region, df)
fligner.test(price ~ region, df)

bartlett.test(price ~ year, df)
fligner.test(price ~ year, df)

### ANCOVA

aov3 <- aov(price ~ region + year + metro + gfa, df)
summary(aov3)
summary.lm(aov3)

ols3 <- lm(price ~ region + year + metro + gfa, df)
anova(ols3)
summary(ols3)

### MANOVA

y <- cbind(df$price, df$rent)

aov4 <- manova(y ~ region + year + metro + gfa, df)
summary(aov4)
summary.lm(aov4)

rm(list=ls())
