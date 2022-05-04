library(dplyr)
library(ggplot2)

library(readxl)
df <- read_excel("office_data.xlsx")

head(df)
str(df)
summary(df)

### Categorical

# Independence

chisq.test(df$region, df$year)
chisq.test(df$region, df$year, simulate.p.value=T)

(tb <- xtabs(~region+year, data=df)[c("cbd", "gbd", "ybd", "etc"),])

chisq.test(tb)
chisq.test(tb, simulate.p.value=T)

fisher.test(tb, conf.level=.95, alternative=c("two.sided"))
fisher.test(tb, conf.level=.95, alternative=c("two.sided"), simulate.p.value=T)

library(vcd)

assocstats(tb)
mosaic(tb, shade=T)

# Goodness of fit

apply(tb, 1, sum)
apply(prop.table(tb), 1, sum)

chisq.test(apply(tb, 1, sum))
chisq.test(apply(tb, 1, sum), c(.3, .4, .2, .1))
chisq.test(apply(tb, 1, sum), c(.3, .4, .2, .1), simulate.p.value=T)

### Numeric

# Mean: One Sample

t.test(df$price, mu=15000000, conf.level=.95, alternative=c("two.sided"))
t.test(df$price, mu=15000000, conf.level=.95, alternative=c("less"))
t.test(df$price, mu=15000000, conf.level=.95, alternative=c("greater"))

# Mean: Two Samples

df <- df %>% mutate(size=ifelse(gfa>=15000, "big", "small"))

var.test(df$price~df$size, ratio=1, conf.level=.95, alternative=c("two.sided"))
t.test(df$price~df$size, var.equal=T, conf.level=.95, alternative=c("two.sided"))
t.test(df$price~df$size, var.equal=F, conf.level=.95, alternative=c("two.sided"))

p1 <- df$price[df$size=="big"]
p2 <- df$price[df$size=="small"]

var.test(p1, p2, ratio=1, conf.level=.95, alternative=c("two.sided"))
t.test(p1, p2, var.equal=T, conf.level=.95, alternative=c("two.sided"))
t.test(p1, p2, var.equal=F, conf.level=.95, alternative=c("two.sided"))

(p3 <- df$price[df$year=="2019"])
(p4 <- df$price[df$year=="2020"])
(p <- c(p3, p4))
(y <- factor(rep(c(1, 2), c(18, 18))))

t.test(p3, p4, paired=T, conf.le3el=.95, alternative=c("two.sided"))
t.test(p~y, paired=T, conf.level=.95, alternative=c("two.sided"))

# Proportion: One Sample

apply(tb, 1, sum)
apply(prop.table(tb), 1, sum)

prop.test(x=16, n=50, p=.3, conf.level=.95, alternative=c("two.sided"))
binom.test(x=16, n=50, p=.3, conf.level=.95, alternative=c("two.sided"))

# Proportion: Two Samples

prop.test(x=c(16, 19), n=c(50, 50), conf.level=.95, alternative=c("two.sided"))

# Variance: Two Samples

var.test(v1, v2)

# Correlation

cor(data.frame(df$price, df$rent, df$gfa))

cor.test(df$price, df$rent)
cor.test(~df$price+df$rent)

library(psych)

corr.test(data.frame(df$price, df$rent, df$gfa))
print(corr.test(data.frame(df$price, df$rent, df$gfa)), short=F)
pairs.panels(data.frame(df$price, df$rent, df$gfa))

library(corrgram)

corrgram(data.frame(df$price, df$rent, df$gfa))

# Normality

shapiro.test(df$price)
ggplot(df, aes(sample=price)) +
  geom_qq() +
  geom_qq_line()

shapiro.test(df$ln_price)
ggplot(df, aes(sample=ln_price)) +
  geom_qq() +
  geom_qq_line()

### Non-parametric

# One Sample

wilcox.test(df$price, mu=15000000,
            conf.level=.95, conf.int=T, alternative=c("two.sided"))

# Two Samples

wilcox.test(df$price~df$size, mu=0,
            conf.level=.95, conf.int=T, alternative=c("two.sided"))

wilcox.test(p1, p2, mu=0,
            conf.level=.95, conf.int=T, alternative=c("two.sided"))

ks.test(p1, p2)

rm(list=ls())
