library(dplyr)
library(ggplot2)

library(readxl)
df <- read_excel("office_data.xlsx")

head(df)
str(df)
summary(df)

### One Variable

# Summarize

mean(df$price)
with(df, mean(price))

summary(df$price)

df %>% summarise(n=n(),
                 na=sum(is.na(price)),
                 mean=mean(price, na.rm=T),
                 median=median(price, na.rm=T),
                 min=min(price, na.rm=T),
                 max=max(price, na.rm=T),
                 range=diff(range(price, na.rm=T)),
                 q1=quantile(price, .25),
                 q3=quantile(price, .75))

# Box Plot

gp <- ggplot(df, aes(x="", y=price)) +
  geom_boxplot() +
  labs(x="")
gp

gp + coord_flip()

ggplot_build(gp)
str(ggplot_build(gp)[[1]][[1]])
ggplot_build(gp)[[1]][[1]]$outliers[[1]]

# Histogram and Density Plot

gp <- ggplot(df, aes(x=price))
gp + geom_histogram()
gp + geom_histogram(bins=10)
gp + geom_histogram(binwidth=5000000)

gp <- ggplot(df, aes(x=price))
gp + geom_density()
gp + geom_density(fill="skyblue") +
  geom_rug() + xlim(0, 50000000)

gp <- ggplot(df, aes(x=price, y=stat(density))) +
  geom_histogram(fill="skyblue", bins=10) +
  geom_density(color="red") +
  xlim(0, 50000000)
gp

gp <- ggplot(df, aes(x=price, y=stat(density))) +
  geom_density(color="red") +
  geom_histogram(fill="skyblue", alpha=.5, bins=10) +
  xlim(0, 50000000)
gp

### By Factor

# Summarize

df %>% group_by(region) %>% 
  summarise(n=n(),
            na=sum(is.na(price)),
            mean=mean(price, na.rm=T),
            median=median(price, na.rm=T),
            min=min(price, na.rm=T),
            max=max(price, na.rm=T),
            range=diff(range(price, na.rm=T)),
            q1=quantile(price, .25),
            q3=quantile(price, .75))

# Box Plot

gp <- ggplot(df, aes(factor(region), price)) +
  geom_boxplot() +
  scale_x_discrete(limits=c("cbd", "gbd", "ybd", "etc"))
gp

# Histogram and Density

gp <- ggplot(df, aes(x=price)) +
  geom_histogram(bins=10) +
  facet_wrap(~region)
gp

gp <- ggplot(df, aes(x=price)) +
  geom_histogram(bins=10) +
  facet_wrap(~region, ncol=1)
gp

gp <- ggplot(df, aes(x=price, fill=factor(region))) +
  geom_histogram(bins=10, alpha=.5)
gp

gp <- ggplot(df, aes(x=price)) +
  geom_density() +
  xlim(0, 50000000) +
  facet_wrap(~region)
gp

gp <- ggplot(df, aes(x=price)) +
  geom_density() +
  xlim(0, 50000000) +
  facet_wrap(~region, ncol=1)
gp

gp <- ggplot(df, aes(x=price, fill=factor(region))) +
  geom_density(alpha=.5) +
  xlim(0, 50000000)
gp

### Two Variables

# Correlation

attach(df)

cor(price, rent, use="everything", method=c("pearson"))
cor(price, rent, use="complete", method=c("pearson"))
cor(price, rent, use="everything", method=c("kendall"))
cor(price, rent, use="everything", method=c("spearman"))

cor(df[, 1:3], use="everything", method=c("pearson"))

detach(df)

# Scatter Plot

gp <- ggplot(df, aes(x=price, y=rent)) +
  geom_point(shape=21, color="red", fill="red", size=3)
gp

gp + geom_smooth()
gp + geom_smooth(se=F)

gp + geom_smooth(method="lm")
gp + geom_smooth(method="lm", se=F)

gp + geom_hline(aes(yintercept=mean(rent)), color="red") +
  geom_vline(aes(xintercept=mean(price)), color="blue") +
  geom_abline(aes(slope=0.002 intercept=1000), color="green")

gp <- ggplot(df, aes(x=price, y=rent, color=region, fill=region)) +
  geom_point(shape=21, size=3)
gp

gp + geom_smooth()
gp + geom_smooth(se=F)

gp + geom_smooth(method="lm")
gp + geom_smooth(method="lm", se=F)

gp + geom_text(aes(label=i),
               nudge_x = 10000, nudge_y = 5000)

gp + facet_wrap(~region, ncol=2, dir="v")

### Many Variables: GGally

library(GGally)
ggcorr(df, method=c("pairwise", "pearson"), label=T, label_round=2)
ggpairs(df, lower=list(continuous="smooth"))

rm(list=ls())
