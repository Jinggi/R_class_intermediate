library(dplyr)
library(ggplot2)

library(readxl)
df <- read_excel("office_data.xlsx")

head(df)
str(df)
summary(df)

### One Variable

# Table

(tb <- with(df, table(region)))
(tb <- tb[c("cbd", "gbd", "ybd", "etc")])
(tb <- with(df, table(region))[c("cbd", "gbd", "ybd", "etc")])

(tb <- xtabs(~region, df))
(tb <- xtabs(~region, df)[c("cbd", "gbd", "ybd", "etc")])

(prop.table(tb))

# Bar chart

plot(tb)

(tb <- as.data.frame(tb))

plot(tb)

gp <- ggplot(tb, aes(x=region, y=Freq)) +
  geom_col() +
  labs(x="region", y="frequency")
gp

gp <- ggplot(df) +
  geom_bar(aes(x=region)) +
  labs(x="region")
gp

gp <- ggplot(df) +
  geom_bar(aes(x=region)) +
  scale_x_discrete(limits=c("cbd", "gbd", "ybd", "etc")) +
  labs(x="region")
gp

gp <- ggplot(df) +
  geom_bar(aes(x=region, y=stat(prop), group=1), fill="skyblue") +
  scale_x_discrete(limits=c("cbd", "gbd", "ybd", "etc")) +
  labs(x="region", y="Proportion")
gp

gp + coord_flip()

# Pie Chart

gp <- ggplot(df) +
  geom_bar(aes(x="", fill=region), width=1) +
  labs(x="", y="") +
  coord_polar(theta="y")+
  theme_void()
gp

### Two Variables

# Table

(tb <- with(df, table(region, year))[c("cbd", "gbd", "ybd", "etc"),])
(tb <- xtabs(~region+year, data=df)[c("cbd", "gbd", "ybd", "etc"),])

margin.table(tb, 1)
margin.table(tb, 2)
margin.table(tb)

apply(tb, 1, sum)
apply(tb, 2, sum)

(addmargins(tb))

prop.table(tb)

(addmargins(prop.table(tb)))

library(gmodels)

(with(df, CrossTable(region, year)))

# Bar Chart

gp <- ggplot(as.data.frame(tb),
             aes(x=year, y=Freq, fill=region)) +
  geom_col()
gp

gp <- ggplot(df, aes(x=year, fill=region)) +
  geom_bar()
gp

gp <- ggplot(df, aes(x=year, fill=region)) +
  geom_bar(position="dodge")
gp

gp <- ggplot(df, aes(x=year, fill=region)) +
  geom_bar(position="dodge2")
gp

gp <- ggplot(df, aes(x=year, fill=region)) +
  geom_bar(position="fill")
gp

gp <- ggplot(df, aes(x=region, y=stat(prop), group=1)) +
  geom_bar() +
  facet_wrap(~year)
gp

rm(list=ls())
