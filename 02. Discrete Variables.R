library(dplyr)
library(ggplot2)

library(readxl)
df <- read_excel("office_data.xlsx")

head(df)
str(df)
summary(df)

### ???? ????

# ????????ǥ

(tb <- with(df, table(region)))
(tb <- tb[c("CBD", "GBD", "YBD", "ETC")])
(tb <- with(df, table(region))[c("CBD", "GBD", "YBD", "ETC")])

(tb <- xtabs(~region, df)[c("CBD", "GBD", "YBD", "ETC")])
(prop.table(tb))

# ????Ʈ: ????????ǥ

plot(tb)   #tb?? class?? table

(tb <- as.data.frame(tb))   #tb?? class?? dataframe��?? ????
gp <- ggplot(tb, aes(x=region, y=Freq)) +
  geom_col() +
  labs(x="region", y="frequency")
gp

# ????Ʈ: ??Ÿ?? ????

gp <- ggplot(df) +
  geom_bar(aes(x=region)) +
  scale_x_discrete(limits=c("CBD", "GBD", "YBD", "ETC")) +
  labs(x="region")
gp

# ????Ʈ: ??Ÿ?? ??��

gp <- ggplot(df) +
  geom_bar(aes(x=region, y=stat(prop), group=1), fill="skyblue") +
  scale_x_discrete(limits=c("CBD", "GBD", "YBD", "ETC")) +
  labs(x="region", y="Proportion")
gp

# ????Ʈ: ???η? ?迭

gp + coord_flip()

# ??????Ʈ

gp <- ggplot(df) +
  geom_bar(aes(x="", fill=region), width=1) +
  labs(x="", y="") +
  coord_polar(theta="y")+
  theme_void()
gp



### ???? ????

# ????ǥ

(tb <- with(df, table(region, year))[c("CBD", "GBD", "YBD", "ETC"),])
(tb <- xtabs(~region+year, data=df)[c("CBD", "GBD", "YBD", "ETC"),])
(tb <- prop.table(tb))

margin.table(tb, 1)   #?? ?հ?
margin.table(tb, 2)   #?? ?հ?
margin.table(tb)   #???հ? ???ڷ? ????

apply(tb, 1, sum)   #?? ?հ?
apply(tb, 2, sum)   #?? ?հ?

prop.table(tb)

(addmargins(tb))   #tb?? ??, ?? ?հ? ?߰?
(addmargins(prop.table(tb)))

library(gmodels)
(with(df, CrossTable(region, year)))

# ????Ʈ: ????ǥ

gp <- ggplot(as.data.frame(tb),
             aes(x=year, y=Freq, fill=region)) +
  geom_col()
gp

# ????Ʈ: ??Ÿ?? ????

gp <- ggplot(df, aes(x=year, fill=region)) +
  geom_bar()
gp

gp <- ggplot(df, aes(x=year, fill=region)) +
  geom_bar(position="dodge")   #?ٸ? ??��?? ????
gp

gp <- ggplot(df, aes(x=year, fill=region)) +
  geom_bar(position="dodge2")   #?? ???̿? ???? ?ֱ?
gp

# ????Ʈ: ??Ÿ?? ??��

gp <- ggplot(df, aes(x=year, fill=region)) +   #???뵵?? ��?? ?ױ?
  geom_bar(position="fill")
gp

gp <- ggplot(df, aes(x=region, y=stat(prop), group=1)) +   #???뵵?? ??��?? ????
  geom_bar() +
  facet_wrap(~year)
gp
