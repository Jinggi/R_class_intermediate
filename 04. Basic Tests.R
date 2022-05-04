library(dplyr)
library(ggplot2)

library(readxl)
df <- read_excel("office_data.xlsx")

head(df)
str(df)
summary(df)

### ?????? ?????? ??��

# ?? ?????? ?????? ??�� (H0: ?????̴?)

(tb <- xtabs(~region+year, data=df)[c("CBD", "GBD", "YBD", "ETC"),])

chisq.test(tb)
chisq.test(tb, simulate.p.value=T)   #ǥ??ũ?? ??�� ???? ????ī???? ?ùķ??̼?

fisher.test(tb, conf.level=.95, alternative=c("two.sided"))
fisher.test(tb, conf.level=.95, alternative=c("two.sided"), simulate.p.value=T)



### ?????? ?????? ??��

# ?? ??????, ?????? ??��

t.test(price, mu=15000000, conf.level=.95, alternative=c("two.sided"))
t.test(price, mu=15000000, conf.level=.95, alternative=c("less"))
t.test(price, mu=15000000, conf.level=.95, alternative=c("greater"))

# ?? ??????, ?????? ???? ??��

df <- df %>% mutate(size=ifelse(gfa>=15000, "big", "small"))

var.test(price~size, ratio=1, conf.level=.95, alternative=c("two.sided"))   #???л? ??��
t.test(price~size, var.equal=T, conf.level=.95, alternative=c("two.sided"))
t.test(price~size, var.equal=F, conf.level=.95, alternative=c("two.sided"))

v1 <- price[size=="big"]
v2 <- price[size=="small"]

var.test(v1, v2, ratio=1, conf.level=.95, alternative=c("two.sided"))
t.test(v1, v2, var.equal=T, conf.level=.95, alternative=c("two.sided"))
t.test(v1, v2, var.equal=F, conf.level=.95, alternative=c("two.sided"))

t.test(v1, v2, paired=T, conf.level=.95, alternative=c("two.sided"))  #?ִ?????
t.test(v~f, paired=T, conf.level=.95, alternative=c("two.sided"))

# ?? ??????, ????�� ??��
# n?? ???࿡?? x?? ?????Ǿ?�� ??, ?߻?Ȯ??�� p???? ?? ?? ?ִ°??

prop.test(x=1000, n=10000, p=.15, conf.level=.95, alternative=c("two.sided"))
binom.test(x=1000, n=10000, p=.15, conf.level=.95, alternative=c("two.sided"))

# ?? ??????, ????�� ???? ??��

prop.test(x=c(1000, 500), n=c(10000, 800), conf.level=.95, alternative=c("two.sided"))

# ?? ??????, ???л? ???? ??��

var.test(v1, v2)   #F??��



### ��?Լ? ??��

shapiro.test(price)
shapiro.test(log(price))   #��?Ժ??? ???? ?ʴ? ???? ?α׺?ȯ

ggplot(df, aes(sample=price)) +   #??��??-??��?? ?׷??? ?ۼ?
  geom_qq() +
  geom_qq_line()

df %>% 
  mutate(lnp=log(price)) %>%   #��?Ժ??? ???? ?ʴ? ???? ?α׺?ȯ
  ggplot(aes(sample=lnp)) +
    geom_qq() +
    geom_qq_line()



### ?????? ??��

#?? ?????? ?߽ɿ? ???? ?????? ??��

wilcox.test(price, mu=15000000, conf.level=.95, conf.int=T, alternative=c("two.sided"))

#?? ?????? ?߽? ???̿? ???? ?????? ??��

wilcox.test(price~size, mu=0, conf.level=.95, conf.int=T, alternative=c("two.sided"))
wilcox.test(v1, v2, mu=0, conf.level=.95, conf.int=T, alternative=c("two.sided"))

ks.test(v1, v2)