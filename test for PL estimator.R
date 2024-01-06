setwd("E:/R/gra_thesis")
library(survival)
num=30
rate=0.8
rt.exp <- rexp(num,rate) # rt: recorded time
ct.exp <-  rep(1,num)# ct: censored time
 # produce a special structure for censored survival data
result.KM.exp <- survfit(Surv(rt.exp, ct.exp) ~ 1, conf.type = "log-log")
# compute confidence intervals based on log-log transformation
# summary(result.KM.exp) # see the full Kaplan-Meier estimate
plot(result.KM.exp) # plot the Kaplan-Meier estimate curve
par(new=TRUE)
curve(pexp(x, rate = rate, lower.tail = FALSE), from = 0, to = max(rt.exp), ylim=c(0,1))
par(new=FALSE)