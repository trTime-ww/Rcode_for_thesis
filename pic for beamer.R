setwd("E:/R/gra_thesis")
library(survival)
num=20
rate=0.8
rt.exp <- rexp(num,rate) # rt: recorded time
ct.exp <-  rep(1,num)# ct: censored time
# produce a special structure for censored survival data
result.KM.exp <- survfit(Surv(rt.exp, ct.exp) ~ 1, conf.type = "log-log")
# compute confidence intervals based on log-log transformation
# summary(result.KM.exp) # see the full Kaplan-Meier estimate

# png("pic(2).png")

plot(result.KM.exp, xaxt = "n", las=1) # plot the Kaplan-Meier estimate curve
abline(v=0.8, col='red2',lty=2)
par(new=TRUE)
curve(pexp(x, rate = rate, lower.tail = FALSE), col="blue", lwd = 1.3, 
      from = 0, to = max(rt.exp), ylim=c(0,1), 
      ann = FALSE, xaxt = "n", yaxt = "n")
axis(1,c(0,0.8,floor(max(rt.exp))))
par(new=FALSE)

# dev.off()

N <- c(15,16,17,18,19,20)
freq <- c(0.87,0.91,0.90,0.96,0.93,0.94)

png("pic(3).png")

plot(N,freq, xlim = c(min(N),max(N)+2),ylim = c(0.8,1),las=1,
     xlab = "n", ylab = "Frequency")

dev.off()

png("pic(4).png")

plot(N,freq, xlim = c(min(N),max(N)+2),ylim = c(0.8,1),las=1,
     xlab = "n", ylab = "Frequency")
lines(N,freq)

dev.off()

# axis(1,min(N):(max(N)+2))
# axis(2,seq(0.8,1,0.05))
