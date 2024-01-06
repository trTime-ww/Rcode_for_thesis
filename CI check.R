setwd("E:/R/gra_thesis")
library(survival)

##### main program #####
n <- seq(15,100,1) # sample size sequence
lambda.surv <- 1 # lambda for survival distribution
lambda.cens <- 0.8 # lambda for censoring distribution
t <- c(0.4,0.8,1.2) # time to be checked
N <- 250 # repetitions (Choose it carefully. A large N makes the program run for
# quite a long time)
S <- function(x)pexp(x,lambda.surv,lower.tail=FALSE) # set survival function S

# For each method, set matrix mat.n = matrix[t_i,n_j] :
mat.n <- matrix(data=0,nrow=length(t),ncol=length(n))
mat.n.list <- list(mat.n,mat.n,mat.n)
names(mat.n.list)[1] <- "plain"
names(mat.n.list)[2] <- "log-log"
names(mat.n.list)[3] <- "arcsin"

# For each method, each n, set matrix mat.trial = matrix[t_i,k] to record 
# results of k = 1:N :
mat.trial <- matrix(data=0,nrow=length(t),ncol=N)
mat.trial.list <- list(mat.trial,mat.trial,mat.trial)

# Prepare a list to accommodate survfit results of 3 methods:
result.PL.list <- list()

mean.rmna <- function(x)mean(x, na.rm=TRUE) # function "mean" in which na.rm=TRUE

# Generate data:
for (j in 1:length(n)) { # for each sample size
  for (k in 1:N) { # repeat the trial N times
    st <- rexp(n[j],lambda.surv) # survival times
    ct <- rexp(n[j],lambda.cens) # censored times
    ot <- apply(cbind(st,ct), 1, min) # observed times
    delta <- st<=ct # index: whether the observed times are survival times
    
    # Survfit results of 3 methods:
    result.PL.list[[1]] <- survfit(Surv(ot,delta)~1, conf.type="plain")
    result.PL.list[[2]] <- survfit(Surv(ot,delta)~1, conf.type="log-log")
    result.PL.list[[3]] <- survfit(Surv(ot,delta)~1, conf.type="arcsin")
    
    for (s in 1:3) { # for each method
      for (i in 1:length(t)) { # for each time
        target.index <- which(result.PL.list[[s]]$time>t[i])[1]-1 # index: the
        # largest index l such that S(t_l)=S(t[i]) (t_l's are observed times)
        
        mat.trial.list[[s]][i,k] <- result.PL.list[[s]]$lower[target.index]<=S(t[i]) && result.PL.list[[s]]$upper[target.index]>=S(t[i])
        # decide whether the true S(t[i]) is in the CI
      }
    }
  }
  
  for (s in 1:3) { # for each method
    mat.n.list[[s]][,j] <- apply(mat.trial.list[[s]], 1, mean.rmna) # calculate 
    # the frequency that the true S(t[i]) is in the CI
  }
  
}


##### plot graph #####
png("3 methods.png", width = 720, height = 720)
par(mfrow=c(2,2))

plot(NULL,NULL,xlim=range(n),ylim=c(0.8,1),xlab="n",ylab="Frequency: inside CI",
     main="t = 0.4",cex.lab=1.3,cex.main=1.8,cex.axis=1.2)
abline(h=0.95,col="gray60",lty=4)
for (i in 1:3) {
  lines(n,mat.n.list[[i]][1,],col=2^(i-1))
}

plot(NULL,NULL,xlim=range(n),ylim=c(0.8,1),xlab="n",ylab="Frequency: inside CI",
     main="t = 0.8",cex.lab=1.3,cex.main=1.8,cex.axis=1.2)
abline(h=0.95,col="gray60",lty=4)
for (i in 1:3) {
  lines(n,mat.n.list[[i]][2,],col=2^(i-1))
}

plot(NULL,NULL,xlim=range(n),ylim=c(0.8,1),xlab="n",ylab="Frequency: inside CI",
     main="t = 1.2",cex.lab=1.3,cex.main=1.8,cex.axis=1.2)
abline(h=0.95,col="gray60",lty=4)
for (i in 1:3) {
  lines(n,mat.n.list[[i]][3,],col=2^(i-1))
}

plot(NULL,NULL,xlim=c(0,1),ylim=c(0,1),ann=FALSE,bty='n',xaxt='n',yaxt='n')
legend(x='center',col=c(1,2,4),lty=c(1,1,1),cex=2,
       legend = c("plain","log-log","arcsin-squared"))

par(mfrow=c(1,1))
dev.off()

##### draft #####
# rt <- rexp(n,lambda) # rt: recorded time
# ct <- rbinom(n,size=1,prob=0.8) # ct: censored time
# # compute confidence intervals based on log-log transformation:
# result.KM <- survfit(Surv(rt, ct) ~ 1, conf.type = "plain")
# 
# # find the maximum i such that t_i<=t:
# for (i in 1:length(t)) {
#   target.index <- which(result.KM$time>t[i])[1]-1
#   
#   if.in.interval <- result.KM$lower[target.index]<=g(t[i]) && 
#     result.KM$upper[target.index]>=g(t[i])
#   
#   
# }
# 
# 
# 
# plot(result.PL.list[[1]])
# par(new=TRUE)
# curve(g(x), from = 0, to = max(ot), ylim=c(0,1))
# par(new=FALSE)
# 
# plot(mat.n.list[[1]][1,],type = "l",ylim = c(0.8,1))
# par(new=TRUE)
# plot(mat.n.list[[2]][1,],type = "l",col=1,ylim = c(0.8,1))
# par(new=TRUE)
# plot(mat.n.list[[3]][1,],type = "l",col="blue",ylim = c(0.8,1))
# par(new=FALSE)
# 
# 
# png("3.03.png")
# 
# par(mfrow=c(1,2))
# 
# plot(x,y,type = 'l',ylim = c(0,0.5))
# # lines(x,exp(-(x-sqrt(2))^2)/sqrt(2*pi),lty=2)
# lines(x,dnorm(x,mean = sqrt(2),1),lty=2)
# lines(x,exp(-(x)),lty=3)
# legend(x='topright', legend = c(expression(g), expression(f_1), expression(f_2)),
#        lty = c(1,2,3),bty = 'n',cex = 0.8)
# 
# plot(x,y/dnorm(x,mean = sqrt(2),1),type = 'l',lty=2,ylim = c(0,2),ylab="g(x)/f(x)")
# # lines(x,exp(-(x-sqrt(2))^2)/sqrt(2*pi),lty=2)
# # lines(x,dnorm(x,mean = sqrt(2),1),lty=2)
# lines(x,g(x)/exp(-(x)),lty=3)
# legend(x='topright', legend = c(expression(g/f_1), expression(g/f_2)),
#        lty = c(2,3),bty = 'n',cex = 0.8)
# 
# par(mfrow=c(1,1))
# dev.off()