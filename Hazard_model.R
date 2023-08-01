# 4 different models with Hazard plot using tongue caner data
#(Exponential, Weibull, Log normal, Log logistic)
library(ggplot2)
library(gridExtra)

fit1 <- survfit(Surv(time,delta) ~ 1, data = AN, type= "fleming-harrington")
fit2 <- survfit(Surv(time,delta) ~ 1, data = DI, type= "fleming-harrington")
km1 <- summary(fit1, times = seq(0,200,2))
km2 <- summary(fit2, times = seq(0,200,2))

M <- data.frame(time1 = km1$time, surv1 = km1$surv, 
                time2 = km2$time, surv2 = km2$surv)

# exponential 
p1 <- ggplot(M, aes(time1, -log(surv1))) + geom_line(size=0.8) + 
  geom_line(aes(time2, -log(surv2)),size = 0.8, linetype = "dashed")+
  theme_bw() + ggtitle("Exponential")+
  labs(x = "time", y = "Estimated Cumulative Hazard Rates")

# Weibull
p2 <- ggplot(M, aes(log(time1), log(-log(surv1)))) + geom_line(size = 0.8) + 
  geom_line(aes(log(time2), log(-log(surv2))),size=0.8, linetype = "dashed")+
  theme_bw() + ggtitle("Weibull")+
  labs(x = "log(time)", y = "Estimated Cumulative Hazard Rates")

# Log-normal
p3 <- ggplot(M, aes(log(time1), pnorm(1-surv1))) + geom_line(size = 0.8) + 
  geom_line(aes(log(time2), pnorm(1-surv2)),size=0.8, linetype = "dashed")+
  theme_bw() + ggtitle("Log-normal")+
  labs(x = "log(time)", y = "Estimated Cumulative Hazard Rates")

# Log-logistic
p4 <- ggplot(M, aes(log(time1), log(1/surv1 - 1))) + geom_line(size = 0.8) + 
  geom_line(aes(log(time2), log(1/surv2 - 1)),size=0.8, linetype = "dashed")+
  theme_bw() + ggtitle("Log-logistic")+
  labs(x = "log(time)", y = "Estimated Cumulative Hazard Rates")

grid.arrange(p1, p2, p3, p4, nrow = 2)