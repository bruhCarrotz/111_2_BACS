library(tidyr)
library(reshape2)
library(FSA)

norm_qq_plot <-function(values, main) {
    probs1000 <- seq(0, 1, 0.001)
    q_vals <- quantile(values, probs=probs1000)
    q_norm <- qnorm(probs1000,mean=mean(values), sd = sd(values))
    plot(q_norm, q_vals, xlab = "normal quantiles", 
        ylab = "values quantiles", main = main)
    abline(a=0, b=1, col="red", lwd = 2)
}

#Data Preparation
data1 <- read.csv("G:/My Drive/111_2_BACS/HW7/pls-media1.csv")$INTEND.0
data2 <- read.csv("G:/My Drive/111_2_BACS/HW7/pls-media2.csv")$INTEND.0
data3 <- read.csv("G:/My Drive/111_2_BACS/HW7/pls-media3.csv")$INTEND.0
data4 <- read.csv("G:/My Drive/111_2_BACS/HW7/pls-media4.csv")$INTEND.0
dataset <- list(data1, data2, data3, data4)

#Q1a
data1_mean <- mean(data1) 
data2_mean <- mean(data2)
data3_mean <- mean(data3)
data4_mean <- mean(data4)
print(paste("The mean of pls-media1.csv: ", data1_mean))
print(paste("The mean of pls-media2.csv: ", data2_mean))
print(paste("The mean of pls-media3.csv: ", data3_mean))
print(paste("The mean of pls-media4.csv: ", data4_mean))

#Q1b
boxplot(rev(dataset), horizontal=TRUE, main="Visualization of pls-media1 to pls-media4")
abline(v=mean(sapply(dataset, mean)), col="red")

#Q2a

#Q2b(i)
sstr <- sum(sapply(dataset, length)*(sapply(dataset, mean)-
    mean(sapply(dataset, mean)))^2)
df_mstr <- 4-1
mstr <- sstr/df_mstr 
print(paste("The MSTR value: ", mstr))

sse <- sum((sapply(dataset, length)-1)*sapply(dataset, var))
df_mse <- sum(sapply(dataset, length)) - 4
mse <- sse/df_mse
print(paste("The MSE value: ", mse))

#Q2b(ii)
f_value <- mstr/mse
print(paste("The F-value: ", f_value))

qf(p=0.95, df1=df_mstr, df2=df_mse)
p_value <- pf(f_value, df_mstr, df_mse, lower.tail=FALSE)
print(paste("The P-value: ", p_value))

#Q2c
#names(dataset) <- c("pls-media1", "pls-media2", "pls-media3", "pls-media4")
aovdata <- melt(dataset, id.vars = NULL,
                 variable.name = "type", value.name = "intend")
anova_model <- aov(aovdata$intend ~ factor(aovdata$L1))
summary(anova_model)

#Q2d
TukeyHSD(anova_model, conf.level = 0.05)

#Q2e
norm_qq_plot(data1, "pls-media1 Q-Q Plot")
norm_qq_plot(data2, "pls-media2 Q-Q Plot")
norm_qq_plot(data3, "pls-media3 Q-Q Plot")
norm_qq_plot(data4, "pls-media4 Q-Q Plot")

#Q3a
ranks <- rank(aovdata$intend)
aovdata$rank <- ranks # add as a new column to data_aov
rankgroup <- split(aovdata, aovdata$L1)
rank1 <- sum(rankgroup$`pls-media1`$rank)
rank2 <- sum(rankgroup$`pls-media2`$rank)
rank3 <- sum(rankgroup$`pls-media3`$rank)
rank4 <- sum(rankgroup$`pls-media4`$rank)
R <- c(rank1^2/length(data1), 
       rank2^2/length(data2),
       rank3^2/length(data3),
       rank4^2/length(data4))

#Q3b(i)
H = (12/(length(dataset)*(length(dataset)+1))) * sum(R) - 3*(length(dataset)+1)
print(paste("H: ", H))

#Q3b(ii)
kruskal_p <- 1 - pchisq(H, df=4-1)
print(paste("Kruskal's P-value: ", kruskal_p))

#Q3c
kruskal.test(intend~L1, data=aovdata)

#Q3d
dunnTest(intend~L1, data=aovdata, method = "bonferroni")
