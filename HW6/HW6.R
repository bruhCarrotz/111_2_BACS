#Q1
#(a)
library(tidyr)
#(b)
file <- read.csv("G:/My Drive/111_2_BACS/HW6/verizon_wide.csv")
verizon <- gather(file, na.rm = TRUE,
    key = "type",
    value = "time")
data <- split(x = verizon$time, f = verizon$type)
#(c)
head(verizon)
tail(verizon)
#(d)
ilec <- data$ILEC
clec <- data$CLEC
plot(density(ilec), lwd=2, main="ILEC and CLEC Density Plots")
lines(density(clec), lwd=2, col="blue")
legend(
    150, 0.12, c("ILEC", "CLEC"),
    lwd = c(2,2), lty = c("solid", "solid"),
    col = c("black", "blue")
)

#Q2
#(b)
#(i) population are equal = reject
t.test(clec, ilec, alt="greater", var.equal=TRUE, conf.level=0.99)
#(ii) population are not equal = do not reject
t.test(clec, ilec, var.equal=FALSE, conf.level=0.99)

#(c)
permute_diff <- function(values, groups) {
  permuted <- sample(values, replace = FALSE)
  grouped <- split(permuted, groups)
  diff <- mean(grouped$CLEC) - mean(grouped$ILEC)
}
#(i)
nperms <- 10000
observed_diff <- mean(clec) - mean(ilec)
permuted_diffs <- replicate(nperms, permute_diff(verizon$time, verizon$type))
hist(permuted_diffs, breaks = "fd", probability = TRUE, labels=seq(-10, 20, 5))
lines(density(permuted_diffs), lwd=2, col="darkgreen", 
    main="permuted_diffs Plot")
abline(v=observed_diff, lty="dashed", col="red")

#(ii)
p_1tailed <- sum(permuted_diffs > observed_diff) / nperms
p_1tailed
p_2tailed <- sum(abs(permuted_diffs) > observed_diff) / nperms
p_2tailed

#(iii)

#Q3
gt_eq <- function(a, b) {
    ifelse(a > b, 1, 0) + ifelse(a == b, 0.5, 0)
}
#(a)
wilcox <- sum(outer(clec, ilec, FUN = gt_eq))
wilcox

#(b)
n1 <- length(ilec) 
n2 <- length(clec)
wilcox_p_1tail <- 1 - pwilcox(wilcox, n1, n2)
wilcox_p_1tail

#(c)
wilcox.test(clec, ilec, alternative = "greater", conf.level=0.01)

#(d)

#Q4
norm_qq_plot <- function(values, main) {
    probs1000 <- seq(0, 1, 0.001)
    q_vals <- quantile(values, probs=probs1000)
    q_norm <- qnorm(probs1000, mean=mean(values), sd=sd(values))
    plot(q_norm, q_vals, xlab = "normal quantiles", 
        ylab = "values quantiles", main = main)
    abline(a = 0, b = 1, col = "red", lwd = 2)
}

d1 <- rnorm(n = 500, mean = 15, sd = 5)
d2 <- rnorm(n = 200, mean = 30, sd = 5)
d3 <- rnorm(n = 100, mean = 45, sd = 5)
d123 <- c(d1, d2, d3)
plot(density(d123), main = "Gaussian Distribution")
norm_qq_plot(values = d123, main = "d123 Q-Q Plot")
