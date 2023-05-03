#Supporting Functions

plot_sample_mean <- function(sample) {
    lines(density(sample), col="#8cd9db")
    return(mean(sample))
}
plot_sample_median <- function(sample) {
    lines(density(sample), col="#8cd9db")
    return(median(sample))
}

#Q1a (i)
datas <- read.csv("G:/My Drive/111_2_BACS/HW5/verizon.csv")
ver_time <- datas$Time

#(i)
plot(density(ver_time), col="darkgreen", lwd=2, 
    main = "Verizon's Repair Time")
abline(v=mean(ver_time), col="blue")

#(ii)
# Given the data above, the null hypothesis will be that Verizon's
# average repair time is 7.5 minutes.

#(iii)
pop_mean <- mean(ver_time)
pop_sd <- sd(ver_time)
pop_sderr <- pop_sd / length(ver_time)^0.5
ci99 <- pop_mean + c(-2.58, 2.58)*pop_sderr
print(paste("The 99% Confindence Interval is between", ci99[1], "and", ci99[2]))

#(iv)
t <- (pop_mean - 7.6) / pop_sderr
t
df <- nrow(datas) - 1


#Q2b
plot(
  density(ver_time), 
  main="Population & Bootstrapped Samples"
)

plot_sample <- function(sample) {
  lines(density(sample), col="#8cd9db")
  return(mean(sample))
}

sample_means <- apply(resamples, 2, FUN=plot_sample)
lines(density(sample_means), col="#fbca8e", lwd=2, lty="dashed")





bootsamples <- replicate(1000, sample(ver_time, length(ver_time), replace=TRUE))
sample_means <- apply(bootsamples, 2,FUN=plot_sample_mean)
lines(density(ver_time), col="red", lwd=2, lty="dashed")

#(i)
sd_error <- sd(sample_means) / length(sample_means)^0.5
ci99_boot <- mean(sample_means) + c(-2.58, 2.58)*sd_error
print(paste("The 99% Confindence Interval is between", ci99_boot[1], "and", ci99_boot[2]))

#(ii)
diff <- sample_means - 7.6
diff_sderror <- sd(diff) / length(diff)^0.5
diff_ci99 <- mean(abs(diff)) + c(-2.58, 2.58) * diff_sderror
print(paste("The 99% Confindence Interval is between", diff_ci99[1], "and", diff_ci99[2]))

#(iii)
#(iv)
#(v)

library(compstatslib)

interactive_t_test()

dev.off()
print(plot(1))
