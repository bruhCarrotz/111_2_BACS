#Supporting Functions
standardize <- function(numbers) {
  numbers <- (numbers - mean(numbers)) / sd(numbers)
  return(numbers)
}

plotFunct <- function(data, title){
  hist(data, prob=TRUE, main = title)
  lines(density(data), lwd = 2, col = "brown", main = title)
}

#Problem 1
set.seed(150)

#Q1(a)
dataset1 <- rnorm(1000, mean=940, sd=190)
rnorm_std <- standardize(dataset1)

#Q1a (i)
print(paste("Mean of the rnorm_std: ", mean(rnorm_std)))
print(paste("Standard deviation of the rnorm_std: ", sd(rnorm_std)))

#Q1a (ii)
plotFunct(dataset1, "Visualization of rnorm")
plotFunct(rnorm_std, "Visualization of rnorm_std")

#Q1(b)
bookings <- read.table("G:/My Drive/111_2_BACS/HW4/first_bookings_datetime_sample.txt", 
    header = TRUE)
hours  <- as.POSIXlt(bookings$datetime, format="%m/%d/%Y %H:%M")$hour
mins   <- as.POSIXlt(bookings$datetime, format="%m/%d/%Y %H:%M")$min
minday <- hours*60 + mins

#Q1b (i)
minday_std <- standardize(minday)
print(paste("Mean of the minday_std: ", mean(minday_std)))
print(paste("Standard deviation of the minday_std: ", sd(minday_std)))

#Q1a (ii)
plotFunct(minday, "Visualization of minday")
plotFunct(minday_std, "Visualization of minday_std")

#Q2
library(compstatslib)

#Q2 (a)
plot_sample_ci(num_samples = 100, sample_size = 100, pop_size=10000, 
                distr_func=rnorm, mean=20, sd=3)

bad_vec <- c()
for (i in 1:10000){
  bad = visualize_sample_ci(num_samples = 100, sample_size = 100, pop_size=10000, distr_func=rnorm, mean=20, sd=3,draw = F,ci=99)
  bad_vec[i] <- length(bad)
}
mean(bad_vec)

#Q2 (b)
plot_sample_ci(num_samples = 100, sample_size = 300, pop_size=10000, 
                distr_func=rnorm, mean=20, sd=3)

#Q2 (c)
plot_sample_ci(num_samples = 100, sample_size = 300, pop_size=10000,
                distr_func=rnorm, mean=20, sd=3)

#Q3

#Q3a (i)
sd_error <- sd(minday) / length(minday)^0.5
ci95 <- mean(minday) + c(-1.96, 1.96)*sd_error
print(paste("Mean of the minday: ", mean(minday)))
print(paste("Standard deviation error of the minday: ", sd_error))
print(paste("The 95% Confindence Interval is between", ci95[1], "and", ci95[2]))

#Q3a (ii)
resamples <- replicate(3000, sample(minday, length(minday), replace=TRUE))

#Q3a (iii)
plot(
  density(minday), 
  lwd=1, 
  main="Population & Bootstrapped Samples"
)

plot_sample <- function(sample) {
  lines(density(sample), col="#8cd9db")
  return(mean(sample))
}
sample_means <- apply(resamples, 2, FUN=plot_sample)
lines(density(minday), col="red", lwd=2, lty="dashed")

#Q3a (iv)
quantile(sample_means, probs=c(0.025, 0.975))

#Q3b (i)
median(minday)

#Q3b (ii)
plot(density(minday), col="red", lty = "dashed", main="Medians of the 2000 Bootstrapped Samples")
plot_sample_median <- function(sample) {
  abline(
    v=median(sample),
    col="#8cd9db"
  ) 
  
  return(median(sample))
}
sample_medians <- apply(resamples, 2, FUN=plot_sample_median)
abline(
  v=median(minday), 
  col="#6cec5e"
)

#Q3b (iii)
quantile(sample_medians, probs=c(0.025, 0.975))
