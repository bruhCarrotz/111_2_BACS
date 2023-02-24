#Q1a
d1 <- rnorm(n = 500, mean = 45,  sd = 5)
d2 <- rnorm(n = 200, mean = 30,  sd = 5)
d3 <- rnorm(n = 100, mean = 15,  sd = 5)

d123 <- c(d1, d2, d3)

plot(density(d123), col = "darkgreen", lwd = 2, main = "Distribution 2")

abline(v = mean(d123))
abline(v = median(d123), lty = "dashed")

mean(d123)
median(d123)

#Q1b
dist <- rnorm(n = 800, mean = 0,  sd = 0)

plot(density(dist), col = "red", lwd = 2, main = "Distribution 3")

abline(v = mean(dist), lwd = 5, col = "green")
abline(v = median(dist), col = "blue")

mean(dist)
median(dist)

#Q1c

#Q2a
rdata <- rnorm(n = 2000, mean = 0, sd = 1)

plot(density(rdata), col = "orange", lwd = 2, main = "Visualization of Q2a")

abline(v = mean(rdata))
for(i in 1:3) { 
    abline(v = i*sd(rdata), lty = "dashed") 
    abline(v = -i*sd(rdata), lty = "dashed") 
} 

#Q2b
q <- quantile(rdata, probs = c(.25, .5, .75))
result <- (q - mean(rdata)) / sd(rdata)
result

#Q2c
rdata <- rnorm(n = 2000, mean = 35, sd = 3.5)
q <- quantile(rdata, probs = c(.25, .5, .75))
result <- (q - mean(rdata)) / sd(rdata)
result

#Q2d
q <- quantile(d123, probs = c(.25, .5, .75))
result <- (q-mean(d123)) / sd(d123)
result

#Q3a

#Q3b (i-iii)
rand_data <- rnorm(n = 800, mean = 20, sd = 5)
n <- length(rand_data)

#(i) Sturges
bins_sturges <- ceiling(log2(n)) + 1
width_sturges <- (max(rand_data) - min(rand_data)) / bins_sturges
bins_sturges
width_sturges

#(ii) Scotts
width_scott <- (3.49 * sd(rand_data)) / n^(1/3)
bins_scott <- ceiling(max(rand_data) - min(rand_data)) + width_scott
width_scott
bins_scott

#(iii) Freedmen formula
width_fd <- (2*IQR(rand_data)) / n^(1/3)
bins_fd <- ceiling(max(rand_data) - min(rand_data)) + width_fd
width_fd
bins_fd

#Q3c
out_data <- c(rand_data, runif(10, min = 40, max = 60))
n <- length(out_data)

#(i) Sturges
bins_sturges <- ceiling(log2(n)) + 1
width_sturges <- (max(out_data) - min(out_data)) / bins_sturges
bins_sturges
width_sturges

#(ii) Scotts
width_scott <- (3.49 * sd(out_data)) / n^(1/3)
bins_scott <- ceiling(max(out_data) - min(out_data)) + width_scott
width_scott
bins_scott

#(iii) Freedmen formula
width_fd <- (2*IQR(out_data)) / n^(1/3)
bins_fd <- ceiling(max(out_data) - min(out_data)) + width_fd
width_fd
bins_fd