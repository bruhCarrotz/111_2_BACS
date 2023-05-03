library(data.table)
library(lsa)

#No 2
salary <- read.csv("G:/My Drive/111_2_BACS/HW10/programmer_salaries.txt", sep="\t")
salary_reg <- lm(salary$Salary ~ salary$Experience +
                          salary$Score + salary$Degree)
summary(salary_reg, data=salary)
head(salary_reg$fitted.values, 5)
head(salary_reg$residuals, 5)

ones <- replicate(length(salary$Salary), 1)
X <- cbind(ones, salary$Experience, salary$Score, salary$Degree)
y <- salary$Salary
beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y # some linear algebra
y_hat <- X %*% beta_hat # predicted values
res <- y - y_hat # residuals
SSR <- sum((y_hat-mean(y))^2)
SSE <- sum((y - y_hat)^2)
SST <- SSR + SSE

#No 3
auto <- read.table(
    "G:/My Drive/111_2_BACS/HW10/auto-data.txt",             
    header=FALSE, na.strings = "?")
names(auto) <- c("mpg", "cylinders", "displacement", 
                "horsepower","weight","acceleration", 
                "model_year", "origin", "car_name")

library(GGally)
ggpairs(auto, columns = 1:8)

round(cor(auto[1:8], use = "pairwise.complete.obs"), 2)

auto_v1 <- lm(auto$mpg ~ 
    auto$cylinders + auto$displacement + auto$horsepower + auto$weight + auto$acceleration + auto$model_year, 
    factor(auto$origin))
summary(auto_v1)





plot(auto$mpg, auto$horsepower, lwd=2, col="blue", main="Distribution of Cars' MPG vs. Horsepower")
plot(auto$weight, auto$acceleration, lwd=2, col="green", main="Distribution of Cars' Weight vs. Acceleration")
hist(auto$origin, lwd=2, col="orange", main="Distribution of Cars' Origin ")
hist(auto$model_year, lwd=2, col="cornflowerblue", main="Distribution of Cars' Model Name")
plot(auto$mpg, auto$acceleration, lwd=2, col="blue", main="Distribution of Cars' MPG vs. Acceleration")
plot(auto$mpg, auto$model_year, lwd=2, col="blue", main="Distribution of Cars' MPG vs. Model Year")





```{r, out.width="70%", fig.align = "center"}
layout(matrix(c(1,2), 1, 2, byrow = TRUE))
plot(auto$mpg, auto$horsepower, 
    lwd=2, col="blue", 
    main="Cars' MPG vs. Horsepower")
plot(auto$weight, auto$acceleration, 
    lwd=2, col="green", 
    main="Cars' Weight vs. Acceleration")
```
```{r, out.width="70%", fig.align = "center"}
layout(matrix(c(1,2), 1, 2, byrow = TRUE))
hist(auto$origin, 
    lwd=2, col="orange", 
    main="Cars' Origin ")
hist(auto$model_year, 
    lwd=2, col="cornflowerblue", 
    main="Cars' Model Name")
```
```{r, out.width="70%", fig.align = "center"}
layout(matrix(c(1,2), 1, 2, byrow = TRUE))
plot(auto$mpg, auto$acceleration, 
    lwd=2, col="blue", 
    main="Cars' MPG vs. Acceleration")
plot(auto$mpg, auto$model_year, 
    lwd=2, col="blue", 
    main="Cars' MPG vs. Model Year")
```
