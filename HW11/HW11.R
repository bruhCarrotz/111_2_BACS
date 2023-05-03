#No 1
cars <- read.table("G:/My Drive/111_2_BACS/HW11/auto-data.txt", 
    header=FALSE, na.strings = "?")
names(cars) <- c("mpg", "cylinders", "displacement", "horsepower", "weight", 
                    "acceleration", "model_year", "origin", "car_name")
cars_log <- with(cars, data.frame(log(mpg), log(cylinders), log(displacement),
                                  log(horsepower), log(weight), 
                                  log(acceleration), model_year, origin))

#1a
reg <- lm(log.mpg. ~ log.cylinders. + log.displacement. +
     log.horsepower. + log.weight. + log.acceleration. +
     model_year +  factor(origin),
     data=cars_log, na.action=na.exclude)
summary(reg)

#1b
regr_wt <- lm(mpg ~ weight, data=cars, na.action=na.exclude)
regr_wt_log <- lm(log.mpg. ~ log.weight., 
    data=cars_log, na.action=na.exclude)

plot(density(regr_wt$residuals), col="darkgreen", lwd=2, 
    main="Residuals of Weight (Raw Data)")
plot(density(regr_wt_log$residuals), col="blue", lwd=2, 
    main="Residuals of Weight (Log Transformed)")

plot(regr_wt_log$residuals, regr_wt_log$log.weight.,
     col="orange", lwd=2,
     main="Scatterplot Log Weight vs. Residuals")

#Problem 2
library(car)

regr_log <- lm(log.mpg. ~ log.cylinders. + log.displacement. 
        + log.horsepower. + log.weight. + log.acceleration. + 
        model_year + factor(origin), data=cars_log)
summary(regr_log)
r2 <- summary(regr_log)$r.squared
vif <- 1 / (1 - r2)
vif

regr_log_vif <- vif(regr_log)
regr_log_vif

org_regr <- lm(mpg ~ cylinders + displacement + 
            horsepower + weight + acceleration + 
            model_year + origin, data = cars)
summary(org_regr)

cars <- cars[-9]
cor(cars$mpg, cars)
cor(cars_log$log.mpg., cars_log)

#Problem 3
origin_colors = c("blue", "darkgreen", "red")
with(cars_log, 
    plot(log.weight., log.mpg., pch=origin, 
    main = "MPG v.s. Weight: different origins",
    col=origin_colors[origin]))
legend(8.3, 3.7, c("USA", "Europe", "Japan"),
 col = c("blue", "darkgreen", "red"),
 pch = c(20,20,20))

USA <- subset(cars_log, origin==1)
reg_USA <- lm(log.mpg. ~ log.weight., data=USA)
abline(reg_USA, col=origin_colors[1], lwd=2)

europe <- subset(cars_log, origin==2)
reg_europe <- lm(log.mpg. ~ log.weight., data=europe)
abline(reg_europe, col=origin_colors[2], lwd=2)

japan <- subset(cars_log, origin==3)
reg_japan <- lm(log.mpg. ~ log.weight., data=japan)
abline(reg_japan, col=origin_colors[3], lwd=2)

regr_log <- lm(log.mpg. ~ log.cylinders. + log.displacement. + 
            log.horsepower. + log.weight. + log.acceleration. + 
            model_year + factor(origin), data=cars_log)
vif(regr_log)
regr_log <- lm(log.mpg. ~ log.cylinders. + 
            log.horsepower. + log.weight. + log.acceleration. + 
            model_year + factor(origin), data=cars_log)
vif(regr_log)
regr_log <- lm(log.mpg. ~ log.cylinders. + 
            log.weight. + log.acceleration. + 
            model_year + factor(origin), data=cars_log)
vif(regr_log)
