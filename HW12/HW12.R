cars <- read.table("G:/My Drive/111_2_BACS/HW12/auto-data.txt", 
    header=FALSE, na.strings = "?")
names(cars) <- c("mpg", "cylinders", "displacement", 
                "horsepower", "weight", "acceleration", 
                "model_year", "origin", "car_name")
vars <- c("mpg", "weight", "acceleration", 
        "model_year", "origin", "cylinders")
cars <- cars[vars]
cars_log <- with(cars, 
            data.frame(log(mpg), log(weight), 
            log(acceleration), model_year, origin, log(cylinders)))

#No 1a
weight_mean_log <- log(mean(cars$weight))
#Light Cars Regression
light_log <- subset(cars_log, log.weight. < weight_mean_log)
light_reg <- with(light_log, lm(log.mpg. ~ log.acceleration.))

#Heavy Cars Regression
heavy_log <- subset(cars_log, log.weight. >= weight_mean_log)
heavy_reg <- with(heavy_log, lm(log.mpg. ~ log.acceleration.))

#Plot
with(light_log, plot(log.acceleration., log.mpg., pch=1, col="blue"))
with(heavy_log, points(log.acceleration., log.mpg., pch=19, col="red"))

#Reg Line
abline(light_reg, col="blue", lwd=2)
abline(heavy_reg, col="red", lwd=2)

#No 1b
regr_light_full <- lm(log.mpg.~ log.weight. + log.acceleration. + model_year + 
                      factor(origin), data=light_log)
regr_heavy_full <- lm(log.mpg.~ log.weight. + log.acceleration. + model_year + 
                      factor(origin), data=heavy_log)
summary(regr_light_full)
summary(regr_heavy_full)

#No 2a
#No 2b
#(i)
no_inter <- lm(log.mpg. ~ log.weight. + log.acceleration. + model_year + 
                factor(origin), data=cars_log)
summary(no_inter)

#(ii)
weight_acc_inter <- lm(log.mpg. ~ log.weight. + log.acceleration. + model_year + 
                factor(origin) + log.weight. * log.acceleration., data=cars_log)
summary(weight_acc_inter)

#(iii)
mean_centralized <- function(data){
        return (scale(data, center=TRUE, scale=FALSE))
}

mean_inter <- lm(mean_centralized(log.mpg.) ~ mean_centralized(log.acceleration.) + 
                mean_centralized(model_year) + mean_centralized(origin) +
                mean_centralized(log.mpg.*log.acceleration.), data=cars_log)
summary(mean_inter)

#(iv)
dot_product <- cars_log$log.weight. * cars_log$log.acceleration.
inter_reg <- lm(dot_product ~ cars_log$log.weight. + cars_log$log.acceleration.)
inter_ortho <- inter_reg$residuals
ortho_inter <- lm(log.mpg. ~ log.weight. + log.acceleration. + model_year +
                factor(origin) + inter_ortho, data=cars_log)
summary(ortho_inter)

#No 2c

#No 3a
reg_weight_acc <- lm(log.weight.~ log.cylinders., data=cars_log)
regr_mpg_weight <- lm(log.mpg.~ log.weight., data=cars_log)

#No 3b

#No 3c
boot_mediation <- function(model1, model2, dataset) {
        boot_index <- sample(1:nrow(dataset), replace=TRUE)
        data_boot <- dataset[boot_index, ]
        regr1 <- lm(model1, data_boot)
        regr2 <- lm(model2, data_boot)
        return(regr1$coefficients[2] * regr2$coefficients[2])
}

set.seed(42)
indirect <- replicate(2000, 
        boot_mediation(reg_weight_acc, regr_mpg_weight, cars_log))
boot_ci <- quantile(indirect, probs=c(0.025, 0.975))

plot(density(indirect), lwd=2, col="cornflowerblue",
        main= "Distribution of the 95% CI of the indirect effect")
abline(v=quantile(indirect, probs=c(0.025, 0.975)))
