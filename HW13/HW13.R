library(car)
library(readxl)

cars <- read.table("G:/My Drive/111_2_BACS/HW13/auto-data.txt", 
    header=FALSE, na.strings = "?")
names(cars) <- c("mpg", "cylinders", "displacement", 
                "horsepower", "weight", "acceleration", 
                "model_year", "origin", "car_name")
cars_log <- with(cars, data.frame(
        log(mpg), log(cylinders), log(displacement), 
        log(horsepower), log(weight), 
        log(acceleration), model_year, origin))
cars_log <- na.omit(cars_log)

#No 1a-(i)
reg_all <- lm(log.mpg. ~ log.cylinders. + log.displacement. + log.horsepower. + 
        log.weight. + log.acceleration. + model_year + factor(origin), 
        data=cars_log)
summary(reg_all)
vif(reg_all)
multi_col <- c("log.cylinders.", "log.displacement.",
        "log.horsepower.", "log.weight.")
cars_multi_col <- cars_log[multi_col]
summary(cars_multi_col)

#No 1a-(ii)
cars_eigen <- eigen(cor(cars_multi_col))
cars_eigenval <- cars_eigen$values
cars_eigenval[1]

#No 1a-(iii)
cars_eigenvec <- cars_eigen$vectors
cars_eigenvec[,1]

#No 1b-(i)
cars_pca <- prcomp(cars_multi_col)
scores <- cars_pca$x
cars_log$composite_score <- scores[,"PC1"]

#No 1b-(ii)
reg_pca <- lm(log.mpg. ~ composite_score + log.acceleration. +
        model_year + factor(origin), data=cars_log)
summary(reg_pca)

#No 1b-(iii)
reg_pca_std <- lm(log.mpg. ~ scale(composite_score) + scale(log.acceleration.) +
        scale(model_year) + factor(origin), data=cars_log)
summary(reg_pca_std)
#No 2
security <- read_excel(
        "G:/My Drive/111_2_BACS/HW13/security_questions.xlsx",
        sheet = "data")
#No 2a
security_eigen <- eigen(cor(security))

#No 2b
security_pca <- prcomp(security, scale=FALSE)
screeplot(security_pca, type="lines")

#No 2c
summary(security_pca)
