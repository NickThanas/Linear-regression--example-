##Load data and remove NAs
air <- airquality
air <- na.omit(air)

##Data normalization (mean, sd)
for(i in 1:ncol(air)) {
  ymean <- mean(air[, i])
  ysd <- sd(air[, i])
  air[, i] <- (air[, i]-ymean)/ysd
}

##Plot Ozone vs Temp, add best fit
plot(air$Temp, air$Ozone)
abline(lm(air$Ozone~air$Temp))

##Model equation
ozone <- as.matrix(air$Ozone)
temp <- cbind(as.matrix(rep(1, nrow(air))), as.matrix(air$Temp))
theta <- ((solve((t(temp) %*% temp))) %*% t(temp)) %*% ozone
print(theta, digits=3)
