library(ISLR)
weather_data <- read.csv("Weather Data.csv")

# Predict temperature (Temp_C) based on:
# Dew point, relative humidity, and pressure.

plot(weather_data)
colnames(weather_data)
plot(weather_data[2:7])


temp = weather_data$Temp_C
dewPointTemp = weather_data$Dew.Point.Temp_C
humidity = weather_data$Rel.Hum_
pressure = weather_data$Press_kPa


#  _______________________ Temperature vs Dew Point _______________________
m1.dewTemp = lm(temp ~ dewPointTemp)
plot(m1.dewTemp)
plot(dewPointTemp, temp, xlab="Dew Point", ylab="Temperature", main="Temperature (°C) vs Dew Point")
abline(m1.dewTemp, col="blue", lwd=2)
summary(m1.dewTemp) # # Adjusted R-squared:  0.8699 

# Quadratic relationships
m1.dewTempQuad <- lm(temp ~ dewPointTemp + I(dewPointTemp^2)) # Adjusted R-squared: 0.87
plot(m1.dewTempQuad)
summary(m1.dewTempQuad) # Adjusted R-squared: 0.87


#any(is.infinite(1/weather_data$Dew.Point.Temp_C))  # Check for Inf

# create new column to adjust values
weather_data$Dew.Point.Temp_C_adj <- weather_data$Dew.Point.Temp_C + 0.001
dewPointTempAdj = weather_data$Dew.Point.Temp_C_adj;

# inverse
m1.dewTempInverse <- lm(temp ~ dewPointTempAdj + I(1/dewPointTempAdj)) 
summary(m1.dewTempInverse)  # Adjusted R-squared:  0.8702 
plot(m1.dewTempInverse)


x <- dewPointTempAdj
xmesh <- seq(min(x)-10, max(x)+10, by=0.1)
yhat <- predict(m1.dewTempInverse, 
                newdata=data.frame(dewPointTempAdj=xmesh))

plot(dewPointTemp, temp, xlab="Dew Point", ylab="Temperature", main="Temperature (°C) vs Dew Point")
abline(m1.dewTemp, col="blue", lwd=2)
lines(xmesh, yhat, col="yellow", lwd=2) 
legend("topright", 
       c("Linear", "Inverse"), 
       lty=c(1,1),
       lwd=c(2,2),
       col=c("blue", "yellow") # colors of lines
)

# _____________________________________________________________________________

#  _______________________ Temperature vs Humidity _______________________
m1.hum = lm(temp ~ humidity)
plot(m1.hum)
plot(humidity, temp, xlab="Humidity", ylab="Temperature", main="Temperature (°C) vs Humidity")
abline(m1.hum, col="green", lwd=2)
summary(m1.hum) # Adjusted R-squared:   0.04837
# _____________________________________________________________________________

#  _______________________ Temperature vs Humidity _______________________
m1.press = lm(temp ~ pressure)
plot(m1.press)
plot(pressure, temp, xlab="Pressure", ylab="Temperature", main="Temperature (°C) vs Pressure")
abline(m1.press, col="purple", lwd=2)
summary(m1.press) # Adjusted R-squared:   0.05577
# _____________________________________________________________________________

# _________________________________Model 2_____________________________________
m2.dewTemp.hum = lm(temp ~ dewPointTemp + humidity)
summary(m2.dewTemp.hum) # Adjusted R-squared:  0.9951; AIC: 21420.31

m2.dewTemp.hum.press = lm(temp ~ dewPointTemp + humidity + pressure) 
summary(m2.dewTemp.hum.press) # Adjusted R-squared:  0.9951; AIC: 21388.26  

m2.1 <- lm(temp ~ dewPointTempAdj + I(dewPointTempAdj^2) + I(1/dewPointTempAdj) + 
             humidity + I(humidity^2) + I(1/humidity) + 
             pressure + I(pressure^2) + I(1/pressure)) 
summary(m2.1) # Adjusted R-squared:  0.9993; 

# removed I(1/dewPointTempAdj). summary showed it is insig.
m2.2 <- lm(temp ~ dewPointTempAdj + I(dewPointTempAdj^2) + 
             humidity + I(humidity^2) + I(1/humidity) + 
             pressure + I(pressure^2) + I(1/pressure))  
summary(m2.2) # Adjusted R-squared:  0.9993;

m2.int_dew_hum <- lm(temp ~ dewPointTemp*humidity)
summary(m2.int_dew_hum)
AIC(m2.int_dew_hum)

m2.int_dew_press_hum <- lm(temp ~ dewPointTemp*pressure*humidity)
summary(m2.int_dew_press_hum)
AIC(m2.int_dew_press_hum)

# making a model using ALL predictors that are possible
# m2.all <- lm(temp ~ ., data=weather_data) # Crashes R studio when I run this
# summary(m2.all)

# all predictors but removing carb:
# m2.noDewAdj <- lm(temp ~ . - weather_data$Dew.Point.Temp_C_adj, data=mtcars)
# summary(m2.noDewAdj)

m1 <- m1.dewTempInverse
m2 <- m2.2
AIC(m1, m2)

# AIC(m1.dewTemp, m1.dewTempQuad, m1.dewTempInverse, m2.dewTemp.hum, m2.dewTemp.hum.press, m2.1, m2.2)

# AICs
#m1.dewTemp         3 50206.459
#m1.dewTempQuad     4 50205.546
#m1.dewTempInverse  4 50192.465
#m1.hum             3 67688.247
#m1.press           3 67619.668
#m2.dewTemp.hum     4 21420.305
#m2.dewTemp.hum.press  5 21388.261
#m2.1              11  4809.765
#m2.2             10  4808.430

