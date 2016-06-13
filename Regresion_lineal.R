###Cálculo Regresion lineal 
##Relación entre velocidad y distancia de frenado

cars
lm(dist~speed, cars)


#Call:
 # lm(formula = dist ~ speed, data = cars)

#Coefficients:
#  (Intercept)        speed  
#-17.579        3.932  

##Comprobación valores 
df <- cars
plot(df)

#Cálculo varianza
var <- sum((df$speed-meanspeed)^2)/(50-1)
var(df$speed)
#Desviación típica
sqrt(var)
sd(df$speed)

#Cálculo Desglosado
#x = speed y=dist, speed es la variable independiente.
df$x2 <- df$speed^2
df$y2 <- df$dist^2
df$xy <- df$speed*df$dist
meanspeed <- mean(df$speed)
meandist <- mean(df$dist)

dx2 <- sum(df$x2)-((sum(df$speed)^2)/nrow(df))

dy2 <- sum(df$y2)-((sum(df$dist)^2)/nrow(df))

dxy <- sum(df$xy)-(sum(df$speed)*sum(df$dist))/nrow(df)

#Coeficiente de correlación                              
r <- dxy/sqrt(dx2*dy2)
r #0.8068949

comprobacion:
  
cor(df$speed,df$dist)

##Coeficientes linea regresion lineal y=bx+a

b <- dxy/dx2
b #3.932409
a <- meandist-b*meanspeed
a #-17.57

reg <- lm(dist~speed, cars)
plot(cars)+ abline(reg)
