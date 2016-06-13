Cálculo Regresion lineal
------------------------

Relación entre velocidad y distancia de frenado

``` r
head(cars)
```

    ##   speed dist
    ## 1     4    2
    ## 2     4   10
    ## 3     7    4
    ## 4     7   22
    ## 5     8   16
    ## 6     9   10

``` r
lm(dist~speed, cars)
```

    ## 
    ## Call:
    ## lm(formula = dist ~ speed, data = cars)
    ## 
    ## Coefficients:
    ## (Intercept)        speed  
    ##     -17.579        3.932

Comprobación valores
--------------------

``` r
df <- cars
plot(df)
```

![](reglineal_calculo_files/figure-markdown_github/unnamed-chunk-2-1.png) \#Cálculo varianza

``` r
meanspeed <- mean(df$speed)
meandist <- mean(df$dist)
var <- sum((df$speed-meanspeed)^2)/(50-1)
var
```

    ## [1] 27.95918

``` r
var(df$speed)
```

    ## [1] 27.95918

Desviación típica
=================

Raíz cuadrada de la varianza:

``` r
sqrt(var)
```

    ## [1] 5.287644

``` r
sd(df$speed)
```

    ## [1] 5.287644

Covarianza:

``` r
sum((((df$speed-meanspeed))*((df$dist-meandist))))*(1/(50-1))
```

    ## [1] 109.9469

``` r
cov(df$speed,df$dist)
```

    ## [1] 109.9469

Cálculo Desglosado
==================

x = speed y=dist, speed es la variable independiente.

``` r
df$x2 <- df$speed^2
df$y2 <- df$dist^2
df$xy <- df$speed*df$dist


dx2 <- sum(df$x2)-((sum(df$speed)^2)/nrow(df))

dy2 <- sum(df$y2)-((sum(df$dist)^2)/nrow(df))

dxy <- sum(df$xy)-(sum(df$speed)*sum(df$dist))/nrow(df)
```

**Coeficiente de correlación**

``` r
r <- dxy/sqrt(dx2*dy2)
r 
```

    ## [1] 0.8068949

Existe una correlación positiva alta, lo que significa que a mayor valor de x mayor valor de y.

comprobacion:

``` r
cor(df$speed,df$dist)
```

    ## [1] 0.8068949

Coeficientes regresion lineal y=bx+a
------------------------------------

``` r
b <- dxy/dx2
b 
```

    ## [1] 3.932409

``` r
a <- meandist-b*meanspeed
a 
```

    ## [1] -17.57909

``` r
reg <- lm(dist~speed, cars)
plot(cars)+ abline(reg)
```

![](reglineal_calculo_files/figure-markdown_github/unnamed-chunk-11-1.png)

    ## numeric(0)
