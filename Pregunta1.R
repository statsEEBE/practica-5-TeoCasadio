#Pregunta 1

#distribucion poblacional (de un resultado)

curve(dnorm(x,95.3, 5.7), xlim=c(80, 120), col='red')

set.seed(84)
n <- 4
simul<- rnorm(n, 95.3, 5.7)
sum(simul)

#suma muestral
set.seed(84)
Y <- function(i){sum(rnorm(n, 95.3, 5.7))}
#Y(1)
#sapply(1:10, Y)
simul100000<- sapply(1:100000, Y)
hist(simul100000)


#teoria, valor esperado de Y: E(Y)=n*mu

4*95.3


#dibujemos el teorema
n <- 4
mu <- 95.4
sigma <- 5.7
hist(simul100000, freq=FALSE)
curve(dnorm(x, n*mu, sqrt(n)*sigma), col='red', add=TRUE)

# Si se selecciona de forma aleatoria una muestra de 100 cables,
#varianza de resistencias de la muestra

#teoria: V(Y)=n*sigma^2
n<-100
n*sigma^2
#simulacion(confirmar)
set.seed(84)
n<-100
Y <- function(i){sum(rnorm(n, 95.3, 5.7))}
simul100000<- sapply(1:100000, Y)
hist(simul100000)
var(simul100000)

#c) Se selecciona de forma aleatoria un cable,
#cual es la probabilidad de que la resistencia del cable no sea menos que 103kg

#P(x>103)
mu <- 95.4
sigma <- 5.7
1-pnorm(103, mu, sigma)


#media de la media muestral para n=4
#E(X_bar)
set.seed(84)
n <- 4
X_bar <- function(i){mean(rnorm(n, 95.3, 5.7))}
simul100000 <- sapply(1:100000, X_bar)
hist(simul100000)
mean(simul100000)

#probabilidad de la media muestral
#P(X_bar < 98)
n <- 4
mu <- 95.4
sigma <- 5.7
pnorm(98, mu, sigma/sqrt(n))

#d) Error! 98 por 32. 
# Probabilidad de que varianza de 100 resistencias sea mas grande que 32.
#Distribución de las varianzas
set.seed(84)
n <- 100
SSq <- function(i){var(rnorm(n, 95.3, 5.7))}
simul100000 <- sapply(1:100000, SSq)
hist(simul100000)

#probabilidad estimada de que SSq>32

mean(simul100000>32)

# P(S^2>32)= P(((S^2*(n-1))/sigma^2)>32*(n-1)/sigma^2))

#Teoria P(SSq>32)= P(W>32*n/sigma^2)
#Es una variable aleatoria Chi cuadrado
n <- 100
sigma <- 5.7
1-pchisq(32*(n-1)/sigma^2, n-1)
