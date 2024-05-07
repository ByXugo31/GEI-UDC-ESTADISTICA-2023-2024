#Calculo de cuartiles
cuartilCalc <- function(cuartil,numero,probabilidad){
  if(cuartil==0) cuart <- 0
  if(cuartil==1) cuart <- 0.25
  if(cuartil==2) cuart <- 0.5
  if(cuartil==3) cuart <- 0.75
  if(cuartil==4) cuart <- 1
  return(qbinom(cuart, numero, probabilidad))
}

cuartilCalc(1,10,0.82)

#Sea X una v.a. Exp(X). Calcula el valor de a tal que P(X<A)=Z.
solve1 <- function(X,Z) {
  a <- uniroot(function(x) pexp(x, rate = X) - Z, interval = c(0, 10))
  return(a$root)
}

#Ejemplo con X=2 y Z=0.3
solve1(2,0.3)

#Sea X una variable aleatoria Exp(A), calcular P(X>B)
probCalc <- function(B,A){
  pexp(B,A,lower.tail = FALSE)
}

probCalc()

#Distribuicion exponencial
expFunc <- function(media, x) {
  lambda <- 1/media
  p_x_equals_x <- dexp(x, rate = lambda)
  p_x_less_than_x <- pexp(x, rate = lambda, lower.tail = TRUE)
  p_x_greater_than_x <- pexp(x, rate = lambda, lower.tail = FALSE)
  p_x_less_or_equal_x <- p_x_less_than_x
  p_x_greater_or_equal_x <- p_x_greater_than_x
  
  cat(" P(X =", x, "):", p_x_equals_x, "\n",
      "P(X <", x, "):", p_x_less_than_x,"\n",
      "P(X >", x, "):", p_x_greater_than_x,"\n",
      "P(X <=", x, "):",p_x_less_or_equal_x,"\n",
      "P(X >=", x, "):",p_x_greater_or_equal_x,"\n")
}

# Ejemplo de uso con media 2 y x=3
expFunc(15,18)


#Distribuicion binomial
calcular_probabilidades <- function(n, p, x) {
  dbinom_x_equals_x <- dbinom(x, n, p)
  pbinom_x_greater_than_x <- sum(dbinom(x:n, n, p))
  pbinom_x_less_than_x <- sum(dbinom(0:(x-1), n, p))
  pbinom_x_less_or_equal_x <- sum(dbinom(0:x, n, p))
  pbinom_x_greater_or_equal_x <- sum(dbinom(x:n, n, p))
  
  cat(" P(X =", x, "):", dbinom_x_equals_x, "\n",
      "P(X >", x, "):", pbinom_x_greater_than_x,"\n",
      "P(X <", x, "):", pbinom_x_less_than_x,"\n",
      "P(X <=", x, "):",pbinom_x_less_or_equal_x,"\n",
      "P(X >=", x, "):",pbinom_x_greater_or_equal_x,"\n")
}

# Ejemplo de uso con B(12,0.8) y x=6
calcular_probabilidades(12,0.8,6)


#Calculo de cuantiles
cuantilCalc <- function(A,B,C) {
  return(qbinom(C, A, B))
}
  
#Ejemplo de uso con B(12,0.8) y calculo del cuantil 0.25
cuantilCalc(12,0.8,0.25)

#Distribuición de Poisson
PoisonCalc <- function(lambda, x) {
  p_x <- dpois(x, lambda)
  p_less_x <- ppois(x-1, lambda)
  p_less_equal_x <- ppois(x, lambda)
  p_greater_x <- 1 - p_less_equal_x
  p_greater_equal_x <- 1 - p_less_x
  
  cat(" P(X = x):", p_x, "\n" ,
       "P(X < x)", p_less_x,"\n" , 
       "P(X > x)", p_greater_x, "\n" ,
       "P(X <= x)", p_less_equal_x, "\n" ,
       "P(X >= x)", p_greater_equal_x)
}

#Ejemplo de uso con lambda (media) = 30 y x = 25
PoisonCalc(30,25)

#Ejercicio de examen de Poisson
#La cantidad de mensajes de correo electrónico que recibe una revista es una variable de Poisson con una media de 30 mensajes por hora.
#Calcula la probabilidad de que se reciban 40 mensajes en hora y media
#Para resolverlo la media sería 30 * 1.5 y x seria 40.

#Distribuición normal
NormalCalc <- function(media, desviacion, x) {
  p_x <- dnorm(x, media, desviacion)
  p_less_x <- pnorm(x, media, desviacion, lower.tail = TRUE)
  p_greater_x <- pnorm(x, media, desviacion, lower.tail = FALSE)
  p_less_equal_x <- pnorm(x, media, desviacion, lower.tail = TRUE)
  p_greater_equal_x <- p_greater_x
  
  cat(" P(X = x):", p_x, "\n" ,
      "P(X < x)", p_less_x,"\n" , 
      "P(X > x)", p_greater_x, "\n" ,
      "P(X <= x)", p_less_equal_x, "\n" ,
      "P(X >= x)", p_greater_equal_x)
}

#Ejemplo de uso con media = 0.1 desviacion = 1.2 y x = 1.2
NormalCalc(0.1,1.2,1.2)

#Distribuición normal en un intervalo P(A<X<B)
NormalInterval <- function(media, desviacion, A, B) {
  prob_B <- pnorm(B, mean = media, sd = desviacion)
  prob_A <- pnorm(A, mean = media, sd = desviacion)
  prob_A_X_B <- prob_B - prob_A
  prob_X_fuera_intervalo <- 1 - prob_A_X_B
  cat("P(A<X<B) = ",  prob_A_X_B, "\n" ,
      "P(X esté fuera del intervalo A B) = ", prob_X_fuera_intervalo)
}

#Ejemplo de uso con A = 6 B = 8 media = 14 y desviacion = 6
NormalInterval(14,6,6,8)
