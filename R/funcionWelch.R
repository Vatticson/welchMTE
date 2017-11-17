#' welch
#'
#' La funcion welch recoge dos muestras de tamanios menores que 30 y devuelve un
#' delta, el cual se utiliza para determinar los grados de libertad adecuados
#' para una t de student a la hora de realizar la aproximacion de Welch. Ahora,
#' no te digo yo que no sino es que si.
#'
#'
#'
#'
#' @param x,y muestras en forma de vectores numericos cuyos tamanios han de ser
#'   menores que 30
#'
#' @return
#' @export
#'
#' @examples
#' #Testeo con las muestras presentes en los apuntes del tema 3 sobre intervalos de confianza.
#' x <- c(31, 18, 17, 16, 37, 16, 32, 13, 14, 49, 25, 19, 13, 32, 27)
#' y <- c(15, 17, 13, 25, 22, 20, 24, 12, 23, 15, 20, 18)
#'
#' welch(x, y)
welch <- function(x,y){
  muestras <- list(x, y)
  names(muestras) <- c("x", "y")

#Comprobamos si x e y son de tipo numerico. En caso negativo, se devuelve un error.
  for(i in 1:2){
    if(!is.numeric(muestras[[i]])){
      stop(paste("El objeto", names(muestras)[i], "no es numerico."))
    }
  }

#Si  el tamaño de alguna muestra es al menos 30, se devuelve un error.

  if(length(x) >= 30 & length(y) >= 30){
    stop("Ambas muestras son mayores o iguales que 30. Utiliza otro estadístico pivote")
  }


#Cálculo de grados de libertad de cada una de las muestras
  n <- length(x)
  m <- length(y)

#Calculamos numerador y denominador de delta de forma separada con
#la finalidad de simplificar la lectura
  num <-((m-1) * var(x)/n - (n-1) * var(y)/m)^2
  denom <-(m-1) * (var(x)/n)^2 + (n-1) * (var(y)/m)^2
  delta <- num/denom

  return(delta)
}
