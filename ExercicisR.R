#Author : Maria Amzil
#Date : 15/05/2025
#Description: Exercicis (projecte en R-studio amb menú per provar les funcions)

#funció que em determina si un nombre és primer
es_primer <- function(n) {
  if (n <= 1) return(FALSE)
  for (i in 2:floor(sqrt(n))) {
    if (n %% i == 0) return(FALSE)
  }
  return(TRUE)
}

#funció recursiva del càlcul de factorial
factorial_recursiu <- function(n) {
  if (n == 0) return(1)
  else return(n * factorial_recursiu(n - 1))
}

suma_divisors <- function(n) {
  suma <- 0
  for (i in 1:(n - 1)) {
    if (n %% i == 0) suma <- suma + i
  }
  return(suma)
}

#funció que m'indica si dos nombres són amics
amics <- function(a, b) {
  return(suma_divisors(a) == b && suma_divisors(b) == a)
}

#funció que m'indica si un nombre és perfecte
es_perfecte <- function(n) {
  return(suma_divisors(n) == n)
}

# MENÚ INTERACTIU

repetir <- TRUE

while (repetir) {
  cat("\nMenú\n")
  cat("1. Comprovar si un nombre és primer\n")
  cat("2. Calcular factorial (recursiu)\n")
  cat("3. Comprovar si dos nombres són amics\n")
  cat("4. Comprovar si un nombre és perfecte\n")
  cat("5. Sortir\n")
  
  opcio <- as.integer(readline("Tria una opció introduint un número de l'1 al 5 (ambdós inclosos): "))
  
  if (opcio == 1) {
    n <- as.integer(readline("Introdueix un nombre: "))
    resultat <- es_primer(n)
    cat(n, "és primer:", resultat, "\n")
    
  } else if (opcio == 2) {
    n <- as.integer(readline("Introdueix un nombre: "))
    resultat <- factorial_recursiu(n)
    cat("El factorial de", n, "és:", resultat, "\n")
    
  } else if (opcio == 3) {
    a <- as.integer(readline("Introdueix el primer nombre: "))
    
    b <- as.integer(readline("Introdueix el segon nombre: "))
    resultat <- amics(a, b)
    cat(a, "i", b, "són amics:", resultat, "\n")
    
  } else if (opcio == 4) {
    n <- as.integer(readline("Introdueix un nombre: "))
    
    resultat <- es_perfecte(n)
    cat(n, "és perfecte:", resultat, "\n")
    
  } else if (opcio == 5) {
    cat("Fi del programa.\n")
    repetir <- FALSE
    
  } else {
    cat("Opció no vàlida. Torna-ho a provar.\n")
  }
}



