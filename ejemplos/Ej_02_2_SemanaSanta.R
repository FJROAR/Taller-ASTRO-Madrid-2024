#https://www.census.gov/data/software/x13as/genhol/easter-dates.html#par_textimage_1300332353
#https://www.assa.org.au/edm
#Puede haber error: https://www.giangrandi.org/soft/easter/easter.shtml

options(digits=11)
options(scipen=999)

ChristianEaster <- function(Year){
  
    C <- Year %/% 100
    N <- Year - 19 * (Year %/% 19)
    K <- (C - 17) %/% 25
    I <- C - C%/% 4 - (C - K)%/%3 + 19 * N + 15
    I <- I - 30* (I %/% 30)
    I <- I - (I %/% 28)*(1 - (I %/% 28)*(29 %/% (I + 1))*((21 - N) %/% 11))
    J <- Year + Year %/% 4 + I + 2 - C + C %/% 4
    J <- J - 7 * (J %/% 7)
    L = I - J
    M = 3 + (L + 40) %/% 44
    D = L + 28 - 31*(M %/% 4)
    
  return(list(M, D))
  
}

#https://www.jstor.org/stable/48665583
#Gauss's Computation of the Easter Date
ChristianEaster(4763)
ChristianEaster(c(1967, 2024, 2025)) #pg 68 Meeus

