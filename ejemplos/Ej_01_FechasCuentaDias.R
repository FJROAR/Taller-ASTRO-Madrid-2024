options(digits=11)
options(scipen=999)

library(RMoon)

#

listaDias <- list(c(1582, 1582, 2000, -123, -4712, 1500, 1700), #Años
                  c(10, 10, 1, 12, 1, 2, 2), #Meses
                  c(4, 15, 1, 31, 1, 29, 29), # Día
                  c(0, 0, 12, 0, 12, 0, 0), #Hora
                  c(0, 0, 0, 0, 0, 0, 0), #Minuto
                  c(0, 0, 0, 0, 0, 0, 0) #Segundo
                  ) 
help("JulianDayMeeus")

Fechas <- JulianDayMeeus(listaDias[[1]],
                         listaDias[[2]],
                         listaDias[[3]],
                         listaDias[[4]],
                         listaDias[[5]],
                         listaDias[[6]])

Fecha01 <- Fechas[[1]][1]
Fecha02 <- Fechas[[1]][2]
Fecha03 <- Fechas[[1]][3]
Fecha04 <- Fechas[[1]][4]
Fecha05 <- Fechas[[1]][5]
Fecha06 <- Fechas[[1]][6]
Fecha07 <- Fechas[[1]][7]

#Se observa por tanto que si se aplica diferencia de días entre una fecha
#superior al 04-10-1582 y otra inferior, se tendría un problema al contarse
#días que no existen

Fecha02 - Fecha01

#Por otro lado la fórmula anterior preserva los bisiestos en el calendario
#Juliano que es el que usa antes del 4-Oct-1582 y los elimina, cuando debe
#según el Gregoriano después del 15-Oct-1582

help("JuliantoGregorianDate")


Fecha06 #El 1500 no debería ser bisiesto según el gregoriano, sin embargo
        #El día existe en el Juliano como tal
JuliantoGregorianDate(Fecha06 - 1)
JuliantoGregorianDate(Fecha06)
JuliantoGregorianDate(Fecha06 + 1)


Fecha07 #El 1700 no debería ser bisiesto según el gregoriano, y por tanto
        #La asociación está bien hecha ya que se observa que

JuliantoGregorianDate(Fecha07 - 1)
JuliantoGregorianDate(Fecha07)
JuliantoGregorianDate(Fecha07 + 1)


#En el siguiente ejemplo se muestra como funciona y se re-ajusta la función anterior
#de modo que el resultado entre fechas que contengan el 1582-10-05 y 1582-10-15 no
#serían válidos y en todo el resto de rango de fechas, la diferencia se hace correctamente,
#es decir, no tiene sentido calcular la diferencia entre los días 1582-10-13 y 
#cualquier otra fecha sencillamente porque 1582-10-13 no existió y ni tampoco se tiene
#en cuenta según este algoritmo 1582-10-15 que sí existió pero es el único punto de
#desajuste de la fórmula que se podría corregir en un programa como el anterior

help("DiffDaysMeeus")

M1 <-matrix(nrow = 2, ncol = 6)
M2 <-matrix(nrow = 2, ncol = 6)

M1[1, ] <- c(listaDias[[1]][1], 
             listaDias[[2]][1],
             listaDias[[3]][1],
             listaDias[[4]][1],
             listaDias[[5]][1],
             listaDias[[6]][1])

M1[2, ] <- c(listaDias[[1]][1], 
             listaDias[[2]][1],
             listaDias[[3]][1],
             listaDias[[4]][1],
             listaDias[[5]][1],
             listaDias[[6]][1])


M2[1, ] <- c(listaDias[[1]][2], 
             listaDias[[2]][2],
             listaDias[[3]][2],
             listaDias[[4]][2],
             listaDias[[5]][2],
             listaDias[[6]][2])
M2[2, ] <- c(1582, 
             10,
             16,
             0,
             0,
             0)


DiffDaysMeeus(M1, M2)


#En R la función que existe calcula mal la diferencia de 2 días
diferencia1 <- difftime(as.Date("1582-10-04"), as.Date("1582-10-15"), units = "days")
diferencia1

#R obliga a usar en todo momento el gregoriano, cuando las fuentes históricas
#usan en general el juliano
diferencia2 <- difftime(as.Date("1500-03-01"), as.Date("1500-02-28"), units = "days")
diferencia2

#Sin embargo la función no tiene en cuenta la modificación gregoriana respecto a la 
#eliminación de días
diferencia3 <- difftime(as.Date("1582-10-14"), as.Date("1582-10-16"), units = "days")
diferencia3

M1 <-matrix(nrow = 1, ncol = 6)
M2 <-matrix(nrow = 1, ncol = 6)

M1[1, ] <- c(1500, 03, 1, 0, 0, 0)
M2[1, ] <- c(1500, 02, 28, 0, 0, 0)

DiffDaysMeeus(M1, 
              M2)


#¿Cuándo descubrió Colón América? ¿Qué día de la semana era?

diferencia <- difftime(as.Date("1582-10-04"), as.Date("1492-10-12"), units = "days")
diferencia

colon <- JulianDayMeeus(1582, 10, 4, 0, 0, 0)[[1]] - 32863
JuliantoGregorianDate(colon)

difftime(as.Date("1500-03-01"), as.Date("1500-02-28"), units = "days")
