#https://www.neoprogrammics.com

options(digits=14)
options(scipen=999)

library(RMoon)

#Se analizan los resultados del fichero OjoconDiasJulianos.bmp


#Se invierte el resultado de la web
inversjulia = JuliantoGregorianDate(2460400.3695718)
julianday1 = JulianDayMeeus(2024, 3, 30, 20, 52, 11)[[1]]

#Se observa más de 1 minuto de diferencia entre la fecha y el resultado JD TT
julianday2 = JulianDayMeeus(2024, 3, 30, 20, 50, 57)[[1]]

#Y por tanto Julianday2 que debería ser igual a 2460400.3695718, difiere en la
#tercera cifra decimal que implica una variación de un minuto, lo que podría ser
#importante para observaciones detalladas


#Dar los siguientes cálculos en un momento cualquiera:

julianday1 = JulianDayMeeus(2024, 3, 31, 11, 42, 16)[[1]]
inversjulia1 = JuliantoGregorianDate(julianday1)

inversjulia2 = JuliantoGregorianDate(2460400.9885417)
julianday2 = JulianDayMeeus(2024, 3, 31, 11, 43, 30)[[1]]
inversjulia3 = JuliantoGregorianDate(julianday2)

#pg 64 Meeus

JuliantoGregorianDate(c(1842713, 1507900.13))

library(gregorian)

#https://clubdellector.edhasa.es/historia/diario-de-cristobal-colon-viernes-12-de-octubre-de-1492/



WeekDayMeeus(c(1492, 1582, 1582, 1582, 1976, 2024), 
             c(10, 10, 10, 10, 2, 4), 
             c(12, 15, 12, 4, 19, 29)
)

help("WeekDayMeeus")

library(lubridate)

# Definir una fecha
mi_fecha <- ymd("1492-10-12")

# Calcular el día de la semana
dia_semana <- weekdays(mi_fecha)

library(lubridate)

mi_fecha1 <- ymd("1582-10-15")

# Calcular el día de la semana
dia_semana1 <- weekdays(mi_fecha1)
dia_semana1

mi_fecha2 <- ymd("1582-10-04")

# Calcular el día de la semana
dia_semana2 <- weekdays(mi_fecha2)
dia_semana2

mi_fecha3 <- ymd("1500-02-29")


library(gregorian)
as_gregorian("1492-10-12") 


