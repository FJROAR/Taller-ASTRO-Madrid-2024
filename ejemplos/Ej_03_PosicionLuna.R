options(digits=11)
options(scipen=999)

#library(devtools)
#install_github("FJROAR/RMoon", force = T)
#remove.packages("RMoon")

library(RMoon)

JulianDayMeeus(c(1992),
               c(4), 
               c(12), 
               c(0), 
               c(0), 
               c(0))[[1]]

Julday <- JulianDayMeeus(c(1992),
                         c(4), 
                         c(12), 
                         c(0), 
                         c(0), 
                         c(0))[[2]]

Julday

Moonbasics <- MoonBasicElements(Julday)
Moonbasics

MoonLongdist <- MoonGeoLongDist(Moonbasics[[1]],
                            Moonbasics[[2]],
                            Moonbasics[[3]],
                            Moonbasics[[4]],
                            Moonbasics[[5]],
                            Moonbasics[[6]],
                            Moonbasics[[7]],
                            Moonbasics[[9]])

PeriodicLongDist

MoonLat <- MoonGeoLat(Moonbasics[[1]],
                  Moonbasics[[2]],
                  Moonbasics[[3]],
                  Moonbasics[[4]],
                  Moonbasics[[5]],
                  Moonbasics[[6]],
                  Moonbasics[[8]],
                  Moonbasics[[9]])


#Conversion to Ecuatorian

Ecliptica <- TrueObliqutiyEcliptic(Julday)

#Ecliptica con efecto de nutación

EcliptNut <- Ecliptica[[1]] + NutationLong(Julday)

#Estimación de la Ascensión Recta y Declinación

Coordenadas <- Ecliptic2Equatorial(MoonLongdist[[1]] + NutationLong(Julday),
                                   MoonLat,
                                   Ecliptica[[1]])

Coordenadas

