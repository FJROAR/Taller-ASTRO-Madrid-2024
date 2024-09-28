options(digits=11)
options(scipen=999)

#library(devtools)
#install_github("FJROAR/RMoon", force = T)
#remove.packages("RMoon")

library(RMoon)

MoonSkyPosition <- function(year, month, day, hour, minute, second,
                            Long,
                            Lat){
  
  #  year = c(2024, 2024, 2024)
  #  month = c(9, 9, 9)
  #  day = c(1, 1, 1)
  #  hour = c(19, 20, 21)
  #  minute = c(0, 0, 0)
  #  second = c(0, 0, 0)
  #  Long = -3 + 37/60 + 40/3600
  #  Lat = 40 + 28/60 + 42/3600
  
  library(swephR)
  
  n_dat = length(year)
  
  H <- vector("numeric", n_dat)
  Hor1 <- vector("numeric", n_dat)
  Hor2 <- vector("numeric", n_dat)
  
  for(i in c(1: n_dat)){
    
    Julday <- JulianDayMeeus(year[i], month[i], day[i], 
                             hour[i], minute[i], second[i])[[2]]
    
    MoonBasics <- MoonBasicElements(Julday)
    MoonLongDist <- MoonGeoLongDist(MoonBasics[[1]],
                                    MoonBasics[[2]],
                                    MoonBasics[[3]],
                                    MoonBasics[[4]],
                                    MoonBasics[[5]],
                                    MoonBasics[[6]],
                                    MoonBasics[[7]],
                                    MoonBasics[[9]])
    
    MoonLat <- MoonGeoLat(MoonBasics[[1]],
                          MoonBasics[[2]],
                          MoonBasics[[3]],
                          MoonBasics[[4]],
                          MoonBasics[[5]],
                          MoonBasics[[6]],
                          MoonBasics[[8]],
                          MoonBasics[[9]])
    
    #To ecuatorian
    
    Ecliptica <- TrueObliqutiyEcliptic(Julday)
    EclNut <- Ecliptica[[1]] + NutationLong(Julday)
    
    Coordenadas <- Ecliptic2Equatorial(MoonLongDist[[1]] + NutationLong(Julday),
                                       MoonLat,
                                       Ecliptica[[1]])
    
    #Obtención de la hora en Greenwich
    
    
    gst <- swe_sidtime(JulianDayMeeus(year[i], month[i], day[i], 
                                      hour[i], minute[i], second[i])[[1]])
    
    longitud_horas <- Long / 15  
    
    lst <- gst + longitud_horas
    lst <- lst %% 24
    
    H[i] <- lst - Coordenadas[[1]]
    
    Hor1[i] <- Equatorial2Horizontal(H[i]*15,
                                Coordenadas[[2]],
                                Lat)[[1]]
    

    Hor2[i] <- Equatorial2Horizontal(H[i]*15,
                                    Coordenadas[[2]],
                                    Lat)[[2]]
    
    
  }
  
  
  
  return(list(H, Hor1, Hor2))
    
}



MoonSkyPosition(year = c(2024),
                month = c(9),
                day = c(27),
                hour = c(2),
                minute = c(0),
                second = c(0),
                Long = -3 + 37/60 + 40/3600,
                Lat = 40 + 28/60 + 42/3600 )


Trayectoria <- MoonSkyPosition(year = c(2024, 2024, 2024, 2024, 2024, 2024,
                                        2024, 2024, 2024, 2024, 2024, 2024,
                                        2024, 2024, 2024, 2024, 2024, 2024,
                                        2024, 2024, 2024, 2024, 2024, 2024,
                                        2024),
                               month = c(9, 9, 9, 9, 9, 9,
                                         9, 9, 9, 9, 9, 9,
                                         9, 9, 9, 9, 9, 9,
                                         9, 9, 9, 9, 9, 9,
                                         9),
                               day = c(1, 1, 1, 1, 1, 2, 
                                       2, 2, 2, 2, 2, 2, 
                                       2, 2, 2, 2, 2, 2, 
                                       2, 2, 2, 2, 2, 2,
                                       2),
                               hour = c(19, 20, 21, 22, 23, 0,
                                        1, 2, 3, 4, 5, 6,
                                        7, 8, 9, 10, 11, 12,
                                        13, 14 , 15, 16, 17, 18,
                                        19),
                               minute = c(0, 0, 0, 0, 0, 0,
                                          0, 0, 0, 0, 0, 0,
                                          0, 0, 0, 0, 0, 0,
                                          0, 0, 0, 0, 0, 0,
                                          0),
                               second = c(0, 0, 0, 0, 0, 0,
                                          0, 0, 0, 0, 0, 0,
                                          0, 0, 0, 0, 0, 0,
                                          0, 0, 0, 0, 0, 0,
                                          0),
                               Long = -3 + 37/60 + 40/3600,
                               Lat = 40 + 28/60 + 42/3600 )


azimuth = Trayectoria[[2]]
altura = Trayectoria[[3]]

library(plotly)


trayectoriaplana <- plot_ly(x = azimuth) %>% 
  add_trace(y = ~altura, mode = 'lines')




library(rayshader)
library(ggplot2)
library(rgl)

# Convertir grados a radianes
azimuths_rad <- azimuth * pi / 180
alturas_rad <- altura * pi / 180

# Calcular las coordenadas cartesianas
x <- cos(alturas_rad) * cos(azimuths_rad)
y <- cos(alturas_rad) * sin(azimuths_rad)
z <- sin(alturas_rad)



# Crear una esfera para visualización
theta <- seq(0, pi, length.out = 100)
phi <- seq(0, 2*pi, length.out = 100)
sphere_x <- outer(sin(theta), cos(phi))
sphere_y <- outer(sin(theta), sin(phi))
sphere_z <- outer(cos(theta), rep(1, length(phi)))

# Crear el círculo ecuatorial en altura = 0º
circle_azimuths <- seq(0, 2*pi, length.out = 100)
circle_x <- cos(circle_azimuths)
circle_y <- sin(circle_azimuths)
circle_z <- rep(0, length(circle_azimuths))  # Altura 0º

# Crear el círculo vertical
# Crear el círculo vertical que pasa por azimuth 90º y 270º, y altura 90º y -90º
theta_circle <- seq(-pi/2, pi/2, length.out = 100)  # Desde el polo norte hasta el polo sur
circle_x_vertical <- c(cos(pi/2) * cos(theta_circle), cos(3*pi/2) * cos(theta_circle))
circle_y_vertical <- c(cos(pi/2) * sin(theta_circle), cos(3*pi/2) * sin(theta_circle))
circle_z_vertical <- c(sin(theta_circle), sin(theta_circle))

# Determinar los colores de los puntos de azimuth y altura
point_colors <- ifelse(z > 0, "yellow", "gray")

# Abrir una nueva ventana 3D con rgl
rgl::open3d()

# Graficar la esfera semitransparente usando rgl
rgl::surface3d(sphere_x, sphere_y, sphere_z, color = "lightblue", alpha = 0.2)

# Graficar el círculo ecuatorial en altura = 0º
rgl::lines3d(circle_x, circle_y, circle_z, color = "blue", lwd = 3)

# Graficar el círculo vertical
rgl::lines3d(circle_x_vertical, circle_y_vertical, circle_z_vertical, color = "red", lwd = 3)


# Graficar los puntos en la esfera con colores según la altura
rgl::points3d(x, y, z, color = point_colors, size = 15)

# Configurar la vista para ver la esfera desde arriba
rgl::view3d(userMatrix = rgl::rotationMatrix(-pi/2, 1, 0, 0), zoom = 1)