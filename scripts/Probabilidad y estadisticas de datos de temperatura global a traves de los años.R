#variable continua #tickets soporte

rm(list = ls())
library(readxl)

archivo <- file.choose()
datos <- read_excel(archivo)



variable_continua <- "Mean"



#Se elije aplicar regla de Sturges ya que es es una variable continua y tiene bastantes valores distintos entre si.
#agrupamiento por regla de Sturges
k<- ceiling(1+ 3.322 * log10(nrow(datos)))

min_val <- floor(min(datos[[variable_continua]], na.rm = TRUE))


max_val <- ceiling(max(datos[[variable_continua]], na.rm = TRUE))

amplitud <- ceiling((max_val - min_val) / k)
max_tope <- min_val + amplitud * k
cortes <- seq(min_val, max_tope, by = amplitud)

#crear columnas con intervalos 
#Se crean las clases

datos$clases <- cut(datos[[variable_continua]], breaks = cortes, right = FALSE, include.lowest = TRUE )

#MARCA DE CLASE 

marca_clase <- (head(cortes, - 1)  + tail(cortes, - 1)) / 2 #marca de clase de cada intervalo

#tabla de frecuencias

tabla_clases <- table(datos$clases)
f_acum <- cumsum(tabla_clases)
f_rel <- prop.table(tabla_clases)
f_rel_acum <- cumsum(f_rel)


#mostrar tablas de frecuencias



tabla_frecuencia <- data.frame (
  
  Intervalo = names(tabla_clases),
  Marca = as.vector(marca_clase),
  Frec_abs = as.vector(tabla_clases),
  Frec_acum = as.vector(f_acum),
  Frec_rel = round(as.vector(f_rel),4),
  Frec_rel_acum = round(as.vector(f_rel_acum),4)
  
  
  
  
  
  
  
)






tabla_frecuencia

#Se calcula media y la media, ya que consideramos que la moda no es necesario en este caso, porque como son intervalos y no valores individuales, no aplica.



# Calculo de la media

frecuencias <- as.vector(tabla_clases)
media_continua <- sum(marca_clase * frecuencias) / sum(frecuencias)





#calculo de la mediana


n_total<- sum(frecuencias)

n_2<- n_total / 2

clase_mediana_index <- which(f_acum >= n_2)[1]

L <- cortes[clase_mediana_index]

F_anterior <- ifelse(clase_mediana_index == 1, 0, f_acum[clase_mediana_index - 1] )

f_mediana <- frecuencias[clase_mediana_index]

mediana_continua <- L + ((n_2 - F_anterior) / f_mediana) * amplitud


#Es la dispersion de valores con respecto a la media

#medidas de dispersion


varianza_continua <- sum(frecuencias * (marca_clase - media_continua)^2) / (n_total - 1)

desvio_continua <- sqrt(varianza_continua)



continua_stats <- data.frame(
  
  Media = round(media_continua, 4),

  Mediana = round(mediana_continua, 4),
  Varianza = round(varianza_continua, 4),
  Desvio_estandar = round(desvio_continua, 4)

  
  
  
)




continua_stats




#GRAFICO



# 4.A HISTOGRAMA
# --------------------------------------------
# Agrupa datos numéricos en intervalos (bins) y muestra su frecuencia.

# R BASE
hist(datos$Mean,
     breaks = 8,               # número de intervalos
     col = "skyblue",           # color de las barras
     main = "Histograma de promedio de aumento de temperatura global a traves de los años", # título del gráfico
     xlab = "Tiempo",                # etiqueta eje x
     freq = TRUE)               # TRUE: muestra frecuencias absolutas en c/barra








