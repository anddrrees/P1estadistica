

rm(list = ls(all = TRUE))

# Comprobamos en qué carpeta está R situado.
getwd()

# Cambiamos a la carpeta deseada (reemplazar "/.../..." por la ruta correcta).

setwd("R:/Downloads/Estadistica/Practica 1")
  #1
# Comprobamos qué archivos están disponibles en la carpeta.
dir()

# Cargamos datos desde un archivo de texto "games.txt" con encabezados y separadores específicos.
empresa <- read.table("datos_grupo_9.txt", header = TRUE, sep = "\t", dec = ".")
  #2
empresa$Tipo.de.proyecto <- as.factor(empresa$Tipo.de.proyecto)
Tipo.de.proyecto <- as.factor(empresa$Tipo.de.proyecto)
dim(empresa)
str(empresa)
summary(empresa)
names(empresa)

  #3
 #tabla frecuencia
tabla.NP <- table(empresa$Número.de.programadores)
tabla.NP # frecuencias absolutas
prop.table(tabla.NP) # frecuencias relativas

tabla.TP <- table(empresa$Tipo.de.proyecto)
tabla.TP # frecuencias absolutas
prop.table(tabla.TP) # frecuencias relativas


# Definimos el número de intervalos 'k' y calculamos la amplitud 'd' y los límites de los intervalos.
tiempo= empresa$Tiempo

k <- 7
d <- (max(tiempo) - min(tiempo)) / k
limites <- min(tiempo) + c(0:k) * d

# Creamos intervalos para la variable 'horas'.
cortes <- cut(tiempo, breaks = limites, include.lowest = TRUE)

# Creamos una tabla de frecuencias para los intervalos.
tabla.Tiempo <- table(cortes)
tabla.Tiempo
prop.table(tabla.Tiempo) # frecuencias relativas 
cumsum(tabla.Tiempo) # frecuencias absolutas acumuladas
cumsum(prop.table(tabla.Tiempo)) # frecuencias relativas acumuladas

# Definimos el número de intervalos 'k' y calculamos la amplitud 'd' y los límites de los intervalos.ç
Ingresos= empresa$Ingresos

k2 <- 7
d2 <- (max(Ingresos) - min(Ingresos)) / k2
limites2 <- min(Ingresos) + c(0:k2) * d2

# Creamos intervalos para la variable 'horas'.
cortes2 <- cut(Ingresos, breaks = limites2, include.lowest = TRUE)

# Creamos una tabla de frecuencias para los intervalos.
tabla.Ingresos <- table(cortes2)
tabla.Ingresos
prop.table(tabla.Ingresos) # frecuencias relativas 
cumsum(tabla.Ingresos) # frecuencias absolutas acumuladas

cumsum(prop.table(tabla.Ingresos)) # frecuencias relativas acumuladas
 #graficos
barplot(tabla.NP, xlab = "Número.de.programadores", ylab = "Frecuencias Absolutas", main = "Diagrama de Barras")
pie(tabla.NP, main = "Diagrama de Sectores de la variable Número.de.programadores")

barplot(tabla.TP, xlab = "Tipo.de.proyecto", ylab = "Frecuencias Absolutas", main = "Diagrama de Barras")
pie(tabla.TP, main = "Diagrama de Sectores de la variable Tipo.de.proyecto")

 #histograma tabla.Tiempo

hist(tiempo, breaks = limites, freq = TRUE)
hist(tiempo, breaks = limites, freq = FALSE)

hist(Ingresos, breaks = limites2, freq = TRUE)
hist(Ingresos, breaks = limites2, freq = FALSE)
 #boxplot
boxplot(empresa$Ingresos, main = "Boxplot: Ingresos")
boxplot(empresa$Tiempo, main = "Boxplot: Tiempo")
boxplot(empresa$Número.de.programadores, main = "Boxplot: Número.de.programadores")
  #medidas descriptivas
 #media
mean(tiempo)
mean(Ingresos)
mean(empresa$Número.de.programadores)
 #desviacion tipica
sd(tiempo)
sd(Ingresos)
sd(empresa$Número.de.programadores)
 #varianza
var(tiempo)
var(Ingresos)
var(empresa$Número.de.programadores)
 #cuantiles
quantile(tiempo, probs = c(0.25, 0.75))
quantile(Ingresos, probs = c(0.25, 0.75))
quantile(empresa$Número.de.programadores, probs = c(0.25, 0.75))
 # todos

summary(tiempo)
summary(Ingresos)
summary(empresa$Número.de.programadores)
  #4 analisis variable tiempo


  #5
 


  #6 Relación bivariante entre 'tiempo' y 'Ingresos'.
 # Creación de histogramas para 'tiempo' y 'Ingresos'.
par(mfrow = c(1, 2))
hist(tiempo)
hist(Ingresos)
par(mfrow = c(1, 1))
 # Medidas descriptivas para 'tiempo' y 'Ingresos'.
TieIng = data.frame(tiempo,Ingresos)
summary(TieIng)
 # grafica  para 'tiempo' y 'Ingresos'.
plot(tiempo,Ingresos)
cor(tiempo, Ingresos)
reg <- lm(Ingresos ~ tiempo)
summary(reg)
plot(tiempo, Ingresos, xlab = "Tiempo", ylab = "Ingresos", col = "blue")
abline(reg, col = "blue")


  #7
 # Medidas descriptivas para 'Tipo de proyecto' , 'tiempo' y 'Ingresos'.
Tipo.de.proyecto <-empresa$Tipo.de.proyecto
data = data.frame(Tipo.de.proyecto,tiempo,Ingresos)
summary(data)
 # Medidas descriptivas dividida en  'Tipo de proyecto' para 'tiempo' y 'Ingresos'.
# Filtramos datos .
listaC <- data[data$Tipo.de.proyecto == "Comercio Eléctrónico", ]
listaP <- data[data$Tipo.de.proyecto == "Página web", ]
# Resumen estadístico .
summary(listaC)
summary(listaP)
 # grafica dividida en  'Tipo de proyecto' para 'tiempo' y 'Ingresos'.
# Gráfico de dispersión .
plot(listaC$tiempo, listaC$Ingresos, xlab = "tiempo", ylab = "Ingresos", col = "blue")
points(listaP$tiempo, listaP$Ingresos, col = "red")
# Calculamos la correlación .
cor(listaC$tiempo, listaC$Ingresos)
cor(listaP$tiempo, listaP$Ingresos)

# Realizamos una regresión lineal.
reg.C <- lm(Ingresos ~ tiempo, data = listaC)
summary(reg.c)
reg.p <- lm(Ingresos ~ tiempo, data = listaP)
summary(reg.p)
# Gráfico de dispersión con líneas de regresión 
plot(listaC$tiempo, listaC$Ingresos, xlab = "tiempo", ylab = "ingreso", col = "blue")
points(listaP$tiempo, listaP$Ingresos, col = "red")
abline(reg.C, col = "blue")
abline(reg.p, col = "red")



