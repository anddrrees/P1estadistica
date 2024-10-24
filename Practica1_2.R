#Antes de todo limpiamos el entorno de trabajo
rm(list = ls(all = TRUE))

#Comprobamos el directorio de trabajo.
getwd()

#Cambiamos el directorio al que querramos.
setwd("R:/Desktop/estadistica/PRACTICA1/practica1")

#Comprobamos que esté el documento de texto necesario para la práctic ("datos_grupo_9.txt").
dir(pattern=".txt")

#APARTADO 1: LEER LOS DATOS
#Creamos una tabla con los datos de dicho documento.
empresa <- read.table("datos_grupo_9.txt", header = TRUE, sep = "\t", dec = ".")

#APARTADO 2: EXPLORAR LOS DATOS
#Analizamos la tabla creada
dim(empresa) #Para saber cuantas filas y columnas hay
str(empresa) #Vista general de la tabla
#Cambiamos el tipo de "Tipo de proyecto" de caracter a factores
empresa$Tipo.de.proyecto <- as.factor(empresa$Tipo.de.proyecto)
summary(empresa) #Resumimos la tabla
names(empresa) #Analizamos los nombres de las columnas de la tabla.
#Cambiamos el tipo de "Tipo de proyecto" de nuevo a caracter
empresa$Tipo.de.proyecto <- as.character(empresa$Tipo.de.proyecto)

#APARTADO 3: ANÁLISIS DESCRIPTIVO UNIVARIANTE
#Creamos una tabla para cada tipo de variable
#Para la variable "Tipo de Proyecto"
tabla.TProyecto <- table(empresa$Tipo.de.proyecto)
tabla.TProyecto
pie(tabla.TProyecto, main = "Número de Tipo de Proyecto",col =c("lightblue","lightgreen")) 

#Para la variable "Numero de Programadores"
tabla.NProgramadores <- table(empresa$Número.de.programadores)
tabla.NProgramadores
barplot(height=c(tabla.NProgramadores),hor = FALSE,ylab = "FRECUENCIAS ABSOLUTAS",xlab = "NÚMERO DE PROGRAMADORES", main = "NÚMERO DE PROGRAMADORES",col ="lightpink")
#Medidas descriptivas para la variable "Número de Programadores"
mean(empresa$Número.de.programadores) 
sd(empresa$Número.de.programadores)
var(empresa$Número.de.programadores)
quantile(empresa$Número.de.programadores)
summary(empresa$Número.de.programadores)

#Para la variable "Tiempo"
# Definimos el número de intervalos 'a' y calculamos la amplitud 'b' y los límites de los intervalos.
a <- 5
b <- (max(empresa$Tiempo) - min(empresa$Tiempo)) / a
limitesTiempo <- min(empresa$Tiempo) + c(0:a) * b
# Creamos intervalos para la variable 'horas'.
intervalosTiempo <- cut(empresa$Tiempo, breaks = limitesTiempo, include.lowest = TRUE)
tabla.Tiempo <- table(intervalosTiempo)
tabla.Tiempo
barplot(tabla.Tiempo,col ="lightgreen",main="SEMANAS DEDICADAS A PROYECTOS",xlab="INTERVALOS DE TIEMPO EN SEMANAS",ylab="FRECUENCIAS ABSOLUTAS")
#Medidas descriptivas para la variable "Tiempo"
mean(empresa$Tiempo)
sd(empresa$Tiempo)
var(empresa$Tiempo)
quantile(empresa$Tiempo)
summary(empresa$Tiempo)

#Para la variable "Ingresos"
# Definimos el número de intervalos 'c' y calculamos la amplitud 'd' y los límites de los intervalos.
c <- 7
d <- (max(empresa$Ingresos/1000) - min(empresa$Ingresos/1000)) / c
limitesIngresos <- min(empresa$Ingresos/1000) + c(0:c) * d
# Creamos intervalos para la variable 'horas'.
intervalosIngresos <- cut(empresa$Ingresos/1000, breaks = limitesIngresos, include.lowest = TRUE)
tabla.Ingresos <- table(intervalosIngresos)
tabla.Ingresos
barplot(tabla.Ingresos,col ="purple",main="INGRESOS",xlab="INTERVALOS DE INGRESOS EN MILES DE EUROS",ylab="FRECUENCIAS ABSOLUTAS")
#Medidas descriptivas para la variable "Ingresos"
mean(empresa$Ingresos)
sd(empresa$Ingresos)
var(empresa$Ingresos)
quantile(empresa$Ingresos)
summary(empresa$Ingresos)

#APARTADO 4: ANÁLISIS DE LA VARIABLE TIEMPO
#COMPARACION TIEMPO / TIPO DE PROYECTO
tabla.TiempoProyecto <- table(empresa$Tipo.de.proyecto,intervalosTiempo)
tabla.TiempoProyecto
prop.table(tabla.TiempoProyecto)
barplot(ylab="FRECUENCIAS ABSOLUTAS",main="SEMANAS POR TIPO DE PROYECTO",beside=TRUE,hor=FALSE,tabla.TiempoProyecto,xlab=c("INTERVALOS DE SEMANAS"),space=c(0.3,1),col = c("red","blue"),legend.text =c("Com Electrónico.","Pág. Web"))

#COMPARACION TIEMPO / NÚMERO DE PROGRAMADORES
tabla.TiempoNProgramadores <- table(empresa$Número.de.programadores,intervalosTiempo)
tabla.TiempoNProgramadores
barplot(ylab="FRECUENCIAS ABSOLUTAS",main="SEMANAS POR NÚMERO DE PROGRAMADORES",hor=FALSE,tabla.TiempoNProgramadores,width=c(1),height=c(tabla.TiempoNProgramadores),beside=TRUE,xlab=c("INTERVALOS DE SEMANAS"),space=c(2.5,0.3,0.3,0.3,0.3),col = c("purple","lightblue","lightgreen","lightyellow","brown"),legend.text =c("3 Programadores","4 Programadores","5 Programadores","6 Programadores","7 Programadores"))
legend(x=4,y=27,legend="[232,333]")
legend(x=12,y=27,legend="(333,433]")
legend(x=21,y=27,legend="(433,533]")
legend(x=30,y=27,legend="(533,633]")
legend(x=40,y=27,legend="(633,733]")
plot(empresa$Tiempo,empresa$Número.de.programadores,main="Diagrama de Dispersión de Tiempo y Número de Programadores") #A mayor tiempo mayor numero de programadores
cor(empresa$Número.de.programadores,empresa$Tiempo) #Correlacion entre Número de Programadores y Tiempo
reg.A <- lm(empresa$Número.de.programadores ~ empresa$Tiempo, data = empresa)
summary(reg.A)
abline(reg.A,col="blue")


#APARTADO 5: ANÁLISIS DE LA VARIABLE INGRESOS
#COMPARACION INGRESOS / TIPO DE PROYECTO
tabla.IngresosProyecto <- table(empresa$Tipo.de.proyecto,intervalosIngresos)
tabla.IngresosProyecto
prop.table(tabla.IngresosProyecto)
barplot(ylab="FRECUENCIAS ABSOLUTAS",main="INGRESOS POR TIPO DE PROYECTO",beside=TRUE,hor=FALSE,tabla.IngresosProyecto,xlab=c("INTERVALOS DE INGRESOS"),space=c(0.3,1),col = c("pink","purple"),legend.text =c("Com Electrónico.","Pág. Web"))

#COMPARACION TIEMPO / NÚMERO DE PROGRAMADORES
tabla.IngresosNProgramadores <- table(empresa$Número.de.programadores,intervalosIngresos)
tabla.IngresosNProgramadores
barplot(ylab="FRECUENCIAS ABSOLUTAS",main="INGRESOS POR NÚMERO DE PROGRAMADORES",hor=FALSE,tabla.IngresosNProgramadores,width=c(1),height=c(tabla.IngresosNProgramadores),beside=TRUE,xlab=c("INTERVALOS DE INGRESOS"),space=c(2.5,0.3,0.3,0.3,0.3),col = c("purple","lightblue","lightgreen","lightyellow","brown"),legend.text =c("3 Programadores","4 Programadores","5 Programadores","6 Programadores","7 Programadores"))
plot(empresa$Ingresos,empresa$Número.de.programadores,main="Diagrama de Dispersión de Ingresos y Número de Programadores") #Tienen una correlacion menor pero aun positiva.
cor(empresa$Número.de.programadores,empresa$Ingresos) #Correlacion entre Número de Programadores y Tiempo
reg.B <- lm(empresa$Número.de.programadores ~ empresa$Ingresos, data = empresa)
summary(reg.B)
abline(reg.B,col="red")

