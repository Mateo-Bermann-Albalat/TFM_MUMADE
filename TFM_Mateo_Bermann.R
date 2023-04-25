# En primer lugar, cargamos los paquetes y los datos necesarios
library(ggplot2)
library(tidyverse)
library(stargazer)
library(scales)
library(ggtext)
library(rpart)
library(rpart.plot)
library(neuralnet)
library(forecast)

data = read.csv("C:/Users/mberm/OneDrive/Escritorio/TFM_MUMADE_Local/dump.csv")
# Ya que vamos a crear más columnas a partir de la información que tenemos, lo insertamos directamente en el dataframe
data["Nivel"] = c(rep("Otro", 39537))
data["Fase"]  = c(rep("Intermedia", 39537))
data["Duracion"] = c(rep("Actualmente", 39537))
data["temp1"] = 2018 - as.numeric(substr(data$startDate, 0,4))
data["Age_estimate_inicio"] = data$ageEstimate - data$temp1
data["Ascenso"] = c(rep(0, 39537))
# Creamos vectores dummy que usaremos más tarde para generar las variables a estudiar. Estos vectores son simplemenete el identificador de usuario recortado una posici?n por arriba o por
# abajo, con lo que podemos entender si es encaja con la anterior / posterior posici?n 
v1 = data$memberUrn
v1 = v1[-1]
v1 = c(v1, "test")
v2 = data$companyUrn
v2 = v2[-1]
v2 = c(v2,"test")


# Rellenamos las columnas añadidas
data$Nivel = ifelse(grepl(pattern = "Engineer",
                          x = data$posTitle,
                          fixed = TRUE), "Técnico", data$Nivel)
data$Nivel = ifelse(grepl(pattern = "Developer",
                          x = data$posTitle,
                          fixed = TRUE), "Técnico", data$Nivel)
data$Nivel = ifelse(grepl(pattern = "Engineer",
                          x = data$posTitle,
                          fixed = TRUE), "Técnico", data$Nivel)
data$Nivel = ifelse(grepl(pattern = "Programmer",
                          x = data$posTitle,
                          fixed = TRUE), "Técnico", data$Nivel)
data$Nivel = ifelse(grepl(pattern = "CEO",
                          x = data$posTitle,
                          fixed = TRUE), "Director", data$Nivel)
data$Nivel = ifelse(grepl(pattern = "Director",
                          x = data$posTitle,
                          fixed = TRUE), "Director", data$Nivel)
data$Nivel = ifelse(grepl(pattern = "President",
                          x = data$posTitle,
                          fixed = TRUE), "Director", data$Nivel)
data$Nivel = ifelse(grepl(pattern = "Manager",
                          x = data$posTitle,
                          fixed = TRUE), "Mánager", data$Nivel)
data$Nivel = ifelse(grepl(pattern = "Lead",
                          x = data$posTitle,
                          fixed = TRUE), "Mánager", data$Nivel)
data$Nivel = ifelse(grepl(pattern = "Head",
                          x = data$posTitle,
                          fixed = TRUE), "Mánager", data$Nivel)
data$Nivel = ifelse(grepl(pattern = "Executive",
                          x = data$posTitle,
                          fixed = TRUE), "Ejecutivo", data$Nivel)
data$Nivel = ifelse(grepl(pattern = "Junior",
                          x = data$posTitle,
                          fixed = TRUE), "Junior", data$Nivel)
data$Nivel = ifelse(grepl(pattern = "Senior",
                          x = data$posTitle,
                          fixed = TRUE), "Senior", data$Nivel)
data$Nivel = ifelse(grepl(pattern = "Lecturer",
                          x = data$posTitle,
                          fixed = TRUE), "Profesor", data$Nivel)
data$Nivel = ifelse(grepl(pattern = "Professor",
                          x = data$posTitle,
                          fixed = TRUE), "Profesor", data$Nivel)
data$Nivel = ifelse(grepl(pattern = "Intern",
                          x = data$posTitle,
                          fixed = TRUE), "Prácticas", data$Nivel)
data$Fase = ifelse(data$memberUrn != v1, "Última", data$Fase)
data$Ascenso = ifelse(data$companyUrn != v2, 1, 0)
v3 = data$Fase
v3 = v3[-39537]
v3 = c("test", v3)
data$Fase = ifelse(v3 == "Última", "Primera", data$Fase)
data$Fase[1] = "Primera"
tabla_temp_inicio_laboral = data %>%  select(20,25) 
colnames(tabla_temp_inicio_laboral)[2] = "fecha"
tabla_temp_inicio_laboral$fecha = as.Date(tabla_temp_inicio_laboral$fecha)
tabla_temp_inicio_laboral = tabla_temp_inicio_laboral %>% group_by(memberUrn) %>% summarise(fecha = min(fecha))


# Ahora vamos a generar una tabla m?s grande, y ya tenemos las coincidencias que necesit?bamos
# as? que procedemos a eliminar los datos vac?os. Si hubi?semos dado este paso al principio, tendr?amos informaci?n incompleta, ya que habr?amos eliminado posibles
# coincidencias. Es un paso necesario para quitarnos de encima informaci?n incompleta.
data = data %>%  drop_na()
data = left_join(data, tabla_temp_inicio_laboral, by="memberUrn")
data = data %>%  drop_na()
# Factorizamos  el nivel
data$Nivel = factor( x = data$Nivel,
                     levels = c("Prácticas",
                                "Otro",
                                "Junior",
                                "Técnico",
                                "Profesor",
                                "Senior",
                                "Mánager",
                                "Ejecutivo",
                                "Director"),
                     labels = c("Prácticas",
                                "Otro",
                                "Junior",
                                "Técnico",
                                "Profesor",
                                "Senior",
                                "Mánager",
                                "Ejecutivo",
                                "Director"),
                     ordered = FALSE)
data$Fase = factor( x = data$Fase,
                    levels = c("Primera",
                               "Intermedia",
                               "Última"),
                    labels = c("Primera",
                               "Intermedia",
                               "Última"))
# Cambiamos el formato de las fechas para poder trabajar con ellas
data$endDate = as.Date(data$endDate)
data$startDate = as.Date(data$startDate)
data$fecha = as.Date(data$fecha)
data$Experiencia = data$startDate - data$fecha
data$Experiencia = as.numeric(data$Experiencia)
data$Duracion = data$endDate - data$startDate
data$Duracion = as.numeric(data$Duracion)
# Hacemos una última limpieza para asegurarnos de que si hay algún vacío en el cálculo de las columnas no aparece en los datos que usamos
finalizados_trabajos = data %>% drop_na()
# Vamos a visualizar los datos en dos dimensiones para hacer una rápida comprobación visual sobre si la relación es lineal o polinómica
texto_grafico_duracion = paste("Media : " , mean(finalizados_trabajos$Duracion), " Desviación tópica : " , sd(finalizados_trabajos$Duracion))

Grafico_final_duracion = ggplot(data = finalizados_trabajos, aes(x = Duracion)) +
  geom_histogram(col = "black", fill = "green", bins = 50) +
  labs(title = "Distribución de la duración", x = "Duración de la posición, en días" , y = "Frecuencia") +
  geom_label(aes( x = 10000, y = 50000, label = texto_grafico_duracion))

ggsave("Grafico_final_duracion.png", plot = Grafico_final_duracion, device = "png")

Grafico_final_nivel = ggplot(data = finalizados_trabajos, aes(x = Nivel)) +
  geom_bar(fill="green", col = "black")+
  theme_gray()+
  labs(title = "Distribución  del nivel \n de los trabajadores", y = "Frecuencia", x = "Nivel")+
  theme(axis.text = element_text(angle = 90))

ggsave("Grafico_final_nivel.png", plot = Grafico_final_nivel, device = "png")

texto_grafico_experiencia = paste("Media : " , mean(finalizados_trabajos$Experiencia), " 
                                  Desviación típica : " , sd(finalizados_trabajos$Experiencia))

Grafico_final_experiencia = ggplot(data = finalizados_trabajos, aes(x = Experiencia)) +
  geom_histogram(col = "black", fill = "green", bins = 50) +
  labs(title = "Distribución de la experiencia", x = "Duración de la experiencia laboral, en días" , y = "Frecuencia") +
  geom_label(aes( x = 10000, y = 5000, label = texto_grafico_experiencia))

ggsave("Grafico_final_experiencia.png", plot = Grafico_final_experiencia, device = "png", height = 7, width = 7)


texto_grafico_ageestimateinicio = paste("Media : " , mean(finalizados_trabajos$Age_estimate_inicio), " Desviación típica : " , sd(finalizados_trabajos$Age_estimate_inicio))

Grafico_final_ageestimateinicio1 = ggplot(data = finalizados_trabajos, aes(x = Age_estimate_inicio)) +
  geom_histogram(col = "black", fill = "green", bins = 50) +
  xlim(-20,15)+
  ylim(0,500)+
  labs(title = "Distribución de la estimación de la edad al iniciar el primer trabajo", x = "Estimación de la edad al iniciar el primer trabajo, en años" , y = "Frecuencia") +
  geom_label(aes( x = 10000, y = 50000, label = texto_grafico_ageestimateinicio))


ggsave("Grafico_final_edad_inicial1.png", plot = Grafico_final_ageestimateinicio1, device = "png", width = 8, height = 5)


Grafico_final_ageestimateinicio2 = ggplot(data = finalizados_trabajos, aes(x = Age_estimate_inicio)) +
  geom_histogram(col = "black", fill = "green", bins = 50) +
  xlim(-20,150)+
  ylim(0,10000)+
  labs(title = "Distribución de la estimación de la edad al iniciar el trabajo", x = "Estimación de la edad al iniciar el primer trabajo, en años" , y = "Frecuencia") +
  geom_label(aes( x = 100, y = 7500, label = texto_grafico_ageestimateinicio))

ggsave("Grafico_final_edad_inicial2.png", plot = Grafico_final_ageestimateinicio2, device = "png", width = 8, height = 7)

texto_grafico_ageestimate = paste("Media : " , mean(finalizados_trabajos$ageEstimate), " Desviación típica : " , sd(finalizados_trabajos$ageEstimate))

Grafico_final_ageestimate = ggplot(data = finalizados_trabajos, aes(x = ageEstimate)) +
  geom_histogram(col = "black", fill = "green", bins = 50) +
  labs(title = "Distribución de la estimación de la edad al   iniciar el  trabajo", x = "Estimación de la edad al iniciar el primer trabajo, en años" , y = "Frecuencia") +
  geom_label(aes( x = 70, y = 1500, label = texto_grafico_ageestimate))


ggsave("Grafico_final_edad_aproximada.png", plot = Grafico_final_ageestimate, device = "png")

Grafico_final_fechainicio = ggplot(data = finalizados_trabajos, aes(x = startDate, y = Duracion)) +
  geom_point(col = "green") +
  labs(title = "Distribución de la fecha al iniciar el  trabajo", x = "Fecha al iniciar el trabajo, en años" , y = "Duración del puesto de trabajo, en días") 

ggsave("Grafico_final_fecha_inicio.png", plot = Grafico_final_fechainicio, device = "png")

Grafico_final_fechafinal = ggplot(data = finalizados_trabajos, aes(x = endDate, y = Duracion)) +
  geom_point(col = "green") +
  labs(title = "Distribución de la fecha al finalizar el  trabajo", x = "Fecha al acabar el trabajo, en años" , y = "Duración del puesto de trabajo, en días") 


ggsave("Grafico_final_fecha_final.png", plot = Grafico_final_fechafinal, device = "png")

Grafico_final_genero = ggplot(data = finalizados_trabajos, aes(x = genderEstimate)) +
  geom_bar(fill="green", col = "black")+
  theme_gray()+
  labs(title = "Distribución del género", x = "Género", y = "Frecuencia")

ggsave("Grafico_final_género.png", plot = Grafico_final_genero, device = "png")

Grafico_final_ascenso = ggplot(data = finalizados_trabajos, aes(x = Ascenso)) +
  geom_bar(fill="green", col = "black")+
  theme_gray()+
  labs(title = "Distribución del Ascenso", x = "Ascenso", y = "Frecuencia")

ggsave("Grafico_final_ascenso.png", plot = Grafico_final_ascenso, device = "png", height = 8, width = 5)

texto_grafico_mediaempresa = paste("Media : " , mean(finalizados_trabajos$avgCompanyPosDuration), " 
Desviación típica : " , sd(finalizados_trabajos$avgCompanyPosDuration))

Grafico_final_mediaempresa = ggplot(data = finalizados_trabajos, aes(x = avgCompanyPosDuration)) +
  geom_histogram(col = "black", fill = "green") +
  labs(title = "Distribución de la media empresarial", x = "Duración media" , y = "Frecuencia") +
  geom_label(aes( x = 2000, y = 5000, label = texto_grafico_mediaempresa)) +
  theme_gray()

ggsave("Grafico_final_mediaempresa.png", plot = Grafico_final_mediaempresa, device = "png")

# Seleccionamos subconjuntos para poder evaluar mejor nuestros modelos

sample <- sample(c(TRUE, FALSE), nrow(finalizados_trabajos), replace=TRUE, prob=c(0.8,0.2))
train  <- finalizados_trabajos[sample, ]
test   <- finalizados_trabajos[!sample, ]

# Y creamos y visualizamos los modelos lineal y logar?tmico

m1_completo = lm(Duracion ~ Experiencia + companyStaffCount + followersCount + genderEstimate + Nivel + avgMemberPosDuration + avgCompanyPosDuration , data = train)
stargazer(m1_completo, type = "text")
m2_completo = lm(Ascenso ~ Experiencia + companyStaffCount + followersCount + genderEstimate + Nivel + avgMemberPosDuration + avgCompanyPosDuration , data = train)
stargazer(m2_completo, type = "text")
# Creamos y visualizamos el árbol de regresi?n
arbol_regresion = rpart(Duracion ~ Experiencia + companyStaffCount + followersCount + genderEstimate + avgMemberPosDuration + avgCompanyPosDuration + Nivel, data=train, method="anova",)
plot(arbol_regresion)
text(arbol_regresion, use.n=TRUE, all=TRUE, cex=.6)
arbol_decision = rpart(Ascenso ~ Experiencia + Nivel + Fase + avgCompanyPosDuration + genderEstimate + companyStaffCount + avgMemberPosDuration, data=finalizados_trabajos)
plot(arbol_decision, col = "red")
text(arbol_decision, use.n=TRUE, all=TRUE, cex=.6)
prp(arbol_decision,faclen=0, extra=1)

arbol_decision2 = rpart(Ascenso ~ Experiencia + genderEstimate + avgMemberPosDuration, data=finalizados_trabajos)
prp(arbol_decision2, faclen = 0, extra = 1)
# Y la red neuronal. Para ello, antes generamos una matriz con el modelo
m = model.matrix( ~ Duracion + Experiencia + Nivel + Fase + avgCompanyPosDuration + genderEstimate + companyStaffCount + avgMemberPosDuration, data = train)
m = as.data.frame(m)
normalized = function(x) (x- min(x))/(max(x) - min(x))
for (i in 2:18) {
  m[i] = lapply(m[i], normalized)
}
# Cambiamos los nombres de las columnas que tienen tildes ( soluciona un error de codificaci?n)
colnames(m)[5] = "NivelJun"
colnames(m)[6] = "NivelTec"
colnames(m)[7] = "NivelPro"
colnames(m)[8] = "NivelSen"
colnames(m)[9] = "NivelMan"
colnames(m)[10] = "NivelEje"
colnames(m)[11] = "NivelDir"
# Primero, creamos una red neuronal que nos sirve de base. Es por ello que no tenemos par?metros, dejamos que R lo calcule por nosotros
net_duracion_baseline = neuralnet(Duracion ~ Experiencia + NivelOtro + NivelJun + NivelTec + NivelPro + NivelSen + NivelMan + NivelEje + NivelDir + avgCompanyPosDuration + genderEstimatefemale + genderEstimatemale + companyStaffCount + avgMemberPosDuration , data = m)
plot(net_duracion_baseline, rep = "best")
# Creamos una segunda red neuronal en la que alteramos los par?metros para reducir el error
net_duracion_v2 = neuralnet(Duracion ~ Experiencia + NivelOtro + NivelJun + NivelTec + NivelPro + NivelSen + NivelMan + NivelEje + NivelDir + avgCompanyPosDuration + genderEstimatefemale + genderEstimatemale + companyStaffCount + avgMemberPosDuration , data = m, hidden = 20, threshold=0.04, stepmax = 1e7)
plot(net_duracion_v2, rep = "best")
net_duracion_v3 = neuralnet(Duracion ~ Experiencia + NivelOtro + NivelJun + NivelTec + NivelPro + NivelSen + NivelMan + NivelEje + NivelDir + avgCompanyPosDuration + genderEstimatefemale + genderEstimatemale + companyStaffCount + avgMemberPosDuration , data = m, hidden = 30, threshold=0.03, linear.output=FALSE, stepmax = 1e7)
plot(net_duracion_v3, rep = "best")
net_duracion_v4 = neuralnet(Duracion ~ Experiencia + NivelOtro + NivelJun + NivelTec + NivelPro + NivelSen + NivelMan + NivelEje + NivelDir + avgCompanyPosDuration + genderEstimatefemale + genderEstimatemale + companyStaffCount + avgMemberPosDuration , data = m, hidden = 70, threshold=0.03, linear.output=FALSE, stepmax = 1e7)
plot(net_duracion_v4, rep = "best")
net_duracion_v5 = neuralnet(Duracion ~ Experiencia + NivelOtro + NivelJun + NivelTec + NivelPro + NivelSen + NivelMan + NivelEje + NivelDir + avgCompanyPosDuration + genderEstimatefemale + genderEstimatemale + companyStaffCount + avgMemberPosDuration , data = m, hidden = 45, threshold=0.02, linear.output=FALSE, stepmax = 1e7)
plot(net_duracion_v5, rep = "best")
net_duracion_v6 = neuralnet(Duracion ~ Experiencia + NivelOtro + NivelJun + NivelTec + NivelPro + NivelSen + NivelMan + NivelEje + NivelDir + avgCompanyPosDuration + genderEstimatefemale + genderEstimatemale + companyStaffCount + avgMemberPosDuration , data = m, hidden = c(20,20,15), threshold=0.02, linear.output=FALSE, stepmax = 1e7)
plot(net_duracion_v6, rep = "best")
net_duracion_v7 = neuralnet(Duracion ~ Experiencia + NivelOtro + NivelJun + NivelTec + NivelPro + NivelSen + NivelMan + NivelEje + NivelDir + avgCompanyPosDuration + genderEstimatefemale + genderEstimatemale + companyStaffCount + avgMemberPosDuration , data = m, hidden = c(10,5,5), threshold=0.08, linear.output=FALSE, stepmax = 1e7)
plot(net_duracion_v7, rep = "best")
net_duracion_v8 = neuralnet(Duracion ~ Experiencia + NivelOtro + NivelJun + NivelTec + NivelPro + NivelSen + NivelMan + NivelEje + NivelDir + avgCompanyPosDuration + genderEstimatefemale + genderEstimatemale + companyStaffCount + avgMemberPosDuration , data = m, hidden = 30, threshold=0.1, linear.output=FALSE, stepmax = 1e7)
plot(net_duracion_v8, rep = "best")
net_duracion_v9 = neuralnet(Duracion ~ Experiencia + NivelOtro + NivelJun + NivelTec + NivelPro + NivelSen + NivelMan + NivelEje + NivelDir + avgCompanyPosDuration + genderEstimatefemale + genderEstimatemale + companyStaffCount + avgMemberPosDuration , data = m, hidden = 30, threshold=0.01, linear.output=FALSE, stepmax = 1e7)
plot(net_duracion_v9, rep = "best")
net_duracion_v10 = neuralnet(Duracion ~ Experiencia + NivelOtro + NivelJun + NivelTec + NivelPro + NivelSen + NivelMan + NivelEje + NivelDir + avgCompanyPosDuration + genderEstimatefemale + genderEstimatemale + companyStaffCount + avgMemberPosDuration , data = m, hidden = c(15,15), threshold=0.02, linear.output=FALSE, stepmax = 1e7)
plot(net_duracion_v10, rep = "best")
# Evaluamos

n = model.matrix( ~ Duracion + Experiencia + Nivel + Fase + avgCompanyPosDuration + genderEstimate + companyStaffCount + avgMemberPosDuration, data = test)
n = as.data.frame(n)
colnames(n)[5] = "NivelJun"
colnames(n)[6] = "NivelTec"
colnames(n)[7] = "NivelPro"
colnames(n)[8] = "NivelSen"
colnames(n)[9] = "NivelMan"
colnames(n)[10] = "NivelEje"
colnames(n)[11] = "NivelDir"
normalization_lduracion = max(n$Duracion)
for (i in 2:18) {
  n[i] = lapply(n[i], normalized)
}
test_no_Duracion = subset(test, select = -c(Duracion) )
pred_lineal = predict(m1_completo, test_no_Duracion)
pred_arbol = predict(arbol_regresion, test_no_Duracion)
pred_neuralnet_baseline = predict(net_duracion_baseline, n)
pred_neuralnet_baseline = pred_neuralnet_baseline * normalization_lduracion
pred_neuralnet_v2 = predict(net_duracion_v2, n)
pred_neuralnet_v2 = pred_neuralnet_v2 * normalization_lduracion
pred_neuralnet_v3 = predict(net_duracion_v3, n)
pred_neuralnet_v3 = pred_neuralnet_v3 * normalization_lduracion
pred_neuralnet_v4 = predict(net_duracion_v4, n)
pred_neuralnet_v4 = pred_neuralnet_v4 * normalization_lduracion
pred_neuralnet_v5 = predict(net_duracion_v5, n)
pred_neuralnet_v5 = pred_neuralnet_v5 * normalization_lduracion
pred_neuralnet_v6 = predict(net_duracion_v6, n)
pred_neuralnet_v6 = pred_neuralnet_v6 * normalization_lduracion
pred_neuralnet_v7 = predict(net_duracion_v7, n)
pred_neuralnet_v7 = pred_neuralnet_v7 * normalization_lduracion
pred_neuralnet_v8 = predict(net_duracion_v8, n)
pred_neuralnet_v8 = pred_neuralnet_v8 * normalization_lduracion
pred_neuralnet_v9 = predict(net_duracion_v9, n)
pred_neuralnet_v9 = pred_neuralnet_v9 * normalization_lduracion
pred_neuralnet_v10 = predict(net_duracion_v10, n)
pred_neuralnet_v10 = pred_neuralnet_v10 * normalization_lduracion

sum(abs(test$Duracion - pred_lineal)) / 23579
sum(abs(test$Duracion - pred_arbol)) / 23579
sum(abs(test$Duracion - pred_neuralnet_baseline)) / 23579
sum(abs(test$Duracion - pred_neuralnet_v2)) / 23579
sum(abs(test$Duracion - pred_neuralnet_v3)) / 23579
sum(abs(test$Duracion - pred_neuralnet_v4)) / 23579
sum(abs(test$Duracion - pred_neuralnet_v5)) / 23579
sum(abs(test$Duracion - pred_neuralnet_v6)) / 23579
sum(abs(test$Duracion - pred_neuralnet_v7)) / 23579
sum(abs(test$Duracion - pred_neuralnet_v8)) / 23579
sum(abs(test$Duracion - pred_neuralnet_v9)) / 23579
sum(abs(test$Duracion - pred_neuralnet_v10)) / 23579

sum((test$Duracion - pred_lineal) ** 2) / 23579
sum((test$Duracion - pred_arbol) ** 2) / 23579
sum((test$Duracion - pred_neuralnet_baseline) ** 2) / 23579
sum((test$Duracion - pred_neuralnet_v2) ** 2) / 23579
sum((test$Duracion - pred_neuralnet_v3) ** 2) / 23579
sum((test$Duracion - pred_neuralnet_v4) ** 2) / 23579
sum((test$Duracion - pred_neuralnet_v5) ** 2) / 23579
sum((test$Duracion - pred_neuralnet_v6) ** 2) / 23579
sum((test$Duracion - pred_neuralnet_v7) ** 2) / 23579
sum((test$Duracion - pred_neuralnet_v8) ** 2) / 23579
sum((test$Duracion - pred_neuralnet_v8) ** 2) / 23579
sum((test$Duracion - pred_neuralnet_v10) ** 2) / 23579


layers = (net_duracion_baseline$weights)
length(layers)
# Y para finalizar, guardamos los datos finales en un archivo CSV para poder trabajar mejor en ellos
write.csv(finalizados_trabajos,"C:/Users/mberm/Desktop/TFM/data_trimmed2.csv", row.names = TRUE)
                   
save.image(file = "nn_hasta_v3.RData")
