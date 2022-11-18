# En primer lugar, cargamos los paquetes y los datos necesarios
library(ggplot2)
library(tidyverse)
library(stargazer)
library(scales)
library(ggtext)
library(rpart)
data = read.csv("C:/Users/mberm/Desktop/TFM/dump.csv")
data = data %>%  drop_na()
# Ya que vamos a crear mÃ¡s columnas a partir de la informaciÃ³n que tenemos, lo insertamos directamente en el dataframe
data["Nivel"] = c(rep("Otro", 37235))
data["Fase"]  = c(rep("Intermedia", 37235))
data["Duracion"] = c(rep("Actualmente", 37235))
data["temp1"] = 2018 - as.numeric(substr(data$startDate, 0,4))
data["Age_estimate"] = data$ageEstimate - data$temp1
data["Ascenso"] = c(rep(0, 37235))
# Creamos vectores dummy que usaremos mÃ¡s tarde para generar las variables a estudiar
v1 = data$memberUrn
v1 = v1[-1]
v1 = c(v1, "test")
v2 = data$companyUrn
v2 = v2[-1]
v2 = c(v2, "test")
# Rellenamos las columnas aÃ±adidas
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
                          fixed = TRUE), "Práticas", data$Nivel)
data$Fase = ifelse(data$memberUrn != v1, "Ãšltima", data$Fase)
data$Ascenso = ifelse(data$companyUrn != v2, 1, 0)
v3 = data$Fase
v3 = v3[-37235]
v3 = c("test", v3)
data$Fase = ifelse(v3 == "Ãšltima", "Primera", data$Fase)
data$Fase[1] = "Primera"
tabla_temp_inicio_laboral = data %>%  select(20,25) 
colnames(tabla_temp_inicio_laboral)[2] = "fecha"
data = left_join(data, tabla_temp_inicio_laboral, by="memberUrn")
# Factorizamos
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
data$endDate = as.Date(data$endDate)
data$startDate = as.Date(data$startDate)
data$fecha = as.Date(data$fecha)
data$Experiencia = data$startDate - data$fecha
data$Experiencia = as.numeric(data$Experiencia)
data$Duracion = data$endDate - data$startDate
data$Duracion = as.numeric(data$Duracion)
# Filtramos para hacer exploraciÃ³n del crecimiento laboral. Al tener cada perfil Ãºnico un primer trabajo y algunos un Ãºltimo
primeros_trabajos = data %>% filter(data$Fase == "Primera")
ultimos_trabajos = data %>% filter(data$Fase  == "Última")
finalizados_trabajos = data %>% drop_na()
# Y vemos que a medida que sube la responsabilidad sube la edad, con lo que los datos encajan en la teorÃ­a
# Vamos a visualizar los datos en dos dimensiones para hacer una rÃ¡pida comprobaciÃ³n visual sobre si la relaciÃ³n es lineal o polinÃ³mica
Grafico_Age_estimate= ggplot(data = finalizados_trabajos, aes(x = Duracion, y = Age_estimate)) +
  geom_point(col="green") +
  theme_gray() +
  labs(title = "Años estimados al entrar al trabajo", y = "Edad", x = "Duración de la posición")
Grafico_Age_estimate
Grafico_numero_empleados = ggplot(data = finalizados_trabajos, aes(x = Duracion, y = companyStaffCount)) +
  geom_point(col="green", size = 0.5)+
  theme_gray()+
  labs(title = "Distribución del tamaño de las \n empresas en relación a la duración", y = "Número de empleados", x = " Duración de la posición")+
  scale_y_continuous(labels = comma)
Grafico_numero_empleados
Grafico_numero_conexiones = ggplot(data = finalizados_trabajos, aes(x = Duracion, y = connectionsCount)) +
  geom_point(col="green", size = 0.5)+
  theme_gray()+
  labs(title = "Distribución del número de  \n  contactos en relación a la duración", y = "Número de conexiones", x = " Duración de la posición")+
  scale_y_continuous(labels = comma)
Grafico_numero_conexiones
Grafico_fecha_finalizacion = ggplot(data = finalizados_trabajos, aes(x = Duracion, y = endDate)) +
  geom_point(col="green", size = 0.5)+
  theme_gray()+
  labs(title = "Distribución de la fecha  de  \n  finalización en relación a la duración", y = "Fecha de finalización", x = " Duración de la posición")
Grafico_fecha_finalizacion
Grafico_fecha_inicio = ggplot(data = finalizados_trabajos, aes(x = Duracion, y = startDate)) +
  geom_point(col="green", size = 0.5)+
  theme_gray()+
  labs(title = "Distribución de la fecha  de  \n  inicio en relación a la duración", y = "Fecha de inicio", x = " Duración de la posición")
Grafico_fecha_inicio
Grafico_numero_seguidores = ggplot(data = finalizados_trabajos, aes(x = Duracion, y = followersCount)) +
  geom_point(col="green", size = 0.5)+
  theme_gray()+
  labs(title = "Distribución del número de  \n  seguidores en relación a la duración", y = "Número de seguidores", x = " Duración de la posición")
Grafico_numero_seguidores
finalizados_trabajos = finalizados_trabajos %>% filter( finalizados_trabajos$Experiencia > -0.1)
Grafico_experiencia = ggplot(data = finalizados_trabajos, aes(x = Duracion, y = Experiencia)) +
  geom_point(col="green", size = 0.5)+
  theme_gray()+
  labs(title = "Relación de la experiencia \n con la duración ", y = "Experiencia", x = " Duración de la posición")+
  scale_y_continuous(labels = comma)
Grafico_experiencia
Grafico_nivel = ggplot(data = finalizados_trabajos, aes(x = Nivel)) +
  geom_bar(fill="green")+
  theme_gray()+
  labs(title = "Distribución  del nivel \n de los trabajadores", y = "Frecuencia", x = "Nivel")+
  theme(axis.text = element_text(angle = 90))
Grafico_nivel
Grafico_fase = ggplot(data = finalizados_trabajos, aes(x = Fase)) +
  geom_bar(fill="green")+
  theme_gray()+
  labs(title = "Distribución  de la fase", y = "Frecuencia", x = "Fase")+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
Grafico_fase
# Eliminamos atípicos 
data = data %>% filter( data$companyFollowerCount < 50000)
Grafico_genero = ggplot(data = finalizados_trabajos, aes(x = genderEstimate)) +
  geom_bar(fill="green")+
  theme_gray()+
  labs(title = "Distribución del género", x = "Género", y = "Frecuencia")
Grafico_genero
Grafico_media_individual= ggplot(data = finalizados_trabajos, aes(x = Duracion, y = avgMemberPosDuration)) +
  geom_point(col="green", size = 0.5)+
  theme_gray()+
  labs(title = "Distribución de la media de  \n cada persona", y = "Media de la persona", x = " Duración de la posición")
Grafico_media_individual
Grafico_media_empresa= ggplot(data = finalizados_trabajos, aes(x = Duracion, y = avgCompanyPosDuration)) +
  geom_point(col="green", size = 0.5)+
  theme_gray()+
  labs(title = "Distribución de \n la media de cada empresa", y = "Media de la empresa", x = " Duración de la posición")
Grafico_media_empresa
primeros_trabajos["temp2"] = c(rep(0, 37235))
join_v1 = as.data.frame(x = primeros_trabajos$memberUrn)
names(join_v1)[1] = "memberUrn"
join_v1["temp2"] = primeros_trabajos$temp2
data = inner_join(data, join_v1, by = "memberUrn")
primeros_trabajos = data %>% filter(data$Fase == "Primera")
ultimos_trabajos = data %>% filter(data$Fase  == "Ãšltima")
finalizados_trabajos = data %>% drop_na()
# Repetimos el chequeo
ggplot(data = finalizados_trabajos, aes(x = Duracion, y = Age_estimate)) +
  geom_point()
max(finalizados_trabajos$Age_estimate)
mean(finalizados_trabajos$Age_estimate)
# Vemos que estos datos tienen mÃ¡s sentido con lo que continuamos con nuestra modelizacion
data_modelo_completo = finalizados_trabajos %>%  filter(finalizados_trabajos$companyStaffCount > 4)
data_modelo_parcial = finalizados_trabajos %>% filter(finalizados_trabajos$companyStaffCount <= 4)
m1_completo = lm(Duracion ~ Experiencia + Nivel + Fase + avgCompanyPosDuration + genderEstimate + companyStaffCount + avgMemberPosDuration, data = finalizados_trabajos)
stargazer(m1_completo, type = "text")
m2_completo = lm(Ascenso ~ Experiencia + Nivel + Fase + avgCompanyPosDuration + genderEstimate + companyStaffCount + avgMemberPosDuration, data = finalizados_trabajos)
m1_parcial = lm(Duracion ~ Age_estimate + Nivel + genderEstimate, data = data_modelo_completo)
stargazer(m1_, type = "text")
stargazer(m2_completo, type = "text")
arbol_regresion = rpart(Duracion ~ Experiencia + Nivel + Fase + avgCompanyPosDuration + genderEstimate + companyStaffCount + avgMemberPosDuration, data=finalizados_trabajos, method="anova",)
plot(arbol_regresion)
text(arbol_regresion, use.n=TRUE, all=TRUE, cex=.6)
arbol_decision = rpart(Ascenso ~ Experiencia + Nivel + Fase + avgCompanyPosDuration + genderEstimate + companyStaffCount + avgMemberPosDuration, data=finalizados_trabajos, method="class",)
plot(arbol_decision)
text(arbol_decision, use.n=TRUE, all=TRUE, cex=.6)
#data_90 = finalizados_trabajos %>% filter(finalizados_trabajos$startDate < 1990) %>% filter(finalizados_trabajos$startDate > 1990)
# Y para finalizar, los guardamos
write.csv(finalizados_trabajos,"C:/Users/mberm/Desktop/TFM/data_trimmed2.csv", row.names = TRUE)

