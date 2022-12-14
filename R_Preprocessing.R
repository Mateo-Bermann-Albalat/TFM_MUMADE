# En primer lugar, cargamos los paquetes y los datos necesarios
library(ggplot2)
library(tidyverse)
library(stargazer)
library(scales)
library(ggtext)
library(rpart)
data = read.csv("C:/Users/mberm/Desktop/TFM/dump.csv")
data = data %>%  drop_na()
# Ya que vamos a crear más columnas a partir de la información que tenemos, lo insertamos directamente en el dataframe
data["Nivel"] = c(rep("Otro", 37235))
data["Fase"]  = c(rep("Intermedia", 37235))
data["Duracion"] = c(rep("Actualmente", 37235))
data["temp1"] = 2018 - as.numeric(substr(data$startDate, 0,4))
data["Age_estimate"] = data$ageEstimate - data$temp1
data["Ascenso"] = c(rep(0, 37235))
# Creamos vectores dummy que usaremos más tarde para generar las variables a estudiar
v1 = data$memberUrn
v1 = v1[-1]
v1 = c(v1, "test")
v2 = data$companyUrn
v2 = v2[-1]
v2 = c(v2, "test")
# Rellenamos las columnas añadidas
data$Nivel = ifelse(grepl(pattern = "Engineer",
                          x = data$posTitle,
                          fixed = TRUE), "T?cnico", data$Nivel)
data$Nivel = ifelse(grepl(pattern = "Developer",
                          x = data$posTitle,
                          fixed = TRUE), "T?cnico", data$Nivel)
data$Nivel = ifelse(grepl(pattern = "Engineer",
                          x = data$posTitle,
                          fixed = TRUE), "T?cnico", data$Nivel)
data$Nivel = ifelse(grepl(pattern = "Programmer",
                          x = data$posTitle,
                          fixed = TRUE), "T?cnico", data$Nivel)
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
                          fixed = TRUE), "M?nager", data$Nivel)
data$Nivel = ifelse(grepl(pattern = "Lead",
                          x = data$posTitle,
                          fixed = TRUE), "M?nager", data$Nivel)
data$Nivel = ifelse(grepl(pattern = "Head",
                          x = data$posTitle,
                          fixed = TRUE), "M?nager", data$Nivel)
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
                          fixed = TRUE), "Pr?ticas", data$Nivel)
data$Fase = ifelse(data$memberUrn != v1, "Última", data$Fase)
data$Ascenso = ifelse(data$companyUrn != v2, 1, 0)
v3 = data$Fase
v3 = v3[-37235]
v3 = c("test", v3)
data$Fase = ifelse(v3 == "Última", "Primera", data$Fase)
data$Fase[1] = "Primera"
tabla_temp_inicio_laboral = data %>%  select(20,25) 
colnames(tabla_temp_inicio_laboral)[2] = "fecha"
data = left_join(data, tabla_temp_inicio_laboral, by="memberUrn")
# Factorizamos
data$Nivel = factor( x = data$Nivel,
                     levels = c("Pr?cticas",
                                "Otro",
                                "Junior",
                                "T?cnico",
                                "Profesor",
                                "Senior",
                                "M?nager",
                                "Ejecutivo",
                                "Director"),
                     labels = c("Pr?cticas",
                                "Otro",
                                "Junior",
                                "T?cnico",
                                "Profesor",
                                "Senior",
                                "M?nager",
                                "Ejecutivo",
                                "Director"),
                     ordered = FALSE)
data$Fase = factor( x = data$Fase,
                    levels = c("Primera",
                               "Intermedia",
                               "?ltima"),
                    labels = c("Primera",
                               "Intermedia",
                               "?ltima"))
data$endDate = as.Date(data$endDate)
data$startDate = as.Date(data$startDate)
data$fecha = as.Date(data$fecha)
data$Experiencia = data$startDate - data$fecha
data$Experiencia = as.numeric(data$Experiencia)
data$Duracion = data$endDate - data$startDate
data$Duracion = as.numeric(data$Duracion)
# Filtramos para hacer exploración del crecimiento laboral. Al tener cada perfil único un primer trabajo y algunos un último
primeros_trabajos = data %>% filter(data$Fase == "Primera")
ultimos_trabajos = data %>% filter(data$Fase  == "?ltima")
finalizados_trabajos = data %>% drop_na()
# Y vemos que a medida que sube la responsabilidad sube la edad, con lo que los datos encajan en la teoría
# Vamos a visualizar los datos en dos dimensiones para hacer una rápida comprobación visual sobre si la relación es lineal o polinómica
Grafico_Age_estimate= ggplot(data = finalizados_trabajos, aes(x = Duracion, y = Age_estimate)) +
  geom_point(col="green") +
  theme_gray() +
  labs(title = "A?os estimados al entrar al trabajo", y = "Edad", x = "Duraci?n de la posici?n")
Grafico_Age_estimate
Grafico_numero_empleados = ggplot(data = finalizados_trabajos, aes(x = Duracion, y = companyStaffCount)) +
  geom_point(col="green", size = 0.5)+
  theme_gray()+
  labs(title = "Distribuci?n del tama?o de las \n empresas en relaci?n a la duraci?n", y = "N?mero de empleados", x = " Duraci?n de la posici?n")+
  scale_y_continuous(labels = comma)
Grafico_numero_empleados
Grafico_numero_conexiones = ggplot(data = finalizados_trabajos, aes(x = Duracion, y = connectionsCount)) +
  geom_point(col="green", size = 0.5)+
  theme_gray()+
  labs(title = "Distribuci?n del n?mero de  \n  contactos en relaci?n a la duraci?n", y = "N?mero de conexiones", x = " Duraci?n de la posici?n")+
  scale_y_continuous(labels = comma)
Grafico_numero_conexiones
Grafico_fecha_finalizacion = ggplot(data = finalizados_trabajos, aes(x = Duracion, y = endDate)) +
  geom_point(col="green", size = 0.5)+
  theme_gray()+
  labs(title = "Distribuci?n de la fecha  de  \n  finalizaci?n en relaci?n a la duraci?n", y = "Fecha de finalizaci?n", x = " Duraci?n de la posici?n")
Grafico_fecha_finalizacion
Grafico_fecha_inicio = ggplot(data = finalizados_trabajos, aes(x = Duracion, y = startDate)) +
  geom_point(col="green", size = 0.5)+
  theme_gray()+
  labs(title = "Distribuci?n de la fecha  de  \n  inicio en relaci?n a la duraci?n", y = "Fecha de inicio", x = " Duraci?n de la posici?n")
Grafico_fecha_inicio
Grafico_numero_seguidores = ggplot(data = finalizados_trabajos, aes(x = Duracion, y = followersCount)) +
  geom_point(col="green", size = 0.5)+
  theme_gray()+
  labs(title = "Distribuci?n del n?mero de  \n  seguidores en relaci?n a la duraci?n", y = "N?mero de seguidores", x = " Duraci?n de la posici?n")
Grafico_numero_seguidores
finalizados_trabajos = finalizados_trabajos %>% filter( finalizados_trabajos$Experiencia > -0.1)
Grafico_experiencia = ggplot(data = finalizados_trabajos, aes(x = Duracion, y = Experiencia)) +
  geom_point(col="green", size = 0.5)+
  theme_gray()+
  labs(title = "Relaci?n de la experiencia \n con la duraci?n ", y = "Experiencia", x = " Duraci?n de la posici?n")+
  scale_y_continuous(labels = comma)
Grafico_experiencia
Grafico_nivel = ggplot(data = finalizados_trabajos, aes(x = Nivel)) +
  geom_bar(fill="green")+
  theme_gray()+
  labs(title = "Distribuci?n  del nivel \n de los trabajadores", y = "Frecuencia", x = "Nivel")+
  theme(axis.text = element_text(angle = 90))
Grafico_nivel
Grafico_fase = ggplot(data = finalizados_trabajos, aes(x = Fase)) +
  geom_bar(fill="green")+
  theme_gray()+
  labs(title = "Distribuci?n  de la fase", y = "Frecuencia", x = "Fase")+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
Grafico_fase
# Eliminamos at?picos 
data = data %>% filter( data$companyFollowerCount < 50000)
Grafico_genero = ggplot(data = finalizados_trabajos, aes(x = genderEstimate)) +
  geom_bar(fill="green")+
  theme_gray()+
  labs(title = "Distribuci?n del g?nero", x = "G?nero", y = "Frecuencia")
Grafico_genero
Grafico_media_individual= ggplot(data = finalizados_trabajos, aes(x = Duracion, y = avgMemberPosDuration)) +
  geom_point(col="green", size = 0.5)+
  theme_gray()+
  labs(title = "Distribuci?n de la media de  \n cada persona", y = "Media de la persona", x = " Duraci?n de la posici?n")
Grafico_media_individual
Grafico_media_empresa= ggplot(data = finalizados_trabajos, aes(x = Duracion, y = avgCompanyPosDuration)) +
  geom_point(col="green", size = 0.5)+
  theme_gray()+
  labs(title = "Distribuci?n de \n la media de cada empresa", y = "Media de la empresa", x = " Duraci?n de la posici?n")
Grafico_media_empresa
primeros_trabajos["temp2"] = c(rep(0, 37235))
join_v1 = as.data.frame(x = primeros_trabajos$memberUrn)
names(join_v1)[1] = "memberUrn"
join_v1["temp2"] = primeros_trabajos$temp2
data = inner_join(data, join_v1, by = "memberUrn")
primeros_trabajos = data %>% filter(data$Fase == "Primera")
ultimos_trabajos = data %>% filter(data$Fase  == "Última")
finalizados_trabajos = data %>% drop_na()
# Repetimos el chequeo
ggplot(data = finalizados_trabajos, aes(x = Duracion, y = Age_estimate)) +
  geom_point()
max(finalizados_trabajos$Age_estimate)
mean(finalizados_trabajos$Age_estimate)
# Vemos que estos datos tienen más sentido con lo que continuamos con nuestra modelizacion
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

