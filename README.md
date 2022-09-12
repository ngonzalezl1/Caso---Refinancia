# Caso---Refinancia
Este código pretende encontrar la probabilidad de pago de 100 cliente de Refinancia durante el mes de julio.

#############################################################################
#
##                    Caso - Ciencias Cuantitativas - Refinancia                   
#
##                     Nicolás González Llano Cédula: 1020843404
#
##                                 Fecha: 10/09/2022
#
#############################################################################
### Primero, cargo la base de datos -------------------------------------------
rm(list=ls()) #Limpio el lugar de trabajo

require(pacman) #Si es falso, debo instalar el paquete
install.packages('pacman') #Instalo la libreria pacman
#Cargo mis paquetes
p_load(broom, dplyr, stargazer, lessR, rio, stringi, tidyr, tidyverse, caret, modelsummary, gamlr, lubridate, class, gmodel, ISLR)

#Primer debo crear una carpeta que contenga un archivo con los datos
setwd(choose.dir()) #Selecciono la carpeta creada para establecer el path (dirección) de los documentos

df <- read.table('Clientes_a_Calificar.csv', header = T, sep = ',', stringsAsFactors = F)

setwd(choose.dir()) # Seleccionar la carperta llamada "historicos"
bases <- paste0('Refinancia_20220', 2:6, '.csv') # Creo vector con el nombre de mis archivos
historico <- data.frame() # Este será el nombre de mi base de datos "TRAIN"
for (base in bases) {
  print(base)
  temp <- read.table(base, header = T, sep = ',', stringsAsFactors = F)
  historico <- bind_rows(historico, temp)
}

### Evalúo missing values:
na_percentage <-sapply(historico, function(y) sum(length(which(is.na(y))))/length(historico$Cosecha))#creo una función para saber cuantos NAs hay por columna 
#### Al parecer no hay missing values
#### Estadísticas Descriptivas:
#### Esto para mi documento en latex:
stargazer(historico, type='latex')
#### Esto para ver resultados en la interfaz de r studio
stargazer(historico, type='text')
#### Veo diferencias interesantes entre variables a traves del tiempo
for (base in bases) {
  print(base)
  temp <- read.table(base, header = T, sep = ',', stringsAsFactors = F)
  stargazer(temp, type='text')
}

#### Agrego nuevas variables, limpio la base y ajusto los datos de entrenamiento-------------------------------------------
url_divipola <- import("https://geoportal.dane.gov.co/descargas/divipola/DIVIPOLA_Municipios.xlsx", encoding = "UTF-8", check.names = FALSE)
names(url_divipola)[names(url_divipola) == "Codificación de la División Político Administrativa de Colombia - DIVIPOLA. Agosto 2022"] <- "codepto"
names(url_divipola)[names(url_divipola) == "...2"] <- "depto"
names(url_divipola)[names(url_divipola) == "...3"] <- "codmuni"
names(url_divipola)[names(url_divipola) == "...4"] <- "municipio"
names(url_divipola)[names(url_divipola) == "...5"] <- "for_drop"
names(url_divipola)[names(url_divipola) == "...6"] <- "latitud"
names(url_divipola)[names(url_divipola) == "...7"] <- "longitud"
url_divipola = url_divipola[-1:-5,]
url_divipola$for_drop <- NULL
url_divipola$depto <- NULL
url_divipola$codepto <- NULL
url_divipola$codmuni <- NULL
url_divipola$municipio <- toupper(stri_trans_general(url_divipola$municipio,"Latin-ASCII"))
names(historico)[names(historico) == "Ciudad"] <- "municipio"
historico$municipio <- toupper(stri_trans_general(historico$municipio,"Latin-ASCII"))
url_divipola$municipio[url_divipola$municipio == 'SAN JOSE DE CUCUTA'] <- 'CUCUTA'
url_divipola$municipio[url_divipola$municipio == 'CARTAGENA DE INDIAS'] <- 'CARTAGENA'
url_divipola$municipio[url_divipola$municipio == 'GUADALAJARA DE BUGA'] <- 'BUGA'
url_divipola$municipio[url_divipola$municipio == 'DOS QUEBRADAS'] <- 'DOSQUEBRADAS'
url_divipola$municipio[url_divipola$municipio == 'VILLA DE SAN DIEGO DE UBATE'] <- 'UBATE'
url_divipola$municipio[url_divipola$municipio == 'SAN SEBASTIAN DE MARIQUITA'] <- 'MARIQUITA'
historico$municipio[historico$municipio == 'ITSMINA'] <- 'ISTMINA'
url_divipola$municipio <- gsub('.', '', url_divipola$municipio, fixed = T)
url_divipola$municipio <- gsub(',', '', url_divipola$municipio, fixed = T)
url_divipola <- url_divipola %>% drop_na()
dta_full <- full_join(url_divipola, historico, by="municipio")
dta_full$latitud <- ifelse(dta_full$municipio == '[FUERA DE COLOMBIA]', 38.934338, dta_full$latitud)
dta_full$longitud <- ifelse(dta_full$municipio == '[FUERA DE COLOMBIA]', -77.093885, dta_full$longitud)
dta_full$latitud <- ifelse(dta_full$municipio == 'EXTERIOR EN ESTADOS UNIDOS DE AMERICA', 38.934338, dta_full$latitud)
dta_full$longitud <- ifelse(dta_full$municipio == 'EXTERIOR EN ESTADOS UNIDOS DE AMERICA', -77.093885, dta_full$longitud)
dta_full$latitud <- ifelse(dta_full$municipio == 'EXTERIOR EN ANDORRA', 38.934338, dta_full$latitud)
dta_full$longitud <- ifelse(dta_full$municipio == 'EXTERIOR EN ANDORRA', -77.093885, dta_full$longitud)
dta_full$latitud <- ifelse(dta_full$municipio == 'EXTERIOR EN COSTA RICA', 38.934338, dta_full$latitud)
dta_full$longitud <- ifelse(dta_full$municipio == 'EXTERIOR EN COSTA RICA', -77.093885, dta_full$longitud)

#### AJUSTO EL MODELO-------------------------------------------

na_percentage <-sapply(dta_full, function(y) sum(length(which(is.na(y))))/length(dta_full$Cosecha))#creo una función para saber cuantos NAs hay por columna 
data_x <- as.data.frame(na_percentage)
#### En un primer proceso de eliminación de variables, eliminamos aquellas con un 
#alto porcentaje de missing values
var <- cbind(Var_name = rownames(data_x), data_x)
rownames(var) <- 1:nrow(var)
var_for_drop <- var[var$na_percentage>=0.45,]
var_for_keep <- var[var$na_percentage<0.45,]
count(var) # Contamos cuantas variables tenemos en total 
count(var_for_keep) # Contamos cuantas variables tienen % missing menor o igual a 45% 
count(var_for_drop) # Contamos cuantas variables tienen % missing mayor a 45% 

#### Ninguna variable tiene un alto porcentaje de missing values
gc()
memory.size() ### Checking your memory size
memory.limit() ## Checking the set limit
memory.limit(size=10000000)


dt_1 <- dta_full %>% subset(Cosecha==202202)

set.seed(4444) #establecemos una semilla para asegurar la reproducibilidad de los resultados
particion <- createDataPartition(y = dta_full$Pago,
                                 p = 0.6,
                                 list = F)
train <- dta_full[particion,]
test <- dta_full[-particion,]
#Usando el paquete caret Separamos la base en entrenamiento (60%) y prueba (40%)

#### Estimación LOGIT:
model <- as.formula("Pago~DiasMora+SaldoCapital+PagosRealizadoUltMes+PagosRealizadoUlt2Meses+PagosRealizadoUlt3Meses+PagosRealizadoUlt4Meses+PagosRealizadoUlt5Meses+PagosRealizadoUlt6Meses+LocalizacionUlt6Meses+AcuerdosUlt6Meses+AcuerdosUlt3Meses+SaldoxCliente+PagosTotales+factor(TipoProducto)+latitud+longitud")
model <- as.formula("Pago~DiasMora+latitud+longitud+factor(TipoProducto)+LocalizacionUlt6Meses+AcuerdosUlt6Meses+SaldoxCliente+PagosTotales")
logit <- glm(model, family=binomial(link="logit"), data=train)
tidy(logit)

dta_full$Pago_log <- predict(logit, newdata=dta_full, type="response")

#### Defino la regla del 70%
rule=0.7
dta_full$log_07 <- ifelse(dta_full$Pago_log>rule,1,0)
#### Variables:

##### PagosRealizadoUltMes
##### PagosRealizadoUlt2Meses
##### PagosRealizadoUlt3Meses
##### PagosRealizadoUlt4Meses
##### PagosRealizadoUlt5Meses
##### LocalizacionUlt6Meses
##### PagosRealizadoUlt6Meses
##### AcuerdosUlt6Meses
##### AcuerdosUlt3Meses
##### SaldoxCliente

##### predecimos en la base de prueba
pred_m1 <- predict(modelo_1, test)
MAE_m1 <- MAE(test$ingtot, pred_m1)
MAPE_m1 <- MAPE(test$ingtot, pred_m1)
RMSE_m1 <- RMSE(test$ingtot, pred_m1)



# Matriz de confusión:
cm_log <- confusionMatrix(data=factor(dta_full$Pago),
                         reference=factor(dta_full$log_07),
                         mode="sens_spec", positive="1")
cm_log
cm = cm_log$table
(cm[1,1]+cm[2,2])/sum(cm) # Accurancy
cm[2,2]/sum(cm[,2]) # Sensitivity
cm[1,1]/sum(cm[,1]) #Specificity
cm[2,1]/sum(cm[2,]) # Ratio Falsos Positivos
cm[1,2]/sum(cm[1,]) # Ratio Falsos Negativos

stargazer(dta_full, type='text')

# Curva de ROC:
