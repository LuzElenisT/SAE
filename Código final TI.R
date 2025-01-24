#proporción de TI
library(data.table)
library(dplyr)
library(survey)
library(haven)
library(ggplot2)

list.files("D:/Pasantía/Trabajo Intantil")
setwd("D:/Pasantía/Trabajo Intantil")
Datos <- fread("CaracteristicasTI2023.csv") 
Datos2 <- fread("OcupadosTI2023.csv")
datos4 <- read_sas("base_2023.sas7bdat")
fex <- fread("TOTAL_23.csv")

# Definir Ocupados según el módulo de Ocupados 
Datos2 <- Datos2 %>%
  mutate(
    OCI = ifelse(P6430 %in% c(1, 2, 3, 4, 5, 6, 7, 8), 1, 0)
  )

# Continua codigo
# Seleccionar columnas adicionales necesarias
columnas_necesarias <- c("DIRECTORIO", "SECUENCIA_P", "ORDEN", "MES", 
                         "FT", "OCI","P6430")
# Realizar el join incluyendo las variables necesarias
data_com_oc <- left_join(Datos, Datos2[, columnas_necesarias, with = FALSE], 
                         by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN", "MES")) #datos con caracteristica y de ocupados

columnas_comunes1 <- intersect(names(Datos2), names(Datos))

data_com_oc <- data_com_oc %>%
  mutate(P6040 = ifelse(is.na(P6040),99,P6040),
         FT = ifelse(is.na(FT),99,FT),
         Ocupados = ifelse(P6040<= 17 & OCI==1, # Incluir información de los ocupados (Cambiar FT por ocupados que definas previamente)
                           1, ifelse(P6040> 17, NA,0)))

# Seleccionar columnas por posición y nombre en data.table
columnas <- c(names(data_com_oc)[c(2,4:7, 10:12, 16)], "OCI", "P6430", "Ocupados", "FT")
data_com_oc <- data_com_oc[, ..columnas]

# Paso 3 Crear la variable "OCI" (Ocupados MInfantil)
Datos3 <- fread("total_tri_4_2023_pl.csv")
columnasS <- c(names(Datos3)[c(1:4,19)], "P400","P3564","P401","P402","P403","FEX_C")
data_comb_MI <- Datos3[, ..columnasS]
#NA en columnas específicas a 0
data_comb_MI <- data_comb_MI %>% mutate_at(vars(P400,P3564,P401, P402,P403), 
                                           ~ ifelse(is.na(.), 99, .))

# Calculando ocupados para 5 a 10 años 
data_comb_MI <- data_comb_MI %>%
  mutate(Ocupados_TI = ifelse((P400 == 1 & P3564 == 1) 
                              | (P401 == 1) 
                              | (P402 == 1) 
                              | (P403 == 1), 1, 0))
columnas_comunes2 <- intersect(names(data_com_oc),names(data_comb_MI))
#columnas_comunes2 <- columnas_comunes2[-c(4,5)]
data_comb_Total <- left_join(data_com_oc, data_comb_MI %>% select(-CLASE), 
                             by = c("DIRECTORIO",  "SECUENCIA_P", "ORDEN"))
#data_comb_Total <- data_comb_Total %>%
#mutate(Ocupados = ifelse(Ocupados.x == 1 | Ocupados.y == 1, 1, 0))
# Paso 4 Crear la variable "Niños" para niños entre 5 y 17 años
data_comb_Total <- data_comb_Total %>%
  mutate(Ninos = ifelse(P6040 >= 5 & P6040 <= 17, 1, 0))

table(data_comb_Total[,c("P6040","Ocupados_TI")])
data_comb_Total <- data_comb_Total %>% mutate(Ocupados_TI = ifelse(is.na(Ocupados_TI),0,Ocupados_TI),
                                              Ocupados2 = ifelse(is.na(Ocupados),0,Ocupados),
                                              Ocupados_F = ifelse(Ocupados2==1 | Ocupados_TI==1,1,0))

sum(data_comb_Total$Ocupados, na.rm = TRUE)
sum(data_comb_Total$Ocupados_TI, na.rm = TRUE)
sum(data_comb_Total$Ocupados_F, na.rm = TRUE)

###############################################################################
#################PRUEBA PARA SABER SI ESTÁN CONTENIDAS#########################
###############################################################################
#columnas_interes <- c("DIRECTORIO", "SECUENCIA_P", "ORDEN", "Ocupados.x", "Ocupados.y", "Ocupados")
#filas_ocupados_y_1 <- subset(data_comb_Total, Ocupados.y == 1, select = columnas_interes)
#filas_ocupados_y_2 <- subset(data_comb_Total, Ocupados.x == 1, select = columnas_interes)
# Mostrar el resultado
#print(filas_ocupados_y_2)

################################## ESTIMACIÓN #####################################
# Crear el diseño de la encuesta
# 
datos4$MES <- as.integer(datos4$MES)
Datos_Total <- left_join(data_comb_Total,datos4,
                         by = c("DIRECTORIO","SECUENCIA_P","ORDEN","MES"))

# fex_agrupado <- fex %>%
#   group_by(DIRECTORIO, SECUENCIA_P, MES) %>%
#   summarise(FEX_DPTO = mean(FEX_DPTO, na.rm = TRUE))

data_com <- left_join(Datos_Total, fex %>% select(DIRECTORIO, SECUENCIA_P, ORDEN, MES, FEX_DPTO) %>% unique(), 
                      by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN", "MES"))
data_com <- data_com %>%
  mutate(FEX_C18_div = FEX_C18 / 12)

data_com <- data_com %>%
  mutate(PESO_FINAL = ifelse(is.na(FEX_DPTO), FEX_C18_div, FEX_DPTO))

data_com_ninos <- data_com %>% 
  filter(Ninos == 1)

data_com_ninos2 <- data_com_ninos %>% group_by(SEGMENTO,MES,DIRECTORIO, PESO_FINAL, MPIO) %>% 
  summarise(Ninos= sum(Ninos, na.rm=TRUE),
            Ninos_ocu = sum(Ocupados_F, na.rm=TRUE))

################################################################################
# Definir el diseño de la muestra con PPS utilizando el método Brewer
design <- svydesign(
  id = ~SEGMENTO+DIRECTORIO,                   # PSU: Segmentos de hogares
  strata = ~MPIO,              # Estratos: Municipios
  weights = ~PESO_FINAL,  
  pps = "brewer",
  data = data_com_ninos2,
  deff = TRUE 
)

# Convertir el diseño a un diseño de replicación bootstrap
boot_design <- as.svrepdesign(design, 
                              type = "bootstrap", 
                              replicates = 1000, 
                              compress =TRUE)
#Estimación proporción
prop_mun <-svyby(~Ninos_ocu,
                 denominator=~Ninos,
                 by=~MPIO, 
                 design=boot_design, 
                 FUN=svyratio,
                 deff = TRUE,
                 df = TRUE)


#Intervalo de confianza 
intervalomu <- confint(prop_mun)
intervalodpto <- confint(prop_dpto)

#Coeficiente de Variación
cvmun <- cv(prop_mun)
cvdpto <- cv(prop_dpto)

#Log Coeficiente de VAriación
Log_cv_mun <- log(cvmun)
Log_cv_dpto <- log(cvdpto)

#Gradosde libertad
gl_global <-degf(design)

# Grados de libertad manuales
gl_muni <- data_com_ninos %>%
  group_by(MPIO) %>%
  summarise(
    num_psu = n_distinct(SEGMENTO),  # Contar el número de PSU en cada municipio
    grados_libertad = num_psu - 1
    # Aplicar la fórmula de grados de libertad
  )

n <- data_com_ninos %>% 
  group_by(MPIO) %>%
  summarise(num_psu = n_distinct(SEGMENTO)) #preguntar

deff <- prop_mun$DEff

Pob <- data_com_ninos %>%
  group_by(MPIO) %>%
  summarize(poblacion = sum(PESO_FINAL))

varianza <- prop_mun$`se.Ninos_ocu/Ninos`^2 
resultados_mun <- data.frame(MPIO=gl_muni$MPIO,
                             Propocion_niños_oc=prop_mun$`Ninos_ocu/Ninos`,
                             IC_inf= intervalomu[,1],
                             IC_sup= intervalomu[,2],
                             CV= cvmun, 
                             gl_muni = gl_muni$grados_libertad,
                             gl_global = rep(gl_global,595),
                             se = prop_mun$`se.Ninos_ocu/Ninos`,
                             varianza = varianza,
                             n = n$num_psu,
                             Pob = Pob,
                             deff = deff)

print(resultados_mun)

################### MODELO FH ##################################################
list.of.packages <- c("ggplot2","ggpubr", "dplyr", "emdi", "SAEval", "data.table", 
                      "plotly","bigstep", "readxl", "caret")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) {
  install.packages(new.packages)
}
for (i in list.of.packages) {
  library(i,character.only = TRUE)
}
rm(i,list.of.packages,new.packages)

#_________________________________Código 6 y 7____________________________________

estim <- resultados_mun
# Se filtran los cv
estim[,"cvlog"] <- ifelse(estim[,"Propocion_niños_oc"] <= 0.5, 
                          estim[,"se"]/(-estim[,"Propocion_niños_oc"] * log(estim[,"Propocion_niños_oc"])),
                          estim[,"se"]/(-(1 - estim[,"Propocion_niños_oc"]) * log(1 - estim[,"Propocion_niños_oc"])))
estim[,"n_eff"] <- estim[,"n"]/estim[,"deff"]

#siedno n numero segmentos
estim_graph <- estim[,c("MPIO","Propocion_niños_oc")]
estim_graph["Var"] <- "Total"
nrow(estim)
estim <- estim[estim[,"n"]>=10,]
nrow(estim)
#estim <- estim[estim[,"n"]/estim[,"deff"]>=30,]
#nrow(estim)
estim <- estim[estim[,"cvlog"]<=0.5,]
nrow(estim)
estim <- estim[estim[,"gl_muni"]>=5,]
nrow(estim)
# estim <- estim[estim[,"rho"]>=(1-estim[,"n_psu_municipio"])/(estim[,"n_psu_municipio"]-1),]
estim <- estim[estim[,"deff"]>=1,]
nrow(estim)

load("D:/Pasantía/Trabajo Intantil/FIES_final_v2 (1).RData")

dt1 <- df_merge
dt2 <- estim
df_final <- left_join(dt1,dt2, "MPIO")
FIES_Final<-df_final
df_sae_comp <- data.frame(FIES_Final)
df_sae_trim <- data.frame(FIES_Final)
data=df_sae_comp[!is.na(df_sae_comp[,"Propocion_niños_oc"]),]
data=data[data[,"Propocion_niños_oc"]!=0,]
data$var.tot.rel <- data$varianza/(data$Propocion_niños_oc**2)
data$Prop_inv=1/data$Propocion_niños_oc
data$Prop_raiz=sqrt(data$Propocion_niños_oc)

#graficos preliminares con theta sin transfomar

sum(is.na(estim$Pob.poblacion))
data$Pob <- data[!is.na(data$Pob.poblacion),'Pob.poblacion']
data$prop_pob = data$n/data$Pob
data <- as.data.frame(data)
# data <- data[data[]]
gvf <- lm(log(varianza) ~ prop_pob + n,
          data=data)
summary(gvf)

data$var.tot.smooth <- exp(predict(gvf))
#data$var.tot.smooth <- predict(gvf)*(data$ModerateSevere**2)
data$var.tot.smooth.corr <- data$var.tot.smooth*(sum(data$varianza,na.rm=T)/sum(data$var.tot.smooth,na.rm=T))

ncomunal <- nrow(data)
plot_gvf_compare <- ggplot(data=data[order(data$n),],aes(x=1:ncomunal)) +
  geom_line(aes(y=varianza,color="VarDirEst")) +
  geom_line(aes(y=var.tot.smooth.corr,color="GVF")) +
  scale_x_continuous(breaks = seq(1, ncomunal, by = 10),
                     labels = data$n[order(data$n)][seq(1,ncomunal, by = 10)]) +
  scale_colour_manual("", values = c("VarDirEst"="red","GVF"="blue")) +
  labs(y = "Variances", x = "n", color = " ")

plot_gvf_compare1 <- ggplot(data=data[order(data$n),],aes(x=n)) +
  geom_line(aes(y=varianza,color="VarDirEst")) +
  geom_line(aes(y=var.tot.smooth.corr,color="GVF")) +
  #scale_x_continuous(breaks = seq(1, ncomunal, by = 10),
  #                   labels = data$n[order(data$n)][seq(1,ncomunal, by = 10)]) +
  scale_colour_manual("", values = c("VarDirEst"="red","GVF"="blue")) +
  labs(y = "Variances", x = "n", color = " ")
plot_gvf_compare
plot_gvf_compare1
ggplotly(plot_gvf_compare)

plot_gvf_compare2 <- ggplot(data=data[order(data$n),],aes(x=Propocion_niños_oc)) +
  geom_line(aes(y=varianza,color="VarDirEst")) +
  geom_line(aes(y=var.tot.smooth.corr,color="GVF")) +
  #scale_x_continuous(breaks = seq(1, ncomunal, by = 10),
  #                   labels = data$n[order(data$n)][seq(1,ncomunal, by = 10)]) +
  scale_colour_manual("", values = c("VarDirEst"="red","GVF"="blue")) +
  labs(y = "Variances", x = "Proporción de niños ocupados", color = " ")
plot_gvf_compare2


plot_gvf_compare2 <- data %>% arrange(n) %>% group_by(n) %>%
  summarise(var.tot.mean=mean(varianza),
            var.tot.smooth.corr.mean=mean(var.tot.smooth.corr)) %>%
  ggplot(aes(x = n)) +
  geom_line(aes(y = var.tot.mean, color = "VarDirEst")) +
  geom_line(aes(y = var.tot.smooth.corr.mean, color = "GVF")) +
  scale_colour_manual("", 
                      values = c("VarDirEst"="red","GVF"="blue")) 

df_sae_comp[,"var.tot.smooth.corr"] <- NULL
df_sae_comp <- df_sae_comp %>% left_join(data[,c("MPIO","var.tot.smooth.corr","var.tot.smooth")], by="MPIO")

ggplot(df_sae_comp[!is.na(df_sae_comp$var.tot.smooth.corr),], aes(x=var.tot.smooth.corr,y=varianza)) + 
  geom_point() + 
  geom_smooth(method=lm)

# Por ultimo se define las variables para el modelo de arcoseno
df_sae_comp <- df_sae_comp %>%
  mutate(deff= n/n_eff,
         deff_fgv = var.tot.smooth.corr/(varianza/deff),
         deff_fgv = ifelse(deff_fgv < 1, 1, deff_fgv), # Comentario CEPAL: Criterio MDS para regularizar el DeffFGV
         n_eff_fgv = n/deff_fgv,
         Propocion_niños_oc = ifelse(!is.na(var.tot.smooth.corr),Propocion_niños_oc,NA),
         var.tot = var.tot.smooth)
df_sae_comp <- data.frame(df_sae_comp)

names(df_sae_comp)

sum(!is.na(df_sae_comp[,c("Propocion_niños_oc")]))

save(df_sae_comp, file=file.path("D:","Pasantía","Trabajo Intantil","df_sae_comp.RData"))

load("D:/Pasantía/Trabajo Intantil/df_sae_comp.RData")

library(fastDummies)

FIES_Final <- df_sae_comp
summary(FIES_Final)
vars_dummy <- names(FIES_Final[, sapply(FIES_Final, class) %in% c('character', 'factor')])[-1]
FIES_FINAL2023 <- dummy_cols(FIES_Final, select_columns = vars_dummy , remove_first_dummy = TRUE, remove_selected_columns = TRUE)
summary(FIES_FINAL2023)
dim(FIES_FINAL2023)

na_counts <- colSums(is.na(FIES_Final))
na_counts[na_counts > 0]

#variables estandarizadas de df_merge
FIES_FINAL2023_ <- FIES_Final %>% dplyr::select(-c(Propocion_niños_oc, 
                                                   se,varianza, gl_muni, gl_global, cvlog,
                                                   deff, CV,n_eff,IC_sup,IC_inf,n,
                                                   Pob.poblacion,deff_fgv,n_eff_fgv,
                                                   Pob.MPIO, var.tot.smooth.corr,
                                                   var.tot.smooth,var.tot,
                                                   DIG2023_Evento_Minas ))%>%
  dplyr::select(where(~ is.numeric(.)))%>%
  mutate_if(is.numeric, function(x) as.numeric(scale(x)))
                                                

na_counts2 <- colSums(is.na(FIES_FINAL2023_))
na_counts2[na_counts2 > 0]

data_arcsin_3 <- prepare_data(FIES_Final$Propocion_niños_oc, FIES_FINAL2023_)
str(FIES_FINAL2023_)

res_arcsin_3 <-  data_arcsin_3 %>% reduce_matrix(minpv = 0.05) %>% stepwise(crit = aic)
#variables correlacionadas
cor(round(res_arcsin_3$X,2))
res_arcsin_3$model
summary(res_arcsin_3)
summary(FIES_FINAL2023[,res_arcsin_3$model])

library(Rfast)

my_formula_3 <- as.formula(                      
  paste("Propocion_niños_oc ~ ", 
        paste(res_arcsin_3$model, 
              collapse = " + ")))

FIES_Final$MPIO <- as.numeric(FIES_Final$MPIO)
FIES_FINAL2023_$MPIO <- FIES_Final$MPIO
FIES_FINAL2023$MPIO <- FIES_Final$MPIO
#FIES <- merge(FIES_FINAL2023_, FIES_FINAL2023, by="MPIO", all.x=TRUE)
FIES <- merge(FIES_FINAL2023_, FIES_Final[,c("MPIO","Propocion_niños_oc", 
                                             "n_eff_fgv","var.tot.smooth.corr",
                                             "var.tot.smooth",
                                             "n")], by.x="MPIO", by.y="MPIO") 

suppressMessages(
model_arcsin_3 <- fh(my_formula_3, 
                     vardir = "var.tot.smooth.corr", domains = "MPIO", method = "reml",
                     combined_data = FIES, 
                     transformation = "arcsin", backtransformation = "bc",
                     eff_smpsize = "n_eff_fgv", B = 100,
                     mse_type = "boot", MSE = TRUE)
)

summary(model_arcsin_3)

fh <- estimators(model_arcsin_3, MSE = TRUE, CV = TRUE) %>% as.data.frame()


fh$CVLOG_fh <- ifelse(fh$FH <= 0.5, 
                      sqrt(fh$FH_MSE)/(-fh$FH * log(fh$FH)),
                      sqrt(fh$FH_MSE)/(-(1 - fh$FH) * log(1 - fh$FH)))

fh$CVLOG_diret <- ifelse(fh$Direct_MSE <= 0.5, 
                         sqrt(fh$Direct_MSE)/(-fh$Direct * log(fh$Direct)),
                         sqrt(fh$Direct_MSE)/(-(1 - fh$Direct) * log(1 - fh$Direct)))


logcv_menor3 <- fh %>% filter(fh$CVLOG_fh <=0.30)
                       
## Gráficos

#Error estandar
fh$temp <- ifelse(is.na(fh$Direct),"Municipios sin muestra","Municipios con muestra")
ggplot(fh, aes(x=sqrt(FH_MSE), y=FH, col=temp)) + geom_point() + labs(y = "Estimación FIES" , x = "Error estandár") 

#CV
ggplot(fh, aes(x = FH_CV, y = FH, col = temp)) +
  geom_point() +
  xlim(0, 1) +
  labs(y = "Estimación FIES" , x = "CV")


#Cvlog
ggplot(fh, aes(x=CVLOG_fh, y=FH, col=temp)) + geom_point() + xlim(0,0.4) + labs(y = "Estimación FIES" , x = "CV LOG") 

a <- data.frame(var="Municipios  con muestra" , var2=(fh$Direct_CV))
b <- data.frame(var="Municipios sin muestra", var2=(fh$FH_CV[!is.na(fh$Direct_CV)]))
a <- rbind(a,b)

p <- ggplot(a, aes(x=var, y=var2)) + geom_boxplot()+
  labs(x="",y="CV") 
p


a <- data.frame(var="Directo", var2=sqrt(fh$Direct_MSE))
b <- data.frame(var="Sintético", var2=sqrt(fh$FH_MSE[!is.na(fh$Direct_MSE)]))
a <- rbind(a,b)

p <- ggplot(a, aes(x=var, y=var2)) + geom_boxplot()+
  labs(x="",y="Error estándar")
p


plot(model_arcsin_3)

######### VALIDACIÓN DEL MODELO 

### Standardised residuals -----------------------------------------------
ggplot(data.frame(Residuals = model_arcsin_3$model$std_real_residuals)) +
  geom_point(col="blue", aes(y = Residuals, x = 1:length(Residuals))) +
  labs(y = "Residuos estandarizados", x = "Municipios") +
  geom_hline(yintercept = 0, col = "red") 

### Standardised predicted values ----------------------------------------
pred.est <- (model_arcsin_3$model$fitted)/sd(model_arcsin_3$model$std_real_residuals)
res <- data.frame(residuals=model_arcsin_3$model$std_real_residuals,pred.est)

ggplot(res, aes(pred.est,residuals)) +
  geom_point(col = "blue") +
  geom_smooth(method = "lm", se = FALSE, aes(color = "regresión")) +
  geom_hline(aes(yintercept = 0, col = "y = 0"), size = 1) +
  labs(y = "Residuos estandarizados", x = "Valores predichos estandarizados") +
  scale_color_manual(name = "rectas", values = c("regresión" = "green", "y = 0" = "red"))


################################################################################

total_est <- sum(data_com$PESO_FINAL)
sub_df <- data_com_ninos[, c("P400", "P3564", "P401","P402","P403")]
same_values <- apply(sub_df, 1, function(row) length(unique(row)) == 99)
sum(same_values == TRUE)

sub_df2 <- Datos2[,c("P6240","P6250","P6260","P6270","P6240S2")]
same_values <- apply(sub_df2, 1, function(row) length(unique(row)) == 99)
sum(same_values == TRUE)

resumen_segmentos <- data_com_ninos %>%
  group_by(SEGMENTO) %>%
  summarise(total_ninos = sum(Ninos, na.rm = TRUE))

print(resumen_segmentos)
num_segmentos_con_ninos <- data_com_ninos %>%
  summarise(num_segmentos = n_distinct(SEGMENTO)) %>%
  pull(num_segmentos)

print(num_segmentos_con_ninos)


