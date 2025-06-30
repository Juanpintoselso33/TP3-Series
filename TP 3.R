
## TP 3 series de tiempo:##################
library(readxl)
library(tseries)
library(urca)
library(vars)
library(dplyr)
library(lubridate)
library(flextable)
library(officer)
library(tsDyn)
library(reshape2)
library(devtools)
source_url("https://raw.githubusercontent.com/anguyen1210/var-tools/master/R/extract_varirf.R")
library(patchwork)
library(extrafont)
library(bootUR)
library(patchwork)
library(svglite)
library(forecast)
library(ggplot2)
library(strucchange)



dire<-"G:/Mi unidad/TP 2 y 3 series de tiempo"
setwd(dire)

comentario<-function(...){
  invisible(NULL)
}

##1.0 Preparación de base datos####

file_name <- "Base TP2 SDT.xlsx"

# Opción 1: Path completo (cambia esta ruta por la tuya)
excel_file <- "C:/Users/trico/OneDrive/UBA/Series de tiempo/TP2/Base TP2 SDT.xlsx"

# Opción 2: Si no funciona, probar buscar automáticamente
if(!file.exists(excel_file)) {
  cat("Buscando archivo Excel...\n")
  cat("Directorio actual:", getwd(), "\n")
  
  # Buscar recursivamente en todos los directorios
  excel_files <- list.files(pattern = "Base.*TP2.*SDT.*xlsx", recursive = TRUE, full.names = TRUE)
  
  if(length(excel_files) > 0) {
    excel_file <- excel_files[1]
    cat("Archivo encontrado:", excel_file, "\n")
  } else {
    # Buscar cualquier archivo Excel como último recurso
    all_excel <- list.files(pattern = "*.xlsx", recursive = TRUE, full.names = TRUE)
    if(length(all_excel) > 0) {
      cat("Archivos Excel disponibles:\n")
      print(all_excel)
      excel_file <- all_excel[1]
      cat("Usando:", excel_file, "\n")
    } else {
      stop("No se encontró ningún archivo Excel. Asegurate de estar en el directorio correcto.")
    }
  }
}

# Cargar y limpiar datos principales de Argentina
datos_argentina <- read_excel(excel_file, sheet = "DATOS PARA TP2", skip = 1)

# Limpiar nombres de columnas
nombres_columnas <- c("PERIODO", "ITCRM", "PBI_ARG", "IMPORTACIONES", "DEMANDA", 
                      "EXPORTACIONES", "Brasil", "Canada", "Chile", "Estados_Unidos", 
                      "Mexico", "Uruguay", "China", "India", "Japon", "Reino_Unido", 
                      "Suiza", "Zona_Euro", "Vietnam", "Otros", "Total_Ponderadores")

# Ajustar nombres si hay diferencias en el número de columnas
if(ncol(datos_argentina) < length(nombres_columnas)) {
  nombres_columnas <- nombres_columnas[1:ncol(datos_argentina)]
} else if(ncol(datos_argentina) > length(nombres_columnas)) {
  nombres_extra <- paste0("Col_", (length(nombres_columnas)+1):ncol(datos_argentina))
  nombres_columnas <- c(nombres_columnas, nombres_extra)
}

names(datos_argentina) <- nombres_columnas

# Filtrar solo filas con datos
datos_argentina <- datos_argentina %>%
  filter(!is.na(PERIODO) & PERIODO != "")

# Cargar PIB de socios comerciales
pib_socios <- read_excel(excel_file, sheet = "PBI socios")

# Usar ponderadores fijos representativos del comercio exterior argentino
ponderadores_bcra <- data.frame(
  pais = c("Brasil", "China", "Estados Unidos", "Zona Euro", "México", 
           "Chile", "Canadá", "Uruguay", "Japón", "India", "Reino Unido", 
           "Suiza", "Vietnam"),
  peso = c(20.5, 18.2, 16.8, 12.5, 4.1, 3.8, 3.2, 2.1, 2.0, 1.8, 1.5, 1.2, 1.0)
)

cat("Ponderadores fijos del BCRA utilizados:\n")
print(ponderadores_bcra)

# Consolidar dataset final
dataset_final <- data.frame(
  PERIODO = datos_argentina$PERIODO,
  ITCRM = as.numeric(datos_argentina$ITCRM),
  PBI_ARG = as.numeric(datos_argentina$PBI_ARG), 
  IMPORTACIONES = as.numeric(datos_argentina$IMPORTACIONES),
  EXPORTACIONES = as.numeric(datos_argentina$EXPORTACIONES)
)

# Crear fechas trimestrales
dataset_final$Year <- as.numeric(paste0("20", substr(dataset_final$PERIODO, nchar(dataset_final$PERIODO)-1, nchar(dataset_final$PERIODO))))
dataset_final$Quarter <- ifelse(substr(dataset_final$PERIODO, 1, 1) == "I", 1,
                                ifelse(substr(dataset_final$PERIODO, 1, 2) == "II", 2,
                                       ifelse(substr(dataset_final$PERIODO, 1, 3) == "III", 3,
                                              ifelse(substr(dataset_final$PERIODO, 1, 2) == "IV", 4, NA))))
dataset_final$Date <- as.Date(paste0(dataset_final$Year, "-", (dataset_final$Quarter-1)*3 + 1, "-01"))

# Filtrar períodos comunes y calcular PIB ponderado
periodos_comunes <- intersect(dataset_final$PERIODO, pib_socios$PERIODO)
pib_socios_clean <- pib_socios[pib_socios$PERIODO %in% periodos_comunes, ]
dataset_temp <- dataset_final[dataset_final$PERIODO %in% periodos_comunes, ]

# Calcular PIB ponderado
pib_ponderado <- numeric(nrow(pib_socios_clean))

for(i in 1:nrow(pib_socios_clean)) {
  pib_periodo <- 0
  peso_total_usado <- 0
  
  for(j in 1:nrow(ponderadores_bcra)) {
    pais <- ponderadores_bcra$pais[j]
    peso <- ponderadores_bcra$peso[j]
    
    # Mapear nombre del país a la columna correcta
    nombre_columna <- case_when(
      pais == "Estados Unidos" ~ "Estados Unidos",
      pais == "Zona Euro" ~ "Zona Euro",
      pais == "México" ~ "México", 
      pais == "Canadá" ~ "Canadá",
      pais == "Japón" ~ "Japón",
      pais == "Reino Unido" ~ "Reino Unido",
      TRUE ~ pais
    )
    
    if(nombre_columna %in% names(pib_socios_clean)) {
      pib_pais <- as.numeric(pib_socios_clean[i, nombre_columna])
      
      if(!is.na(pib_pais) && pib_pais > 0) {
        pib_periodo <- pib_periodo + (peso * pib_pais / 100)
        peso_total_usado <- peso_total_usado + peso
      }
    }
  }
  
  if(peso_total_usado > 0) {
    pib_ponderado[i] <- pib_periodo * (100 / peso_total_usado)
  } else {
    pib_ponderado[i] <- NA
  }
}

# Dataset final consolidado
dataset_final <- data.frame(
  PERIODO = dataset_temp$PERIODO,
  Year = dataset_temp$Year,
  Quarter = dataset_temp$Quarter,
  Date = dataset_temp$Date,
  PIB_ARGENTINA = dataset_temp$PBI_ARG,
  IMPORTACIONES = dataset_temp$IMPORTACIONES,
  EXPORTACIONES = dataset_temp$EXPORTACIONES,
  TCR_MULTILATERAL = dataset_temp$ITCRM,
  PIB_SOCIOS_PONDERADO = pib_ponderado
)

# Filtrar filas completas
dataset_final <- dataset_final[complete.cases(dataset_final), ]

cat("Dataset final - Dimensiones:", dim(dataset_final), "\n")

##2.0 Punto 1 del tp3 ####
comentario("Grafique las series y realice un análisis estadístico para determinar la existencia de raíces 
unitarias, el orden de integración, y la presencia de estacionalidad. Use las series en 
logaritmos naturales.  ")

###2.1 Convertir a logaritmos naturales#####
dataset_log <- dataset_final
dataset_log$log_PIB_ARG <- log(dataset_final$PIB_ARGENTINA)
dataset_log$log_IMPORTACIONES <- log(dataset_final$IMPORTACIONES)
dataset_log$log_EXPORTACIONES <- log(dataset_final$EXPORTACIONES)
dataset_log$log_TCR <- log(dataset_final$TCR_MULTILATERAL)
dataset_log$log_PIB_SOCIOS <- log(dataset_final$PIB_SOCIOS_PONDERADO)


##Funcion para generar gráficos:#####


graficossb<-function(serie_ts,titulo){
  autoplot(serie_ts, color="darkred", alpha=0.8)+
    labs(y=titulo, x="")+
    papaja::theme_apa()+
    theme(text=element_text(family="Times New Roman"),
          axis.title.y = element_text(family = "Times New Roman", face="bold"))+
    annotate('rect',
             xmin=2020+ 1/4,
             xmax=2021+ 4/4,
             ymin = -Inf, ymax=Inf,
             alpha=0.2, fill='darkgray')+
    annotate('rect',
             xmin=2012+ 1/4,
             xmax=2013+ 1/4,
             ymin = -Inf, ymax=Inf,
             alpha=0.2, fill='darkgray')+
    annotate('rect',
             xmin=2007+ 1/4,
             xmax=2008+ 4/4,
             ymin = -Inf, ymax=Inf,
             alpha=0.2, fill='darkgray')+
    annotate('rect',
             xmin=2018+ 2/4,
             xmax=2018+ 3/4,
             ymin = -Inf, ymax=Inf,
             alpha=0.2, fill='darkgray')+
    annotate('rect',
             xmin=2023+ 4/4,
             xmax=2024+ 2/4,
             ymin = -Inf, ymax=Inf,
             alpha=0.2, fill='darkgray')
}


y3<-ts(dataset_log$log_PIB_ARG, start=c(2004,1,1), frequency=4)
y2<-ts(dataset_log$log_IMPORTACIONES, start=c(2004,1,1), frequency = 4)
y1<-ts(dataset_log$log_EXPORTACIONES, start=c(2004, 1,1 ), frequency=4)
y4<-ts(dataset_log$log_PIB_SOCIOS, start=c(2004,1,1 ), frequency = 4)
y5<-ts(dataset_log$log_TCR, start=c(2004, 1,1), frequency = 4)


View(dataset_log)
g1<-graficossb(y1, "Log(Exportaciones)")
g2<-graficossb(y2, "Log(Importaciones)")
g3<-graficossb(y3, "Log(PIB Argentina)")
g4<-graficossb(y4, "Log(PIB socios)")
g5<-graficossb(y5, "Log(TCRM)")


gfinal<-(g1|g2)/
        (g3|g4)/
         (g5)  


gfinal

ggsave("grafico1_tp3.svg",
       dpi=300,
       plot=gfinal,
       width = 2180, heigh=1860,
       unit="px")

## Pruebas de raíz unitaria####

# Función para realizar tests de raíz unitaria
unit_root_tests <- function(series, name, max_lags = 8) {
  cat("\n=== TESTS DE RAÍZ UNITARIA:", name, "===\n")
  
  # Test ADF (Augmented Dickey-Fuller)
  cat("\n1. Test ADF:\n")
  
  # ADF con constante
  adf_const <- ur.df(series, type = "drift", lags = max_lags, selectlags = "AIC")
  cat("   ADF con constante - Estadístico:", adf_const@teststat[1], "\n")
  cat("   Valor crítico 1%:", adf_const@cval[1,1], "\n")
  cat("   Valor crítico 5%:", adf_const@cval[1,2], "\n")
  
  # ADF con constante y tendencia
  adf_trend <- ur.df(series, type = "trend", lags = max_lags, selectlags = "AIC")
  cat("   ADF con tendencia - Estadístico:", adf_trend@teststat[1], "\n")
  cat("   Valor crítico 1%:", adf_trend@cval[1,1], "\n")
  cat("   Valor crítico 5%:", adf_trend@cval[1,2], "\n")
  
  # Test KPSS
  cat("\n2. Test KPSS:\n")
  kpss_level <- ur.kpss(series, type = "mu")
  kpss_trend <- ur.kpss(series, type = "tau")
  cat("   KPSS nivel - Estadístico:", kpss_level@teststat, "\n")
  cat("   KPSS tendencia - Estadístico:", kpss_trend@teststat, "\n")
  
  # Test Phillips-Perron
  cat("\n3. Test Phillips-Perron:\n")
  pp_test <- ur.pp(series, type = "Z-tau", model = "trend")
  cat("   PP - Estadístico:", pp_test@teststat, "\n")
  cat("   Valor crítico 5%:", pp_test@cval[2], "\n")
  
  return(list(adf_const = adf_const, adf_trend = adf_trend, 
              kpss_level = kpss_level, kpss_trend = kpss_trend, pp = pp_test))
}


mlist<-list(y1,y2,y3, y4, y5)
res_est<-list()
res_estt<-list()
for (i in seq_along(mlist)){
res_est[[i]]<-ur.df(mlist[[i]], type = "drift", lags = 8, selectlags = "AIC")
res_estt[[i]]<-ur.df(mlist[[i]], type = "trend", lags = 8, selectlags = "AIC")
}

nombres<-c("EXP", "IMP", "PIB ARG", "PIB SOC", "TCRM")

tabla_drift <- data.frame(
  var = nombres,
  tau2 = sapply(res_est, function(x) round(x@teststat[1], 3)),
  lag = sapply(res_est, function(x) x@lags),
  c1 = sapply(res_est, function(x) round(x@cval[1, 1], 3)),
  c5 = sapply(res_est, function(x) round(x@cval[1, 2], 3)),
  c10 = sapply(res_est, function(x) round(x@cval[1, 3], 3))
)
c<-c("Variable", "T-Stats","Rezagos",  "1 %", "5 %", "10 %")
colnames(tabla_drift)<-c
print(tabla_drift)


tabla_trend <- data.frame(
  var = nombres,
  tau2 = sapply(res_estt, function(x) round(x@teststat[1], 3)),
  lag = sapply(res_estt, function(x) x@lags),
  c1 = sapply(res_estt, function(x) round(x@cval[1, 1], 3)),
  c5 = sapply(res_estt, function(x) round(x@cval[1, 2], 3)),
  c10 = sapply(res_estt, function(x) round(x@cval[1, 3], 3))
)


colnames(tabla_trend)<-c



doc<-read_docx() %>% 
  body_add_flextable(value=qflextable(tabla_drift)) %>% 
  body_add_par("Nota:Elaboración propia por los autor")

print(doc, target="Dickey_fuller_drift.docx")



doc<-read_docx() %>% 
  body_add_flextable(value=qflextable(tabla_trend)) %>% 
  body_add_par("Nota:Elaboración propia por los autor")

print(doc, target="Dickey_fuller_trend.docx")

### Generando series diferenciadas:####

for (var in vars) {
  serie_original <- get(var)
  nueva_serie <- ts(diff(serie_original), start = start(serie_original),
                    frequency = frequency(serie_original))
  assign(paste0("d", var), nueva_serie)
}


### Tablas#####
dtabla_drift <- data.frame(
  var = nombres,
  tau2 = sapply(dres_est, function(x) round(x@teststat[1], 3)),
  lag = sapply(dres_est, function(x) x@lags),
  c1 = sapply(dres_est, function(x) round(x@cval[1, 1], 3)),
  c5 = sapply(dres_est, function(x) round(x@cval[1, 2], 3)),
  c10 = sapply(dres_est, function(x) round(x@cval[1, 3], 3))
)




### análisisi diferenciada:####

dmlist<-list(dy1,dy2,dy3, dy4, dy5)
dres_est<-list()
dres_estt<-list()
for (i in seq_along(dmlist)){
  dres_est[[i]]<-ur.df(mlist[[i]], type = "drift", lags = 8, selectlags = "AIC")
  dres_estt[[i]]<-ur.df(mlist[[i]], type = "trend", lags = 8, selectlags = "AIC")
}





dtabla_drift <- data.frame(
  var = nombres,
  tau2 = sapply(dres_est, function(x) round(x@teststat[1], 3)),
  lag = sapply(dres_est, function(x) x@lags),
  c1 = sapply(dres_est, function(x) round(x@cval[1, 1], 3)),
  c5 = sapply(dres_est, function(x) round(x@cval[1, 2], 3)),
  c10 = sapply(dres_est, function(x) round(x@cval[1, 3], 3))
)

colnames(dtabla_drift)<-c

doc<-read_docx() %>% 
  body_add_flextable(value=qflextable(dtabla_drift)) %>% 
  body_add_par("Nota:Elaboración propia por los autor")

print(doc, target="Dickey_fuller_drift_diff.docx")

##3.0 Modelización de estacionalidad.


