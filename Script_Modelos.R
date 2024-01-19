## ----setup, include=FALSE------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ---- warning = FALSE----------------------------------------------------------------------------------------------------
source("Script_Variables.R")


## ------------------------------------------------------------------------------------------------------------------------
p_load(stargazer, car, tidyr, knitr) #Añadimos las librerías que vamos a usar a mayores que las vistas en el Script 1.


## ------------------------------------------------------------------------------------------------------------------------
purl("Script_Modelos.Rmd", output = "Script_Modelos.R")


## ------------------------------------------------------------------------------------------------------------------------
modelo1 <- lm(glob_index ~ cult_div, data = df_models) #Creamos el Modelo 1 donde establecemos una regresión lineal con el índice de globalización como variable dependiente y la fragmentación cultural como variable dependiente.

summary(modelo1) #Visualizamos: podemos rechazar H0 y aceptar que hay una relación significativa (con un nivel de confianza del 99.9%) y negativa.


## ------------------------------------------------------------------------------------------------------------------------
modelo2 <- lm(glob_index ~ cult_div + estabilidad + demo + derechos, data = df_models) #No visualizamos todos los modelos porque se hará posteriormente de forma conjunta; optimizamos espacio y facilitamos la lectura.


## ------------------------------------------------------------------------------------------------------------------------
modelo3 <- lm(glob_index ~ cult_div + estabilidad + demo + derechos + cgini + idh + logistics_quality + trade_gdp_percent , data = df_models)


## ------------------------------------------------------------------------------------------------------------------------
modelo4 <- lm(glob_index ~ cult_div + estabilidad + demo + derechos + cgini + logistics_quality + trade_gdp_percent * idh, data = df_models)


## ------------------------------------------------------------------------------------------------------------------------
stargazer(modelo1, modelo2, modelo3, modelo4, #Definimos los modelos que queremos que aparezcan
          type = "text",
          style = "apsr", #Elegimos el estilo académico de la Political Science Review
          column.labels = c("Modelo 1", "Modelo 2", "Modelo 3", "Modelo 4"),
          dep.var.labels = c("Índice de globalización de KOF"), #Nombramos la V.D
          covariate.labels = c("Diversidad cultural", "Estabilidad política: Inestable", "Existencia de democracia: No.", "Derechos políticos", "Coef. Gini", "IDH: Bajo", "Calidad de la logística", "% PIB Comercio * IDH Bajo", "% PIB Comercio", "Constante")) #Nombramos las etiquetas.


## ------------------------------------------------------------------------------------------------------------------------
VIF_Modelo2 <- vif(modelo2) #Para el Modelo 1 no es necesario calcular estos valores (solo hay 1 variable explicativa)
print(VIF_Modelo2)  #Visualizamos: valores menores de 5 no deberían preocuparnos.


## ------------------------------------------------------------------------------------------------------------------------
VIF_Modelo3 <- vif(modelo3) #La variable derechos políticos darían algunos pequeños problemas de multicolinealidad.
print(VIF_Modelo3)


## ---- warning = FALSE----------------------------------------------------------------------------------------------------
VIF_Modelo4 <- vif(modelo4)
print(VIF_Modelo4)


## ------------------------------------------------------------------------------------------------------------------------
p_load(gvlma)  #Añadimos la librería para comprobar el modelo.
modelo2_hipo <- gvlma(modelo2)
summary(modelo2_hipo)


## ------------------------------------------------------------------------------------------------------------------------
modelo3_hipo <- gvlma(modelo3)
summary(modelo3_hipo)


## ------------------------------------------------------------------------------------------------------------------------
df_ts <- read_dta("qog_std_ts_jan23_stata14.dta") #Cargamos datos


## ------------------------------------------------------------------------------------------------------------------------
df_ts <- df_ts |> 
 rename(glob_index = dr_ig) |> 
 select(cname, year, glob_index, ccodealp) |> 
 filter(glob_index != "NA")
 
head(df_ts)


## ------------------------------------------------------------------------------------------------------------------------
df_ts <- df_ts |> 
 pivot_wider(names_from = year, values_from = glob_index) #Cambiamos la unidad de observación de año-país a país

filter(df_ts, cname == "Spain" | cname == "France") #Visualizamos los datos en formato correcto para España y Francia


## ---- warning= FALSE-----------------------------------------------------------------------------------------------------
#Con el paquete knitr podemos hacer una tabla estética

df_descriptivos <- df_models |> 
 mutate(estabilidad = as.numeric(estabilidad)) |> #Para ver los descriptivos, primero cambiamos a numéricas las variables que no lo son
 mutate(demo = as.numeric(demo)) |> 
 mutate(idh = as.numeric(idh))

descriptivos_table <- sapply(df_descriptivos, function(x) c(Mínimo = min(x), Máximo = max(x), Media = mean(x), Mediana = median(x), SD = sd(x))) #Ajustamos los valores deseados, mínimo, máximo, media, etc.

descriptivos_table <- t(descriptivos_table)

descriptives_table1 <- kable(descriptivos_table, format = "html", digits = 3)

descriptives_table1

