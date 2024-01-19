## -----------------------------------------------------------------------------
library(pacman)
p_load(tidyverse, dplyr, forcats, haven, stargazer, readxl, tidyr, car, knitr, rstudioapi)


##-----------------------------------------------------------------------------
df <- read_dta("qog_std_cs_jan23_stata14.dta") #Usando la Cross-Section del QoG

setTheme("Cobalt")

## -----------------------------------------------------------------------------
df <- df |>
 rename(glob_index = dr_ig)#Recodificamos con la función rename de dplyr

summary(df$glob_index) #Visualizamos si la variable está correctamente especificada

#Podríamos eliminar los casos perdidos ahora, pero se hará posteriormente de forma más óptima.


## -----------------------------------------------------------------------------
df <- df |> 
 rename(cult_div = fe_cultdiv) #Repetimos el proceso

summary(df$cult_div) #Visualizamos.


## -----------------------------------------------------------------------------
 df <- df |> 
 rename(estabilidad = wbgi_pve) |> 
 mutate(estabilidad = #Con mutate cambiamos los valores de nuestra variable
         case_when(estabilidad >=0 ~ "Estable", #Donde valores positivos implican una política estable
                   estabilidad < 0 ~ "Inestable", #Y valores negativos lo contrario: inestabilidad. 
                   estabilidad == "NA" ~ NA)) |>
 mutate(estabilidad = factor(estabilidad))
 
table(df$estabilidad) #Visualizamos, esta vez con table(), que se ha ajustado bien la variable.



## -----------------------------------------------------------------------------
df <- df |> 
  rename(demo = bmr_dem) |> 
  mutate(demo = case_when(
    demo == 0 ~ "No democrático",
    demo == 1 ~ "Democrático"
  ))


## -----------------------------------------------------------------------------
df <- df |> 
  mutate(derechos = abs(fh_pr-8) #Invertimos el orden (restando 8 y calculando en valores absolutos), para que un mayor número implique más derechos y el análisis posterior sea más sencillo
  )

summary(df$derechos)


## -----------------------------------------------------------------------------
df <- df |> 
 rename(cgini = wdi_gini)
  summary(df$cgini)


## -----------------------------------------------------------------------------
df <- df |> 
 rename(gdpxcap = wdi_gdpcapcur)

summary(df$gdpxcap)



## -----------------------------------------------------------------------------
summary(df$undp_hdi) #Siendo 0,74 la mediana para esta variable, podemos dividir en dos a los países
df <- df |> 
 rename(idh = undp_hdi) |> 
 mutate(idh = case_when(idh < 0.74 ~ "Bajo", #Los valores inferiores a esta media se consideran IDH Bajo
                        idh >= 0.74 ~ "Alto")) #Y por encima, IDH alto.

table(df$idh)


## -----------------------------------------------------------------------------
df_logistics <- read_xlsx("df_logistics.xlsx") #Cargamos nuestro EXCEL.


## -----------------------------------------------------------------------------
df_merged <- right_join(df, df_logistics, by = "ccodealp") #Usamos el código del país como referencia para la unión de los datos, pues los nombre de algunos países usando "cnames" es distinto en la QoG y en los datos del Banco Mundial.


## ---- warning= FALSE----------------------------------------------------------
df_merged <- df_merged |> 
 rename(trade_gdp_percent = `trade % gdp`) |> 
 mutate(trade_gdp_percent = as.numeric(trade_gdp_percent)) #Se transforma en variable numérica.


## ---- warning= FALSE----------------------------------------------------------
df_merged <- df_merged |> 
 mutate(logistics_quality = as.numeric(logistics_quality)) #Se transforma en variable numérica.



## -----------------------------------------------------------------------------
myvars <- c("glob_index", "cult_div", "estabilidad", "demo", "derechos", "cgini", "idh", "logistics_quality", "trade_gdp_percent")  #Agrupamos las variables que deseamos dentro de los modelos.
df_models<-df_merged[myvars] #Creamos un nuevo Data Frame con ellas
df_models<- na.omit(df_models) #Omitimos los casos perdidos con na.omit()

