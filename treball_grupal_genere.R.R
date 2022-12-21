library(tidyverse)
library(wbstats)
library(ggplot2)
library(gganimate)
library(rnaturalearth)
library(rnaturalearthdata)
library(wordcloud2)
library(gt)

gender <- wbsearch(pattern = "gender", field = "indicator")

escolarizacion <- wb(indicator = c("SE.ENR.PRIM.FM.ZS", "SE.ENR.SECO.FM.ZS", "SE.ENR.TERT.FM.ZS"))

escol1 <- escolarizacion %>% filter(country == "Spain"|
                                      country == "France"|
                                      country == "Italy"|
                                      country == "United Kingdom"|
                                      country == "China") %>% filter(date >= 2000)


#gráfico IPG primaria
prim <- escol1 %>% filter (indicatorID == "SE.ENR.PRIM.FM.ZS")

primgg <- ggplot(prim, aes(x = date, y = value, color =country)) + geom_point() + geom_line(aes(group = country)) + labs(title = "ÍNDICE DE PARIDAD DE GÉNERO", subtitle = "ESCOLARIZACIÓN PRIMARIA") + scale_x_discrete(breaks = c("1990","1995", "2000", "2005", "2010", "2015"))

primgg



#gráfico IPG secundaria
sec <- escol1 %>% filter (indicatorID == "SE.ENR.SECO.FM.ZS")

secgg <- ggplot(sec, aes(x = date, y = value, color =country)) + geom_point() + geom_line(aes(group = country)) + labs(title = "ÍNDICE DE PARIDAD DE GÉNERO", subtitle = "ESCOLARIZACIÓN SECUNDARIA") + scale_x_discrete(breaks = c("1990","1995", "2000", "2005", "2010", "2015"))

secgg



#gráfico IPG terciaria
terc <- escol1 %>% filter (indicatorID == "SE.ENR.TERT.FM.ZS")

tercgg <- ggplot(terc, aes(x = date, y = value, color = country)) + geom_point() + geom_line(aes(group = country)) + labs(title = "ÍNDICE DE PARIDAD DE GÉNERO", subtitle = "ESCOLARIZACIÓN TERCIARIA") + scale_x_discrete(breaks = c("1990","1995", "2000", "2005", "2010", "2015"))

tercgg


#gráfico escolarizaciones juntas, por países
escol_nivel <- wb(country = c( "ESP", "FRA", "GBR", "ITA"), 
                  indicator =c("SE.ENR.PRIM.FM.ZS", "SE.ENR.SECO.FM.ZS", "SE.ENR.TERT.FM.ZS"),startdate = 2000, enddate = 2016, POSIXct = TRUE)

por_paises <- ggplot(escol_nivel, aes(x = date_ct ,y = value, color = indicatorID, group = indicatorID)) + geom_point() + geom_line() + theme(panel.background = element_rect(fill = "gray28",colour = "white"), plot.background = element_rect(fill = "aliceblue",colour = "bisque")) +labs(title = "ÍNDICE DE PARIDAD DE GÉNERO SEGÚN EL NIVEL DE ESTUDIOS", "Fecha", y = "valor", colour = "Nivel de Estudios") + theme(panel.background = element_rect(fill = "antiquewhite")) +facet_wrap(~country) +  scale_color_discrete(name = "indicatorID", labels = c("primaria", "secundaria" , "terciaria")) 

por_paises


#Proporción de mujeres empresarias por países (más oscuro menos mujeres)

mujeres <- wbsearch(pattern = "employment", field = "indicator")

porcentaje <- wb(indicator = c("SL.EMP.MPYR.FE.ZS"))

mujempresarias <- porcentaje %>% 
  select(date, value, country, iso3c) %>% 
  mutate(date = as.numeric(date)) %>%  
  filter(date == 2019) %>%
  mutate(proporción = ntile 
         (value, 4))


world <- ne_countries(scale = "medium", returnclass = "sf")


world <- world %>% filter(subregion != "Antarctica") %>% filter(admin != "Greenland")
aa <- ggplot() + geom_sf(data = world) + theme_void()
world <- world %>% select(name, iso_a3, geometry)


mujempresarias2 <- left_join(mujempresarias, world, by = c("iso3c" = "iso_a3"))


mujempresariasgg <- mujempresarias2 %>% 
  ggplot + geom_sf (data = mujempresarias2, aes(geometry = geometry , fill = proporción)) + scale_fill_viridis_c(option = "plasma") + labs(title = "MUJERES TRABAJADORAS QUE SON EMPRESARIAS", subtitle = "en porcentajes") 

mujempresariasgg


#otra forma de visualizar los datos de % de mujeres empresarias
nombrespaises<- mujempresarias %>% group_by(country) %>% summarise(value=sum(value))%>% ungroup() %>% arrange(desc(value))

nombrespaises <- nombrespaises %>% mutate(word = country, freq = value)
nombrespaises <- nombrespaises %>% select(word,freq)
wordcloud2(data = nombrespaises, size = 0.2)





#centrándonos en España, porcentaje de salario que cobra una mujer respecto un hombre, en el sector público y privado

gdp1 <- wbsearch(pattern = "wage", field = "indicator")
porcSALARIO <- wb(indicator = c("BI.WAG.PRVS.FM.SM", "BI.WAG.PUBS.FM.SM"))
porcSALARIO1 <- porcSALARIO %>% filter(country == "Spain")


porcSALARIOgg <- ggplot(porcSALARIO1, aes(x = date, y = value, color = indicatorID)) + 
  geom_point() + geom_line(aes(group = indicatorID)) + labs(title = "SALARIO DE LAS MUJERES RESPECTO EL DE LOS HOMBRES, POR SECTORES", subtitle = "en porcentajes") + 
  scale_color_discrete(name = "indicatorID", labels = c("sector privado", "sector público"))



porcSALARIOgg 


#- ahora, por países (los mismos que antes), vamos a calcular la diferencia (en euros) entre los salarios de hombres y mujeres, actualizado a tiempos actuales, sin centrarnos en ningún sector en concreto

#tal como lo ha hecho el profesor en clase


my_url <- "https://raw.githubusercontent.com/perezp44/iris_data/master/data/PIAAC_data_small.csv"
df_original <- read_csv(my_url)

salarios <- df_original %>% select(Country, Gender, Education, Wage_month, Wage_hour, Numeracy_score)


tablasalarios <- salarios %>% 
  group_by(Country, Gender) %>% 
  summarise(W_mes_medio = mean(Wage_month, na.rm = TRUE)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = Gender, values_from = W_mes_medio) %>% 
  mutate(dif_W = Male-Female, dif_percent_W = dif_W/Female) 

tablasalarios$dif_percent_W <- round(tablasalarios$dif_percent_W, digits=3)

banderas <- c("https://www.comprarbanderas.es/images/banderas/400/60-espana-sin-escudo_400px.jpg",
              "https://upload.wikimedia.org/wikipedia/commons/c/c3/Flag_of_France.svg",
              "https://www.banderas-mundo.es/data/flags/w1600/gb.png" , "https://media.istockphoto.com/id/1063640060/es/vector/vector-bandera-de-italia.jpg?s=612x612&w=0&k=20&c=gZUTN3jEmTjiqIJWbW6oOZjcH55MzsjsLMBs74-R1Lo=")

tablasalarios1 <- cbind(tablasalarios, banderas)

tablasalarios1 <- tablasalarios1 %>% gt()

tablasalarios1 %>% gt::text_transform(locations = cells_body(columns = c(banderas)), fn = function(x){gt::web_image(x, height = 25)})

#otra forma de visualizar los datos

graficodif <- ggplot(tablasalarios, aes(x="", y = dif_percent_W, fill=Country)) +     geom_bar(stat ="identity", color = "black") + 
  geom_text(aes(label = dif_percent_W), position = position_stack(vjust=0.6), color="black", size = 2.5, ) + 
  coord_polar(theta = "y")  + 
  theme_void() +
  labs(title = "% DIFERENCIA DE SALARIOS")

graficodif + theme(panel.grid.major = element_line(linetype = "blank")) +labs(fill = "PAÍSES") 




# Proporción de tiempo dedicado al trabajo doméstico y de cuidados no remunerado (h/dia)

trabajo <- wbsearch(pattern = "work", field = "indicator")

trabajodoméstico <- wb(indicator = c("SG.TIM.UWRK.MA", "SG.TIM.UWRK.FE"))

trabajodoméstico1 <- trabajodoméstico %>% filter(country == "Spain") %>% filter(date >= 2000)

trabdom <- ggplot(trabajodoméstico1, aes(date, value, fill = indicatorID)) +
  geom_col()  +  
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.ontop = TRUE) + labs(title = "Proporción de horas dedicadas al trabajo doméstico", subtitle = "h al dia") +
  scale_fill_discrete(name = "HORAS DIARIAS", labels = c("mujeres", "hombres")) 


trabdom



# Mujeres y hombres que trabajan por cuenta propia

cuentapropia <- wb(indicator = c("SL.EMP.OWAC.FE.ZS", "SL.EMP.OWAC.MA.ZS"), country = c( "ESP", "FRA", "GBR", "ITA"))


cuentapropiagg <- ggplot(cuentapropia, aes(x = date , y = value, color = indicatorID, group = indicatorID)) + geom_point() + geom_line() + theme(panel.background = element_rect(fill = "gray28",
                                                                                                                                                                                 colour = "white"), plot.background = element_rect(fill = "aliceblue",
                                                                                                                                                                                                                                   colour = "bisque")) +labs(title = "TRABAJADORES POR CUENTA PROPIA", subtitle = "en miles", "Fecha", y = "valor") + theme(panel.background = element_rect(fill = "antiquewhite")) +
  facet_wrap(~country) +
  scale_color_discrete(name = "cantidad", labels = c("mujeres", "hombres")) + 
  scale_x_discrete(breaks = c("1990","1995", "2000", "2005", "2010", "2015"))




cuentapropiagg



#días que se requieren para iniciar un negocio, en el caso de las mujeres

iniciarnegocio <- wb(indicator = c("IC.REG.DURS.FE"))

iniciarnegocio0 <- iniciarnegocio %>% filter(country == "Spain"|
                                               country == "France"|
                                               country == "Italy"|
                                               country == "United Kingdom"|
                                               country == "China") %>% filter(date == 2019)


inicionegocio <- ggplot() +
  geom_col(data = iniciarnegocio0, aes(x = reorder(indicatorID, value), y = value, fill = country), position = "dodge") +
  scale_fill_brewer(palette = "Greens") + 
  theme(axis.text.x = element_text(colour = "black"),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white")) +
  labs(title = "Tiempo requerido para iniciar un negocio", x = NULL, y = NULL, caption = "Fuente: Elaboración propia con datos del Banco Mundial") +
  labs(y = "número de días") +
  labs(x = "mujeres") +
  theme(legend.position = "bottom", legend.direction = "horizontal")


inicionegocio


#Otra forma de visualizar los datos.

graficodif <- ggplot(iniciarnegocio0, aes(x="", y = value, fill=country)) +   geom_bar(stat ="identity", color = "black") + 
  geom_text(aes(label = value), position = position_stack(vjust=0.6), color="black", size = 2.5, ) + 
  coord_polar(theta = "y")  + 
  theme_void() +
  labs(title = "DÍAS REQUERIDOS PARA INICIAR UN NEGOCIO",subtitle = "En el caso de las mujeres")

graficodif + theme(panel.grid.major = element_line(linetype = "blank")) +labs(fill = "PAÍSES")


#tasa de actividad
my_url <- "https://www.ine.es/jaxi/files/tpx/es/csv_bd/20486.csv?nocab=1"
curl::curl_download(my_url, "./datos/tasaactividad-genero.csv")
df <- rio::import("./datos/tasaactividad-genero.csv")
df <- janitor::clean_names(df)
df <- df %>% mutate(total = stringr::str_replace(total, "," , "." ))
str(df)
df_orig <- df 
df_act1 <- df_orig %>%
  filter(periodo >= 2015) %>%
  filter(sexo == "Mujeres"|
           sexo == "Varones") %>%
  filter(grupos_de_edad == "entre los 25 y 29 años")

actjoven <- ggplot(df_act1, aes(x = periodo, y = total, color = sexo)) + geom_point() + geom_line(aes(group = sexo)) + labs(title = "Tasa de actividad", subtitle = "de hombres y mujeres de entre los 25 y 29 años") + scale_y_discrete(breaks = c(86.17, 87.06, 87.29, 87.47, 87.59))

actjoven

df_act2 <- df_orig %>%
  filter(periodo >= 2015) %>%
  filter(sexo == "Mujeres"|
           sexo == "Varones") %>%
  filter(grupos_de_edad == "entre los 30 y 34 años")

actjoven2 <- ggplot(df_act2, aes(x = periodo, y = total, color = sexo)) + geom_point() + geom_line(aes(group = sexo)) + labs(title = "Tasa de actividad", subtitle = "de hombres y mujeres de entre los 30 y 34 años") + scale_y_discrete(breaks = c(90.31, 92.13, 93.82, 93.87, 94.04))

actjoven2


#Tasa de paro de hombres y mujeres, en porcentajes
my_url <- "https://www.ine.es/jaxiT3/files/t/es/csv_bdsc/4887.csv?nocab=1"
curl::curl_download(my_url, "./datos/epa_genero-edad.csv")
df <- rio::import("./datos/epa_genero-edad.csv")
df <- janitor::clean_names(df) 
df <- df %>% mutate(total = stringr::str_replace(total, "," , "." ))
df <- df %>% rename(tasa_paro = total)
df <- df %>% mutate(edad.f = as_factor(edad), .after = edad)
df <- df %>%  mutate(edad.f = forcats::fct_relevel(edad.f, "Total", after = Inf))


# 2010
df <- df %>% filter(sexo != "Ambos sexos")
df_2010 <- df %>% filter(edad == "Total")
df_2010 <- df_2010 %>% filter(periodo == 2010)


p10 <- ggplot(df_2010, aes(x = sexo, y = tasa_paro )) + geom_col(fill = "lightcoral") + coord_flip() + labs(title = "TASA DE PARO 2010", subtitle = "en porcentajes") 

p10


#2015
df <- df %>% filter(sexo != "Ambos sexos")
df_2015 <- df %>% filter(edad == "Total")
df_2015 <- df_2015 %>% filter(periodo == 2015)


p15 <- ggplot(df_2015, aes(x = sexo, y = tasa_paro )) + geom_col(fill = "lightcoral") + coord_flip() + labs(title = "TASA DE PARO 2015 ", subtitle = "en porcentajes") 

p15


#2020
df <- df %>% filter(sexo != "Ambos sexos")
df_2020 <- df %>% filter(edad == "Total")
df_2020 <- df_2020 %>% filter(periodo == 2020)

p20 <- ggplot(df_2020, aes(x = sexo, y = tasa_paro )) + geom_col(fill = "lightcoral") + coord_flip() + labs(title = "TASA DE PARO 2020 ", subtitle = "en porcentajes")

p20


#otra forma de visualizar los datos
my_url <- "https://www.ine.es/jaxiT3/files/t/es/csv_bdsc/4887.csv?nocab=1"
curl::curl_download(my_url, "./datos/epa_genero-edad.csv")
df <- rio::import("./datos/epa_genero-edad.csv")
df <- janitor::clean_names(df) 

df <- df %>% mutate(total = stringr::str_replace(total, "," , "." ))
df <- df %>% mutate(total = as.numeric(total))
df <- df %>% rename(tasa_paro = total)
df <- df %>% mutate(edad.f = as_factor(edad), .after = edad)
df <- df %>%  mutate(edad.f = forcats::fct_relevel(edad.f, "Total", after = Inf))


df_orig <- df   
df <- df_orig %>% filter(sexo != "Ambos sexos") 
df_tot <- df %>% filter(edad == "Total") %>% 
  select(-edad)


df_tot_wide <- df_tot %>% 
  pivot_wider(names_from = sexo, values_from = tasa_paro) %>% 
  mutate(dif = Mujeres - Hombres) %>%
  select(periodo , Hombres , Mujeres , dif)

tab <- df_tot_wide %>% gt()
tab <- gt::gt(df_tot_wide)
tab <- tab %>% 
  opt_row_striping() %>% 
  tab_options(column_labels.border.bottom.color = "purple",
              table_body.border.bottom.color = "purple",
              table_body.hlines.color = "pink")

tab %>% gtExtras::gt_theme_nytimes() 
