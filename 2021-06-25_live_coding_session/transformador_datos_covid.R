# Librerias requeridas --------------

library(tidyverse)

library(janitor)

library(lubridate)

library(rmarkdown)



# Importar datos ---------------------------



col_requeridas <- c("FECHA","positivos","nue_posi","muj_posi","hom_posi","extranj_posi","costar_posi","adul_posi","am_posi","menor_posi","eda_ignor_posi",
                    
                    "fallecidos","nue_falleci","muj_fall","hom_fall", "adul_fall","am_fall","menor_fall",
                    
                    "RECUPERADOS","NUE_RECUP","MUJ_RECUP","HOM_RECUP","ADUL_RECUP","AM_RECUP","MENOR_RECUP","EDA_IGNO_RECUP")



datos_en_crudo <- read_csv2(file = "https://geovision.uned.ac.cr/oges/archivos_covid/2021_06_08/06_08_21_CSV_GENERAL.csv") %>%
  
  select(col_requeridas) %>%
  
  clean_names() %>%
  
  mutate(fecha = dmy(fecha)) %>%
  
  rename(nue_fall = nue_falleci) %>%
  
  rename(eda_ignor_recup = eda_igno_recup)



# Automatizacion fecha -------------



dia <- today() - days(1)



url <- str_c("https://geovision.uned.ac.cr/oges/archivos_covid/",
             
             str_replace_all(as.character(dia), "-", "_"),
             
             "/",
             
             format(dia, "%m_%d_%y"),
             
             "_CSV_GENERAL.csv")



datos_en_crudo <- read_csv(file = url) %>%
  
  select(col_requeridas) %>%
  
  clean_names() %>%
  
  mutate(fecha = dmy(fecha)) %>%
  
  rename(nue_fall = nue_falleci) %>%
  
  rename(eda_ignor_recup = eda_igno_recup)



# Transformación datos de recuperados, fallecidos y positivos -------------



datos_procesados_posi <- datos_en_crudo %>%
  
  select("fecha", contains("posi")) %>%
  
  rename(total = positivos) %>%
  
  pivot_longer(cols = -fecha,
               
               names_to = "clasificacion",
               
               values_to = "positivos") #%>%
  
  mutate(clasificacion = str_replace(clasificacion, "_posi", ""))





transformaciones <- function(datos,
                             
                             grupo = c("positivos", "fallecidos", "recuperados", "salon", "uci")) {
  
  abrev <- ifelse(grupo == "positivos",
                  
                  "posi",
                  
                  ifelse(grupo == "fallecidos",
                         
                         "fall",
                         
                         ifelse(grupo == "recuperados",
                                
                                "recup",
                                
                                ifelse(grupo == "salon",
                                       
                                       "salon",
                                       
                                       "uci"
                                       
                                )
                                
                         )
                         
                  )
                  
  )
  
  
  
  abrev2 <- str_c("_", abrev)
  
  
  
  x <- datos %>%
    
    select("fecha", contains(abrev)) %>%
    
    rename(total = grupo) %>%
    
    pivot_longer(cols = -fecha,
                 
                 names_to = "clasificacion",
                 
                 values_to = grupo) %>%
    
    mutate(clasificacion = str_replace(clasificacion, abrev2, ""))
}



adv <- "posi"

grupo <- "positivos"



datos_preparados_posi <- datos_en_crudo %>%
  
  select(fecha, contains(adv)) %>%
  
  rename(total = grupo) %>%
  
  pivot_longer(cols = -fecha,
               
               names_to = "clasificacion",
               
               values_to = grupo)





datos_preparados_posi <- transformaciones(datos = datos_en_crudo, grupo = "positivos")



datos_preparados_fall <- transformaciones(datos = datos_en_crudo, grupo = "fallecidos")



datos_preparados_recup <- transformaciones(datos = datos_en_crudo, grupo = "recuperados")





# Consolidación de datos en una sola tabla



data_final <- datos_preparados_posi %>%
  
  full_join(datos_preparados_fall, c("fecha", "clasificacion")) %>%
  
  full_join(datos_preparados_recup, c("fecha", "clasificacion")) #%>%
  
  pivot_longer(cols = c(positivos, recuperados, fallecidos),
               
               names_to = "poblacion",
               
               values_to = "cantidad") %>%
  
  pivot_wider(
    
    names_from = clasificacion,
    
    values_from = cantidad
    
  ) %>%
  
  mutate(muj = muj/total * 100,
         
         hom = hom/total * 100,
         
         extranj = extranj/total * 100,
         
         costar = costar/total * 100,
         
         adul = adul/total * 100,
         
         am = am/total * 100,
         
         menor = menor/total * 100,
         
         eda_ignor = eda_ignor/total * 100
         
  )



saveRDS(data_final, file = "data_final.RDS")





# Correr el R Markdown ---------------------------



render(input = "generador_reporte_covid.Rmd")